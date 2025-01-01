#' Checking if needed package is installed

#' @param package A character vector naming the package(s), whose installation
#'   needs to be checked in any of the libraries.
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical that decides whether the function should stop if the
#'   needed package is not installed.
#' @param quietly Logical, if `TRUE`, invisibly returns a vector of logicals
#'   (`TRUE` for each installed package, `FALSE` otherwise), and does not stop
#'   or throw a warning. If `quietly = TRUE`, arguments `stop` and `prompt` are
#'   ignored. Use this argument to internally check for package dependencies
#'   without stopping or warnings.
#' @param prompt If `TRUE`, will prompt the user to install needed package(s).
#'   Ignored if `quietly = TRUE`.
#' @param minimum_version A character vector, representing the minimum package
#'   version that is required for each package. Should be of same length as
#'   `package`. If `NULL`, will automatically check the DESCRIPTION file for
#'   the correct minimum version. If using `minimum_version` with more than one
#'   package, `NA` should be used instead of `NULL` for packages where a
#'   specific version is not necessary.
#' @param ... Currently ignored
#'
#' @return If `stop = TRUE`, and `package` is not yet installed, the
#'   function stops and throws an error. Else, a named logical vector is
#'   returned, indicating which of the packages are installed, and which not.
#'
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' \donttest{
#' check_if_installed("insight")
#' try(check_if_installed("datawizard", stop = FALSE))
#' try(check_if_installed("rstanarm", stop = FALSE))
#' try(check_if_installed("nonexistent_package", stop = FALSE))
#' try(check_if_installed("insight", minimum_version = "99.8.7"))
#' try(check_if_installed(c("nonexistent", "also_not_here"), stop = FALSE))
#' try(check_if_installed(c("datawizard", "rstanarm"), stop = FALSE))
#' try(check_if_installed(c("datawizard", "rstanarm"),
#'   minimum_version = c(NA, "2.21.1"), stop = FALSE
#' ))
#' }
#' @export
check_if_installed <- function(package,
                               reason = "for this function to work",
                               stop = TRUE,
                               minimum_version = NULL,
                               quietly = FALSE,
                               prompt = interactive(),
                               ...) {
  is_installed <- vapply(package, requireNamespace, quietly = TRUE, FUN.VALUE = TRUE)
  what_is_wrong <- what_you_can_do <- NULL

  # This function is currently not reliable, as it reads the suggests field
  # from "insight", even when called from other functions. See when running
  # this example: https://github.com/easystats/bayestestR/issues/627

  # if (is.null(minimum_version)) {
  #   minimum_version <- .safe(.get_dep_version(dep = package))
  # }

  # validation check for equal length of package and minimum_version
  if (!is.null(minimum_version) && length(package) != length(minimum_version)) {
    minimum_version <- NULL
  }

  ## Test
  if (!all(is_installed)) {
    # only keep not-installed packages
    package <- package[!is_installed]

    what_is_wrong <- sprintf(
      "Package%s %s required %s.",
      if (length(package) > 1L) "s" else "",
      paste(sprintf("`%s`", package), collapse = " and "),
      reason
    )

    what_you_can_do <- sprintf(
      "Please install %s by running `install.packages(%s)`.",
      if (length(package) > 1L) "them" else "it",
      toString(sprintf("\"%s\"", package))
    )
  } else if (!is.null(minimum_version)) {
    needs_update <- unlist(Map(function(p, m) {
      if (is.na(m)) {
        FALSE
      } else {
        utils::packageVersion(p) < package_version(m)
      }
    }, package, minimum_version))

    if (any(needs_update)) {
      # set is_installed to FALSE for packages that fail minimum version check
      is_installed[needs_update] <- FALSE

      # only keep not-up-to-date packages
      package <- package[needs_update]
      minimum_version <- minimum_version[needs_update]

      what_is_wrong <- sprintf(
        "Package%s %s %s installed, but package version%s %s %s required.",
        if (length(package) > 1L) "s" else "",
        paste(sprintf("`%s`", package), collapse = " and "),
        if (length(package) > 1L) "are" else "is",
        if (length(package) > 1L) "s" else "",
        paste(sprintf("`%s`", minimum_version), collapse = " and "),
        if (length(package) > 1L) "are" else "is"
      )

      what_you_can_do <- sprintf(
        "Please update %s by running `install.packages(%s)`.",
        if (length(package) > 1L) "them" else "it",
        toString(sprintf("\"%s\"", package))
      )
    }
  }

  class(is_installed) <- c("check_if_installed", class(is_installed))

  ## What do?
  if (!is.null(what_is_wrong) && !quietly) {
    if (prompt) {
      what_you_can_do <- sprintf(
        "\nWould you like to %s %s? [y/n] ",
        if (grepl("update", what_you_can_do, fixed = TRUE)) "update" else "install",
        if (grepl("them", what_you_can_do, fixed = TRUE)) "them" else "it"
      )

      print_color(format_message(what_is_wrong), "red")
      should_install <- readline(format_message(what_you_can_do))

      if (tolower(should_install) == "y") {
        utils::install.packages(package, ...)
        return(invisible(is_installed))
      }
    } else {
      msg <- format_message(what_is_wrong, what_you_can_do)
      if (stop) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }
  }

  invisible(is_installed)
}


#' @export
print.check_if_installed <- function(x, ...) {
  if (any(x)) {
    cat("Following packages are installed:\n")
    print_color(paste0("- ", names(x)[x], collapse = "\n"), "green")
  }

  if (!all(x)) {
    if (any(x)) {
      cat("\n\n")
    }
    cat("Following packages are not installed:\n")
    print_color(paste0("- ", names(x)[!x], collapse = "\n"), "red")
  }
}

## BUG This function is currently not reliable, as it reads the suggests field
## from "insight", even when called from other functions. See when running
## this example: https://github.com/easystats/bayestestR/issues/627

.get_dep_version <- function(dep) {
  # iterate environments, to find the correct package
  minframe <- 1L
  n <- sys.nframe()
  pkg <- NULL
  while (n > minframe && is.null(pkg)) {
    pkg <- utils::packageName(env = sys.frame(n))
  }
  # if still NULL, or if "insight", return
  if (is.null(pkg) || pkg == "insight") {
    return(NULL)
  }
  suggests_field <- utils::packageDescription(pkg, fields = "Suggests")
  suggests_list <- unlist(strsplit(suggests_field, ",", fixed = TRUE))
  out <- lapply(dep, function(x) {
    dep_string <- grep(x, suggests_list, value = TRUE, fixed = TRUE)
    dep_string <- dep_string[which.min(nchar(dep_string))]
    dep_string <- unlist(strsplit(dep_string, ">", fixed = TRUE))
    gsub("[^0-9.]+", "", dep_string[2])
  })
  out <- unlist(compact_list(out))
  if (all(is.na(out)) || !length(out)) {
    out <- NULL
  }
  out
}
