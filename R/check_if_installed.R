#' Checking if needed package is installed

#' @param package A character vector naming the package(s), whose installation
#'   needs to be checked in any of the libraries.
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical controlling whether a failed check stops or warns when
#'   no installation or update prompt is shown. Ignored if `quietly = TRUE`.
#' @param quietly Logical, if `TRUE`, returns the result invisibly without
#'   reporting failed checks. Arguments `stop` and `prompt` are ignored.
#' @param prompt If `TRUE`, will prompt the user to install needed package(s).
#'   Ignored if `quietly = TRUE`. By default, `prompt` is set to `interactive()`.
#' @param minimum_version A character vector, representing the minimum package
#'   version that is required for each package. Should be of same length as
#'   `package`. If `NULL`, minimum version requirements are ignored. If using
#'   `minimum_version` with more than one package, `NA` should be used for
#'   packages where a specific version is not necessary.
#' @param ... Further arguments passed down to `utils::install.packages()`.
#'
#' @return A named logical vector indicating which packages passed the check.
#'
#' @details Installed packages that fail to load are reported with the original
#'   load error. Installation prompts apply to missing packages and packages
#'   below the requested minimum version.
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
check_if_installed <- function(
  package,
  reason = "for this function to work",
  stop = TRUE,
  minimum_version = NULL,
  quietly = FALSE,
  prompt = interactive(),
  ...
) {
  load_errors <- vapply(
    package,
    function(pkg) {
      tryCatch(
        {
          loadNamespace(pkg)
          NA_character_
        },
        error = conditionMessage
      )
    },
    FUN.VALUE = NA_character_
  )
  is_installed <- is.na(load_errors)
  what_is_wrong <- what_you_can_do <- NULL

  # validation check for equal length of package and minimum_version
  if (!is.null(minimum_version) && length(package) != length(minimum_version)) {
    minimum_version <- NULL
  }

  ## Test
  if (!all(is_installed)) {
    if (!quietly) {
      diagnostics <- .package_load_diagnostics(
        package[!is_installed],
        load_errors[!is_installed],
        reason
      )
      package <- diagnostics$missing_packages
      what_is_wrong <- diagnostics$problems
      what_you_can_do <- diagnostics$advice
    }
  } else if (!is.null(minimum_version)) {
    needs_update <- unlist(Map(
      function(p, m) {
        if (is.na(m)) {
          FALSE
        } else {
          utils::packageVersion(p) < package_version(m)
        }
      },
      package,
      minimum_version
    ))

    if (any(needs_update)) {
      # set is_installed to FALSE for packages that fail minimum version check
      is_installed[needs_update] <- FALSE

      # only keep not-up-to-date packages
      package <- package[needs_update]
      minimum_version <- minimum_version[needs_update]
      plural <- length(package) > 1L
      suffix <- if (plural) "s" else ""
      verb <- if (plural) "are" else "is"

      what_is_wrong <- sprintf(
        "Package%s %s %s installed, but package version%s %s %s required.",
        suffix,
        paste(sprintf("`%s`", package), collapse = " and "),
        verb,
        suffix,
        paste(sprintf("`%s`", minimum_version), collapse = " and "),
        verb
      )

      what_you_can_do <- sprintf(
        "Please update %s by running `install.packages(%s)`.",
        if (plural) "them" else "it",
        toString(sprintf("\"%s\"", package))
      )
    }
  }

  class(is_installed) <- c("check_if_installed", class(is_installed))

  ## What do?
  if (length(what_is_wrong) && !quietly) {
    if (prompt && !is.null(what_you_can_do)) {
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


.package_load_diagnostics <- function(package, load_errors, reason) {
  present <- lengths(lapply(package, find.package, quiet = TRUE)) > 0L
  problems <- sprintf(
    "Could not load package `%s`: %s",
    package[present],
    trimws(load_errors[present], which = "right")
  )
  missing_packages <- package[!present]
  advice <- NULL

  if (length(missing_packages)) {
    problems <- c(
      problems,
      sprintf(
        "Package%s %s required %s.",
        if (length(missing_packages) > 1L) "s" else "",
        paste(sprintf("`%s`", missing_packages), collapse = " and "),
        reason
      )
    )
    advice <- sprintf(
      "Please install %s by running `install.packages(%s)`.",
      if (length(missing_packages) > 1L) "them" else "it",
      toString(sprintf("\"%s\"", missing_packages))
    )
  }

  list(missing_packages = missing_packages, problems = problems, advice = advice)
}


#' @export
print.check_if_installed <- function(x, ...) {
  if (any(x)) {
    cat("Packages that passed the check:\n")
    print_color(paste0("- ", names(x)[x], collapse = "\n"), "green")
  }

  if (!all(x)) {
    if (any(x)) {
      cat("\n\n")
    }
    cat("Packages that failed the check:\n")
    print_color(paste0("- ", names(x)[!x], collapse = "\n"), "red")
  }
}
