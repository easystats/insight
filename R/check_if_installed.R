#' Checking if needed package is installed

#' @param package A character vector naming the package(s), whose installation
#'   needs to be checked in any of the libraries.
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical that decides whether the function should stop if the
#'   needed package is not installed.
#' @param quietly Logical, if `TRUE`, invisibly returns either `TRUE` if all
#'   packages are installed, `FALSE` otherwise, and does not stop or throw a
#'   warning. If `quietly = TRUE`, argument `stop` is ignored. Use this argument
#'   to internally check for package dependencies without stopping or warnings.
#' @param prompt If `TRUE`, will prompt the user to install needed package(s).
#'   Ignored if `quietly = TRUE`.
#' @param minimum_version A character vector, representing the minimum package
#'   version that is required for each package. Should be of same length as
#'   `package`. If `NULL`, no check for minimum version is done.
#' @param ... Currently ignored
#'
#' @return If `stop = TRUE`, and `package` is not yet installed, the
#'   function stops and throws an error. Else, a named logical vector is
#'   returned, indicating which of the packages are installed, and which not.
#'
#' @examples
#' \dontrun{
#' check_if_installed("inexistent_package")
#' check_if_installed("insight")
#' check_if_installed("insight", minimum_version = "99.8.7")
#'
#' x <- check_if_installed(c("inexistent", "also_not_here"), stop = FALSE)
#' x
#' }
#' @export
check_if_installed <- function(package,
                               reason = "for this function to work",
                               stop = TRUE,
                               minimum_version = NULL,
                               quietly = FALSE,
                               prompt = interactive(),
                               ...) {

  is_installed <- sapply(package, requireNamespace, quietly = TRUE)
  what_is_wrong <- what_you_can_do <- NULL

  ## Test
  if (!all(is_installed)) {
    # only keep not-installed packages
    package <- package[!is_installed]

    what_is_wrong <- sprintf("Package%s %s required %s.",
                             if (length(package) > 1L) "s" else "",
                             paste(sprintf("'%s'", package), collapse = " and "),
                             reason)

    what_you_can_do <- sprintf("Please install %s by running install.packages(%s).",
                               if (length(package) > 1L) "them" else "it",
                               paste(sprintf("\"%s\"", package), collapse = ", "))

  } else if (!is.null(minimum_version)) {
    needs_update <- utils::packageVersion(package) < package_version(minimum_version)

    if (any(needs_update)) {
      # only keep not-up-to-date packages
      package <- package[needs_update]
      minimum_version <- minimum_version[needs_update]

      what_is_wrong <- sprintf("Package%s %s %s installed, but package version%s %s %s required.",
                               if (length(package) > 1L) "s" else "",
                               paste(sprintf("'%s'", package), collapse = " and "),
                               if (length(package) > 1L) "are" else "is",
                               if (length(package) > 1L) "s" else "",
                               paste(sprintf("'%s'", minimum_version), collapse = " and "),
                               if (length(package) > 1L) "are" else "is")

      what_you_can_do <- sprintf("Please update %s by running install.packages(%s).",
                                 if (length(package) > 1L) "them" else "it",
                                 paste(sprintf("\"%s\"", package), collapse = ", "))
    }
  }

  class(is_installed) <- c("check_if_installed", class(is_installed))

  ## What do?
  if (!is.null(what_is_wrong) && !quietly) {
    if (prompt) {
      what_you_can_do <- sprintf("Would you like to %s %s? [y/n]",
                                 if (grepl("update", what_you_can_do)) "update" else "install",
                                 if (grepl("them", what_you_can_do)) "them" else "it")

      print_color(format_message(what_is_wrong), "red")
      should_install <- readline(format_message(what_you_can_do))

      if (tolower(should_install) == "y") {
        utils::install.packages(package, ...)
        return(invisible(is_installed))
      }
    } else {
      message <- format_message(what_is_wrong, what_you_can_do)
      if (stop) stop(message, call. = FALSE) else warning(message, call. = FALSE)
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

  if (any(!x)) {
    if (any(x)) {
      cat("\n\n")
    }
    cat("Following packages are not installed:\n")
    print_color(paste0("- ", names(x)[!x], collapse = "\n"), "red")
  }
}
