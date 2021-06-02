#' Checking if needed package is installed

#' @param package A string naming the package, whose installation needs to be
#'   checked in any of the libraries
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical that decides whether the function should stop if the
#'   needed package is not installed.
#' @param minimum_version String, representing the minimum package version that
#'   is required. If \code{NULL}, no check for minimum version is done.
#' @param ... Currently ignored
#'
#' @examples
#' \dontrun{
#' check_if_installed("inexistent_package")
#' check_if_installed("insight")
#' check_if_installed("insight", minimum_version = "99.8.7")
#' }
#' @export
check_if_installed <- function(package,
                               reason = "for this function to work",
                               stop = TRUE,
                               minimum_version = NULL,
                               ...) {
  # does it need to be displayed?
  is_installed <- requireNamespace(package, quietly = TRUE)
  if (!is_installed) {
    # prepare the message
    message <- format_message(
      paste0("Package '", package, "' is required ", reason, "."),
      paste0("Please install it by running install.packages('", package, "').")
    )

    if (stop) stop(message, call. = FALSE) else warning(message, call. = FALSE)
  } else if (!is.null(minimum_version) && utils::packageVersion(package) < package_version(minimum_version)) {
    # prepare the message
    message <- format_message(
      paste0("Package '", package, "' is installed, but package version '", minimum_version,"' is required ", reason, "."),
      paste0("Please update the package by running install.packages('", package, "').")
    )

    if (stop) stop(message, call. = FALSE) else warning(message, call. = FALSE)
  }

  invisible(is_installed)
}
