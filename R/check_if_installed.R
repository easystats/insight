#' Checking if needed package is installed

#' @param package A string naming the package, whose installation needs to be
#'   checked in any of the libraries
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical that decides whether the function should stop if the
#'   needed package is not installed.
#' @param ... Currently ignored
#'
#' @export

check_if_installed <- function(package,
                               reason = "for this function to work",
                               stop = TRUE,
                               ...) {
  # does it need to be displayed?
  if (!requireNamespace(package, quietly = TRUE)) {
    # prepare the message
    message <- cat(
      "Package '", package, "' is required ", reason, ".\n",
      "Please install it by running install.packages('", package, "').",
      sep = ""
    )

    if (stop) stop(message(message), call. = FALSE) else message(message)
  }
}
