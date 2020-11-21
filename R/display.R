#' @title Generic export of data frames into formatted tables
#' @name display
#'
#' @description \code{display()} is a generic function to export data frames
#' into various table formats (like plain text, markdown, ...). \code{print_md()}
#' usually is a convenient wrapper for \code{display(format = "markdown")}. See
#' the documentation for the specific objects' classes.
#'
#' @param x A data frame.
#' @param ... Arguments passed to other methods.
#'
#' @return A data frame.
#' @export
display <- function(object, ...) {
  UseMethod("display")
}


#' @rdname display
#' @export
print_md <- function(x, ...) {
  UseMethod("print_md")
}
