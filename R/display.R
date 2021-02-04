#' @title Generic export of data frames into formatted tables
#' @name display
#'
#' @description `display()` is a generic function to export data frames
#' into various table formats (like plain text, markdown, ...). `print_md()`
#' usually is a convenient wrapper for `display(format = "markdown")`.
#' Similar, `print_html()` is a shortcut for `display(format = "html")`.
#' See the documentation for the specific objects' classes.
#'
#' @param object,x A data frame.
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


#' @rdname display
#' @export
print_html <- function(x, ...) {
  UseMethod("print_html")
}
