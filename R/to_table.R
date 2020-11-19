#' @title Generic export of data frames into tables
#' @name to_table
#'
#' @description This is a generic function to export data frames into various
#' table formats (like plain text, markdown, ...). See the documentation
#' for the specific objects' classes.
#'
#' @param x A data frame.
#' @param ... Arguments passed to other methods.
#'
#' @return A data frame.
#' @export
to_table <- function(x, ...) {
  UseMethod("to_table")
}


#' @rdname to_table
#' @export
table_to_markdown <- function(x, ...) {
  UseMethod("table_to_markdown")
}
