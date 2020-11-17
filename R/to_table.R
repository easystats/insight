#' Generic export of data frames into tables
#'
#' @param x A data frame.
#' @param ... Arguments passed to other methods.
#'
#' @return A data frame.
#'
#' @details This is a generic function to export data frames into various
#' table formats (like plain text, markdown, ...). See the documentation
#' for the specific objects' classes.
#'
#' @export
to_table <- function(x, ...) {
  UseMethod("to_table")
}
