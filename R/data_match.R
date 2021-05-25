#' Find rows of a data frame that are matching a specific subset
#'
#' Find row indices of a data frame that are matching a specific configuration.
#'
#' @param x A data frame.
#' @param to The data frame of which to meet the characteristics.
#'
#' @examples
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
#' mtcars[matching_rows, ]
#'
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#' mtcars[matching_rows, ]
#' @export
data_match <- function(x, to) {

  # Sanity checks
  if (!is.data.frame(to)) {
    to <- as.data.frame(to)
  }

  # Find matching rows
  idx <- 1:nrow(x)
  for (col in names(to)) {
    if (col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }
  to_numeric(row.names(x)[idx])
}
