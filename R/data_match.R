#' Find rows of a dataframe that are matching a specific subset
#'
#' Find row indices of a dataframe that are matching a specific configuration.
#'
#' @param x A dataframe.
#' @param to The dataframe of which to meet the characteristics.
#'
#' @examples
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
#' mtcars[matching_rows, ]
#'
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#' mtcars[matching_rows, ]
#'
#' @export
data_match <- function(x, to) {

  # Sanity checks
  if(!is.data.frame(to)) to <- as.data.frame(to)

  # Find matching rows
  idx <- 1:nrow(x)
  for(col in names(to)) {
    if(col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }
  as.numeric_ifnumeric(row.names(x)[idx])
}