#' Restore the type of columns according to a reference data frame
#'
#' @param data A data frame.
#' @param reference A reference data frame from which to find the correct
#'   column types.
#' @param ... Additional arguments passed on to methods.
#'
#' @return
#'
#' A dataframe with columns whose types have been restored based on the
#' reference dataframe.
#'
#' @examples
#' data <- data.frame(
#'   Sepal.Length = c("1", "3", "2"),
#'   Species = c("setosa", "versicolor", "setosa"),
#'   New = c("1", "3", "4")
#' )
#'
#' fixed <- data_restoretype(data, reference = iris)
#' summary(fixed)
#' @export
data_restoretype <- function(data, reference = NULL, ...) {
  for (col in names(data)) {

    # No reference data (regular fixing) ----------------
    if (is.null(reference)) {
      if (is.character(data[[col]])) {
        data[[col]] <- .to_numeric(data[[col]])
      }
    } else {
      if (is.factor(reference[[col]]) && !is.factor(data[[col]])) {
        # Restore factor levels
        data[[col]] <- factor(data[[col]], levels = levels(reference[[col]]))
      }

      if (is.numeric(reference[[col]]) && !is.numeric(data[[col]])) {
        data[[col]] <- .to_numeric(as.character(data[[col]]))
      }

      if (is.character(reference[[col]]) && !is.character(data[[col]])) {
        data[[col]] <- as.character(data[[col]])
      }
    }
  }

  data
}
