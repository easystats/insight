#' Restore the type of columns according to a reference dataframe
#'
#' @inheritParams data_to_long
#' @param reference A reference dataframe from which to find the correct
#'   column types.
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
data_restoretype <- function(data, reference = NULL) {

  for(col in names(data)) {

    # No reference data (regular fixing) ----------------
    if(is.null(reference)) {
      if(is.character(data[[col]])) {
        data[[col]] <- as.numeric_ifnumeric(data[[col]])
      }
    } else {

      if(is.factor(reference[[col]]) && !is.factor(data[[col]])) {
        # Restore factor levels
        data[[col]] <- factor(data[[col]], levels = levels(reference[[col]]))
      }

      if (is.numeric(reference[[col]]) && !is.numeric(data[[col]])) {
        data[[col]] <- as.numeric_ifnumeric(as.character(data[[col]]))
      }

      if (is.character(reference[[col]]) && !is.character(data[[col]])) {
        data[[col]] <- as.character(data[[col]])
      }
    }
  }

  data
}











#' Convert to Numeric if Possible
#'
#' Tries to convert vector to numeric if possible (if no warnings or errors). Otherwise, leaves it as is.
#'
#' @param x A vector to be converted.
#'
#' @examples
#' as.numeric_ifnumeric(c("1", "2"))
#' as.numeric_ifnumeric(c("1", "2", "A"))
#' @return Numeric vector (if possible)
#' @export
as.numeric_ifnumeric <- function(x) {
  tryCatch(as.numeric(as.character(x)), error = function(e) x, warning = function(w) x)
}


