#' Reshape (pivot) data from wide to long
#'
#' This function "lengthens" data, increasing the number of rows and decreasing the number of columns. This is a dependency-free base-R equivalent of \code{tidyr::pivot_longer()}.
#'
#' @param data A data frame to pivot.
#' @param cols A vector of column names or indices to pivot into longer format.
#' @param colnames_to The name of the new column that will contain the column names.
#' @param values_to The name of the new column that will contain the values of the pivotted variables.
#' @param rows_to The name of the column that will contain the row-number from the original data. If \code{NULL}, will be removed.
#' @param ... Additional arguments passed on to methods.
#' @param names_to Same as \code{colnames_to}, is there for compatibility with \code{tidyr::pivot_longer()}.
#'
#'
#' @examples
#' data <- data.frame(replicate(5, rnorm(10)))
#'
#' # Default behaviour (equivalent to tidyr::pivot_longer(data, cols = 1:5))
#' data_to_long(data)
#'
#' # Customizing the names
#' data_to_long(data,
#'              cols = c(1, 2),
#'              colnames_to = "Column",
#'              values_to = "Numbers",
#'              rows_to = "Row")
#'
#'
#' @export
data_to_long <- function(data, cols = "all", colnames_to = "Name", values_to = "Value", rows_to = NULL, ..., names_to = colnames_to) {

  # Sanity checks
  if(names_to != colnames_to) colnames_to <- names_to

  # Select columns
  if(is.character(cols) && length(cols) == 1 && cols == "all") {
    cols <- names(data)
  }

  if(is.numeric(cols)) {
    cols <- names(data)[cols]
  }

  # Create Index column as needed by reshape
  data[["_Row"]] <- 1:nrow(data)

  # Reshape
  long <- stats::reshape(data,
                         varying = cols,
                         idvar = "_Row",
                         v.names = values_to,
                         timevar = colnames_to,
                         direction = "long"
  )

  # Sort the dataframe (to match pivot_longer's output)
  long <- long[order(long[["_Row"]], long[[colnames_to]]), ]

  # Remove or rename the row index
  if(is.null(rows_to)) {
    long[["_Row"]] <- NULL
  } else {
    names(long)[names(long) == "_Row"] <- rows_to
  }

  # Clean
  long[[colnames_to]] <- cols[long[[colnames_to]]]  # Re-insert col names as levels
  row.names(long) <- NULL  # Reset row names

  long
}
