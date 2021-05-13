#' Relocate (reorder) columns of a dataframe
#'
#' @param data A data frame to pivot.
#' @param cols A character vector indicating the names of the columns to move.
#' @param before,after Destination of columns. Supplying neither will move columns to the left-hand side; specifying both is an error.
#' @param safe If \code{TRUE}, will disregard non-existing columns.
#'
#' @examples
#' # Reorder columns
#' data_relocate(iris, cols="Species", before = "Sepal.Length")
#' data_relocate(iris, cols="Species", before = "Sepal.Width")
#' data_relocate(iris, cols="Sepal.Width", after = "Species")
#' data_relocate(iris, cols=c("Species", "Petal.Length"), after = "Sepal.Width")
#'
#' @return data.frame
#' @export
data_relocate <- function(data, cols, before = NULL, after = NULL, safe = TRUE) {

  # Sanitize
  if (!is.null(before) && !is.null(after)) stop("You must supply only one of `before` and `after`.")

  if(safe) cols <- cols[cols %in% names(data)]

  # Move columns to the right hand side
  data <- data[c(setdiff(names(data), cols), cols)]

  # Get columns and their original position
  data_cols <- names(data)
  position <- which(data_cols %in% cols)

  # Find new positions
  if (!is.null(before)) {
    before <- before[before %in% data_cols][1]  # Take first that exists (if vector is supplied)
    if(length(before) != 1) stop("The column passed to 'before' wasn't found. Possibly mispelled.")
    where <- min(match(before, data_cols))
    position <- c(setdiff(position, where), where)
  } else if (!is.null(after)) {
    after <- after[after %in% data_cols][1]  # Take first that exists (if vector is supplied)
    if(length(after) != 1) stop("The column passed to 'after' wasn't found. Possibly mispelled.")
    where <- max(match(after, data_cols))
    position <- c(where, setdiff(position, where))
  } else {
    where <- 1
    position <- union(position, where)
  }

  # Set left and right side
  lhs <- setdiff(seq(1, where - 1), position)
  rhs <- setdiff(seq(where + 1, ncol(data)), position)
  position <- unique(c(lhs, position, rhs))
  position <- position[position <= length(data_cols)]

  data[position]
}
