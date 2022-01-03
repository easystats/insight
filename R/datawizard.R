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




#' Convert to Numeric (if possible)
#'
#' Tries to convert vector to numeric if possible (if no warnings or errors).
#' Otherwise, leaves it as is.
#'
#' @param x A vector to be converted.
#'
#' @examples
#' to_numeric(c("1", "2"))
#' to_numeric(c("1", "2", "A"))
#' @return Numeric vector (if possible)
#' @export
to_numeric <- function(x) {
  tryCatch(as.numeric(as.character(x)),
           error = function(e) x,
           warning = function(w) x
  )
}





#' Find row indices of a data frame matching a specific condition
#'
#' Find row indices of a data frame that match a specific condition.
#'
#' @param x A data frame.
#' @param to A data frame matching the specified conditions.
#' @param ... Other arguments passed to or from other functions.
#'
#' @return
#'
#' A dataframe containing rows that match the specified configuration.
#'
#' @examples
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
#' mtcars[matching_rows, ]
#'
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#' mtcars[matching_rows, ]
#' @export
data_match <- function(x, to, ...) {

  # Input checks
  if (!is.data.frame(to)) to <- as.data.frame(to)

  # Find matching rows
  idx <- 1:nrow(x)
  for (col in names(to)) {
    if (col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }

  to_numeric(row.names(x)[idx])
}





#' Relocate (reorder) columns of a data frame
#'
#' @param data A data frame to pivot.
#' @param cols A character vector indicating the names of the columns to move.
#' @param before,after Destination of columns. Supplying neither will move
#'   columns to the left-hand side; specifying both is an error.
#' @param safe If `TRUE`, will disregard non-existing columns.
#' @param ... Additional arguments passed on to methods.
#'
#' @examples
#' # Reorder columns
#' head(data_relocate(iris, cols = "Species", before = "Sepal.Length"))
#' head(data_relocate(iris, cols = "Species", before = "Sepal.Width"))
#' head(data_relocate(iris, cols = "Sepal.Width", after = "Species"))
#' head(data_relocate(iris, cols = c("Species", "Petal.Length"), after = "Sepal.Width"))
#' @return A data frame with reordered columns.
#'
#' @export
data_relocate <- function(data,
                          cols,
                          before = NULL,
                          after = NULL,
                          safe = TRUE,
                          ...) {

  # Sanitize
  if (!is.null(before) && !is.null(after)) {
    stop("You must supply only one of `before` or `after`.")
  }

  if (safe) {
    cols <- cols[cols %in% names(data)]
  }

  # save attributes
  att <- attributes(data)

  # Move columns to the right hand side
  data <- data[c(setdiff(names(data), cols), cols)]

  # Get columns and their original position
  data_cols <- names(data)
  position <- which(data_cols %in% cols)

  # Find new positions
  if (!is.null(before)) {
    before <- before[before %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(before) != 1) {
      stop("The column passed to 'before' wasn't found. Possibly mispelled.")
    }
    where <- min(match(before, data_cols))
    position <- c(setdiff(position, where), where)
  } else if (!is.null(after)) {
    after <- after[after %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(after) != 1) {
      stop("The column passed to 'after' wasn't found. Possibly mispelled.")
    }
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

  out <- data[position]
  attributes(out) <- utils::modifyList(att, attributes(out))

  out
}
