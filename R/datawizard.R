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





#' Reshape (pivot) data from wide to long
#'
#' This function "lengthens" data, increasing the number of rows and decreasing
#' the number of columns. This is a dependency-free base-R equivalent of
#' `tidyr::pivot_longer()`.
#'
#' @param data A data frame to pivot.
#' @param cols A vector of column names or indices to pivot into longer format.
#' @param colnames_to The name of the new column that will contain the column
#'   names.
#' @param values_to The name of the new column that will contain the values of
#'   the pivoted variables.
#' @param rows_to The name of the column that will contain the row-number from
#'   the original data. If `NULL`, will be removed.
#' @param colnames_from The name of the column that contains the levels to be
#'   used as future columns.
#' @param values_from The name of the column that contains the values of the put
#'   in the columns.
#' @param rows_from The name of the column that identifies the rows. If
#'   `NULL`, will use all the unique rows.
#' @param ... Additional arguments passed on to methods.
#' @param names_to,names_from Same as `colnames_to`, is there for
#'   compatibility with `tidyr::pivot_longer()`.
#' @param sep The indicating a separating character in the variable names in the
#'   wide format.
#'
#'
#' @examples
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#'
#' # From wide to long
#' # ------------------
#' # Default behaviour (equivalent to tidyr::pivot_longer(wide_data, cols = 1:5))
#' data_to_long(wide_data)
#'
#' # Customizing the names
#' data_to_long(wide_data,
#'   cols = c(1, 2),
#'   colnames_to = "Column",
#'   values_to = "Numbers",
#'   rows_to = "Row"
#' )
#'
#' # From long to wide
#' # -----------------
#' long_data <- data_to_long(wide_data, rows_to = "Row_ID") # Save row number
#' data_to_wide(long_data,
#'   colnames_from = "Name",
#'   values_from = "Value",
#'   rows_from = "Row_ID"
#' )
#'
#' # Full example
#' # ------------------
#' if (require("psych")) {
#'   data <- psych::bfi # Wide format with one row per participant's personality test
#'
#'   # Pivot long format
#'   long <- data_to_long(data,
#'     cols = "\\d", # Select all columns that contain a digit
#'     colnames_to = "Item",
#'     values_to = "Score",
#'     rows_to = "Participant"
#'   )
#'
#'   # Separate facet and question number
#'   long$Facet <- gsub("\\d", "", long$Item)
#'   long$Item <- gsub("[A-Z]", "", long$Item)
#'   long$Item <- paste0("I", long$Item)
#'
#'   wide <- data_to_wide(long,
#'     colnames_from = "Item",
#'     values_from = "Score"
#'   )
#'   head(wide)
#' }
#' @return data.frame
#' @export
data_to_long <- function(data,
                         cols = "all",
                         colnames_to = "Name",
                         values_to = "Value",
                         rows_to = NULL,
                         ...,
                         names_to = colnames_to) {
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data)
  } else {
    tbl_input <- FALSE
  }

  # Select columns ----------------
  if (is.character(cols) && length(cols) == 1) {
    # If only one name

    if (cols == "all") {
      # If all, take all
      cols <- names(data)
    } else {
      # Surely, a regex
      cols <- grep(cols, names(data), value = TRUE)
    }
  }

  # If numeric, surely the index of the cols
  if (is.numeric(cols)) {
    cols <- names(data)[cols]
  }


  # Sanity checks ----------------
  # Make sure all cols are in data
  if (!all(cols %in% names(data))) {
    stop("Some variables as selected by 'cols' are not present in the data.")
  }

  # Compatibility with tidyr
  if (names_to != colnames_to) colnames_to <- names_to

  # save attribute of each variable
  variable_attr <- lapply(data, attributes)

  # Reshaping ---------------------
  # Create Index column as needed by reshape
  data[["_Row"]] <- to_numeric(row.names(data))

  # Reshape
  long <- stats::reshape(data,
                         varying = cols,
                         idvar = "_Row",
                         v.names = values_to,
                         timevar = colnames_to,
                         direction = "long"
  )

  # Cleaning --------------------------
  # Sort the dataframe (to match pivot_longer's output)
  long <- long[order(long[["_Row"]], long[[colnames_to]]), ]

  # Remove or rename the row index
  if (is.null(rows_to)) {
    long[["_Row"]] <- NULL
  } else {
    names(long)[names(long) == "_Row"] <- rows_to
  }

  # Re-insert col names as levels
  long[[colnames_to]] <- cols[long[[colnames_to]]]

  # Reset row names
  row.names(long) <- NULL

  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL

  # add back attributes where possible
  for (i in colnames(long)) {
    attributes(long[[i]]) <- variable_attr[[i]]
  }

  if (isTRUE(tbl_input)) {
    class(long) <- c("tbl_df", "tbl", "data.frame")
  }

  long
}



#' @rdname data_to_long
#' @export
data_to_wide <- function(data,
                         values_from = "Value",
                         colnames_from = "Name",
                         rows_from = NULL,
                         sep = "_",
                         ...,
                         names_from = colnames_from) {
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data)
  } else {
    tbl_input <- FALSE
  }

  # Compatibility with tidyr
  if (names_from != colnames_from) colnames_from <- names_from

  # save attribute of each variable
  variable_attr <- lapply(data, attributes)

  # If no other row identifier, create one
  if (is.null(rows_from)) {
    if (all(names(data) %in% c(values_from, colnames_from))) {
      data[["_Rows"]] <- row.names(data)
    }
    data[["_Rows"]] <- apply(data[, !names(data) %in% c(values_from, colnames_from), drop = FALSE], 1, paste, collapse = "_")
    rows_from <- "_Rows"
  }

  # Reshape
  wide <- stats::reshape(data,
                         v.names = values_from,
                         idvar = rows_from,
                         timevar = colnames_from,
                         sep = sep,
                         direction = "wide"
  )

  # Clean
  if ("_Rows" %in% names(wide)) wide[["_Rows"]] <- NULL
  row.names(wide) <- NULL # Reset row names

  # Remove reshape attributes
  attributes(wide)$reshapeWide <- NULL

  # add back attributes where possible
  for (i in colnames(wide)) {
    attributes(wide[[i]]) <- variable_attr[[i]]
  }

  if (isTRUE(tbl_input)) {
    class(wide) <- c("tbl_df", "tbl", "data.frame")
  }

  wide
}
