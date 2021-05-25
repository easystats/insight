#' Reshape (pivot) data from wide to long
#'
#' This function "lengthens" data, increasing the number of rows and decreasing the number of columns. This is a dependency-free base-R equivalent of \code{tidyr::pivot_longer()}.
#'
#' @param data A data frame to pivot.
#' @param cols A vector of column names or indices to pivot into longer format.
#' @param colnames_to The name of the new column that will contain the column names.
#' @param values_to The name of the new column that will contain the values of the pivoted variables.
#' @param rows_to The name of the column that will contain the row-number from the original data. If \code{NULL}, will be removed.
#' @param colnames_from The name of the column that contains the levels to be used as future columns
#' @param values_from The name of the column that contains the values of the put in the columns.
#' @param rows_from The name of the column that identifies the rows. If \code{NULL}, will use all the unique rows.
#' @param ... Additional arguments passed on to methods.
#' @param names_to,names_from Same as \code{colnames_to}, is there for compatibility with \code{tidyr::pivot_longer()}.
#' @param sep The indicating a separating character in the variable names in the wide format.
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
#'              cols = c(1, 2),
#'              colnames_to = "Column",
#'              values_to = "Numbers",
#'              rows_to = "Row")
#'
#' # From long to wide
#' # -----------------
#' long_data <- data_to_long(wide_data, rows_to = "Row_ID")  # Save row number
#' data_to_wide(long_data,
#'              colnames_from = "Name",
#'              values_from = "Value",
#'              rows_from = "Row_ID")
#'
#' # Full example
#' # ------------------
#' if (require("psych")) {
#'   data <- psych::bfi  # Wide format with one row per participant's personality test
#'
#'   # Pivot long format
#'   long <- data_to_long(data,
#'                        cols = "\\d",  # Select all columns that contain a digit
#'                        colnames_to = "Item",
#'                        values_to = "Score",
#'                        rows_to = "Participant")
#'
#'   # Separate facet and question number
#'   long$Facet <- gsub("\\d", "", long$Item)
#'   long$Item <- gsub("[A-Z]", "", long$Item)
#'   long$Item <- paste0("I", long$Item)
#'
#'   wide <- data_to_wide(long,
#'                        colnames_from = "Item",
#'                        values_from = "Score")
#'    head(wide)
#' }
#' @return data.frame
#' @export
data_to_long <- function(data, cols = "all", colnames_to = "Name", values_to = "Value", rows_to = NULL, ..., names_to = colnames_to) {

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
data_to_wide <- function(data, values_from = "Value", colnames_from = "Name", rows_from = NULL, sep = "_", ..., names_from = colnames_from) {

  # Compatibility with tidyr
  if (names_from != colnames_from) colnames_from <- names_from

  # save attribute of each variable
  variable_attr <- lapply(data, attributes)

  # If no other row identifier, create one
  if (is.null(rows_from)) {
    if (all(names(data) %in% c(values_from, colnames_from))) {
      data[["_Rows"]] <- row.names(data)
    }
    data[["_Rows"]] <- apply(data[  , !names(data) %in% c(values_from, colnames_from), drop = FALSE ] , 1 , paste , collapse = "_")
    rows_from <- "_Rows"
  }

  # Reshape
  wide <- stats::reshape(data,
                         v.names = values_from,
                         idvar = rows_from,
                         timevar = colnames_from,
                         sep = sep,
                         direction = "wide")

  # Clean
  if ("_Rows" %in% names(wide)) wide[["_Rows"]] <- NULL
  row.names(wide) <- NULL  # Reset row names

  # add back attributes where possible
  for (i in colnames(wide)) {
    attributes(wide[[i]]) <- variable_attr[[i]]
  }

  wide
}




# Aliases -----------------------------------------------------------------

#' @rdname data_to_long
#' @export
reshape_longer <- data_to_long

#' @rdname data_to_long
#' @export
reshape_wider <- data_to_wide
