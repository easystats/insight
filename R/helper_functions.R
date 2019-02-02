# remove trailing/leading spaces from character vectors
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# remove NULL elements from lists
compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i))]

# is string empty?
is_empty_string <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0 || all(nchar(x) == 0)
}

# is string empty?
is_empty_object <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0 || is.null(x)
}

# does string contain pattern?
string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grepl(pattern, x, perl = TRUE)
}

# has object an element with given name?
obj_has_name <- function(x, name) {
  name %in% names(x)
}

# merge data frames, remove double columns
merge_dataframes <- function(data, ..., replace = TRUE) {
  # check for identical column names
  tmp <- cbind(...)

  if (nrow(data) == 0) return(tmp)

  doubles <- colnames(tmp) %in% colnames(data)

  # keep order?
  reihenfolge <- c(which(!doubles), which(doubles))

  # remove duplicate column names, if requested
  if (replace && any(doubles)) tmp <- tmp[, !doubles, drop = FALSE]

  # bind all data
  x <- cbind(tmp, data)

  # restore order
  if (replace) {
    # check for correct length. if "data" had duplicated variables,
    # but not all variable are duplicates, add indices of regular values
    if (ncol(x) > length(reihenfolge)) {
      # get remaining indices
      xl <- seq_len(ncol(x))[-seq_len(length(reihenfolge))]
      # add to "reihefolge"
      reihenfolge <- c(reihenfolge, xl)
    }
    # sort data frame
    x <- x[, order(reihenfolge)]
  }

  x
}


# check if zero-inf random effects are requested
# although model has no zi-component
is_invalid_zeroinf <- function(dots) {
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(TRUE)
  }
  FALSE
}
