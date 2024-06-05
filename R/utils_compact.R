#' Remove empty elements from lists
#'
#' @param x A list or vector.
#' @param remove_na Logical to decide if `NA`s should be removed.
#'
#' @examples
#' compact_list(list(NULL, 1, c(NA, NA)))
#' compact_list(c(1, NA, NA))
#' compact_list(c(1, NA, NA), remove_na = TRUE)
#' @export
compact_list <- function(x, remove_na = FALSE) {
  # to make regular(!) R code work with vctrs, we need to remove the vctrs-class
  x <- .clean_from_vctrs(x)
  # finally, we can compact the list...
  if (remove_na) {
    x[!sapply(x, function(i) !is_model(i) && !inherits(i, c("Formula", "gFormula")) && (length(i) == 0L || is.null(i) || (length(i) == 1L && is.na(i)) || all(is.na(i)) || any(i == "NULL", na.rm = TRUE)))]
  } else {
    x[!sapply(x, function(i) !is_model(i) && !inherits(i, c("Formula", "gFormula")) && (length(i) == 0L || is.null(i) || any(i == "NULL", na.rm = TRUE)))]
  }
}

#' Remove empty strings from character
#'
#' @param x A single character or a vector of characters.
#'
#' @return
#'
#' A character or a character vector with empty strings removed.
#'
#' @examples
#' compact_character(c("x", "y", NA))
#' compact_character(c("x", "NULL", "", "y"))
#'
#' @export
compact_character <- function(x) {
  x[!sapply(x, function(i) !nzchar(i, keepNA = TRUE) || all(is.na(i)) || any(i == "NULL", na.rm = TRUE))]
}


# helper ----------------------------------------------------------------------

.clean_from_vctrs <- function(x) {
  # iterate over elements of the list. we need to check
  # whether we have data frames
  if (!is.null(x) && length(x) && is.list(x)) {
    # data frame is also a "list" - sanity check here
    # and remove attributes directly
    if (is.data.frame(x)) {
      x <- .remove_vctrs_class(x)
    } else {
      x <- lapply(x, function(element) {
        # for data frames, check if columns are vctrs.
        # if so, remove the vctrs-class attributes
        if (!is.null(element) && length(element) && is.data.frame(element)) {
          element <- .remove_vctrs_class(element)
        }
        element
      })
    }
  }
  x
}


.remove_vctrs_class <- function(x) {
  x[] <- lapply(x, function(i) {
    # remove vctr-class attributes in data frames
    class(i) <- setdiff(class(i), c("haven_labelled", "vctrs_vctr"))
    i
  })
  x
}
