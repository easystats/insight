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
  is_remove <- vapply(
    x,
    function(i) {
      if (is_model(i) || inherits(i, c("Formula", "gFormula")) || is.function(i)) {
        return(FALSE)
      }
      if (remove_na) {
        if (is.atomic(i) && all(is.na(i))) {
          return(TRUE)
        } else if (.safe(all(is.na(i)), FALSE)) {
          return(TRUE)
        }
      } else if (length(i) == 0L || is.null(i)) {
        return(TRUE)
      }
      .is_null_string(i)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
  x[!is_remove]
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
  x[
    !sapply(x, function(i) {
      !nzchar(i, keepNA = TRUE) ||
        all(is.na(i)) ||
        any(as.character(i) == "NULL", na.rm = TRUE)
    })
  ]
}

# helper -----------------

.is_null_string <- function(object) {
  if (is.character(object) || is.factor(object)) {
    return(any(object == "NULL", na.rm = TRUE))
  }
  if (is.atomic(object)) {
    return(FALSE)
  }
  if (is.list(object)) {
    # recursion on nested lists
    return(any(rapply(object, .is_null_string, how = "unlist")))
  }
  .safe(any(as.character(object) == "NULL", na.rm = TRUE), FALSE)
}
