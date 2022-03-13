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
  if (remove_na) {
    x[!sapply(x, function(i) !inherits(i, c("Formula", "gFormula")) && (length(i) == 0L || is.null(i) || (length(i) == 1L & is.na(i)) || any(i == "NULL", na.rm = TRUE)))]
  } else {
    x[!sapply(x, function(i) !inherits(i, c("Formula", "gFormula")) && (length(i) == 0L || is.null(i) || any(i == "NULL", na.rm = TRUE)))]
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
  x[!sapply(x, function(i) nchar(i) == 0 || all(is.na(i)) || any(i == "NULL", na.rm = TRUE))]
}
