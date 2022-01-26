#' @title Capitalizes the first letter in a string
#' @name format_capitalize
#'
#' @description This function converts the first letter in a string into upper case.
#'
#' @param x A character vector or a factor. The latter is coerced to character.
#'   All other objects are returned unchanged.
#' @param verbose Toggle warnings.
#'
#' @return `x`, with first letter capitalized.
#'
#' @examples
#' format_capitalize("hello")
#' format_capitalize(c("hello", "world"))
#' unique(format_capitalize(iris$Species))
#' @export
format_capitalize <- function(x, verbose = TRUE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    if (verbose) {
      warning(format_message("This function only works on factors or character vector."), call. = FALSE)
    }
    return(x)
  }
  capped <- grep("^[A-Z]", x, invert = TRUE)
  substr(x[capped], 1, 1) <- toupper(substr(x[capped], 1, 1))
  x
}
