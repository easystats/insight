#' String Values Formatting
#'
#' @param x String value.
#' @param length Numeric, maximum length of the returned string. If not \code{NULL},
#'   will shorten the string to a maximum \code{length}, however, it will not
#'   truncate inside words. I.e. if the string length happens to be inside a word,
#'   this word is removed from the returned string, so the returned string has
#'   a \emph{maximum} length of \code{length}, but might be shorter.
#' @param abbreviate String that will be used as suffix, if \code{x} was shortened.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A formatted string.
#'
#' @examples
#' s <- "This can be considered as very long string!"
#' # string is shorter than max.length, so returned as is
#' format_string(s, 60)
#'
#' # string is shortened to as many words that result in
#' # a string of maximum 20 chars
#' format_string(s, 20)
#' @export
format_string <- function(x, ...) {
  UseMethod("format_string")
}


#' @export
format_string.default <- function(x, ...) {
  x
}


#' @export
format_string.data.frame <- function(x, length = NULL, abbreviate = "...", ...) {
  as.data.frame(sapply(x, format_string, length = length, abbreviate = abbreviate, simplify = FALSE))
}


#' @rdname format_string
#' @export
format_string.character <- function(x, length = NULL, abbreviate = "...", ...) {
  if (!is.null(length)) {
    pattern <- paste("(.{1,", length, "})(\\s|$)", sep = "")
    tmp <- paste0(substr(x, 0, unlist(regexec(abbreviate, sub(pattern, replacement = paste0("\\1", abbreviate), x), fixed = TRUE)) - 1), abbreviate)
    too.long <- nchar(x) > length
    x[too.long] <- tmp[too.long]
  }
  x
}
