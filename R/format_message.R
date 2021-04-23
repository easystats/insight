#' @title Format messages and warnings
#' @name format_message
#'
#' @description Inserts line breaks into a longer message or warning string.
#'   Line length is adjusted to maximum length of the console, if the width
#'   can be accessed. By default, new lines are indented by two whitespace.
#'
#' @param string A string.
#' @param line_length Numeric, the maximum length of a line.
#'
#' @return A formatted string.
#' @examples
#' format_message("Much too long string for just one line, I guess!", 15)
#' @export
format_message <- function(string, line_length = options()$width) {
  if (is.null(line_length) || is.infinite(line_length) || line_length < 1) {
    line_length <- 70
  }

  linesep <- "\\1\n  "
  lsub <- 0

  pattern <- paste("(.{1,", line_length, "})(\\s|$)", sep = "")
  if (line_length > 0 && nchar(string) > line_length) {
    string <- gsub(pattern, linesep, string)
    l <- nchar(string)
    lc <- substr(string, l - lsub, l)
    if (lc == "\n") {
      string <- substr(string, 0, l - (lsub + 1))
    }
  }
  string
}
