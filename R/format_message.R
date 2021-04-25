#' @title Format messages and warnings
#' @name format_message
#'
#' @description Inserts line breaks into a longer message or warning string.
#'   Line length is adjusted to maximum length of the console, if the width
#'   can be accessed. By default, new lines are indented by two whitespace.
#'
#' @param string A string.
#' @param ... Further strings that will be concatenated as indented new lines.
#' @param line_length Numeric, the maximum length of a line.
#'
#' @return A formatted string.
#' @examples
#' msg <- format_message("Much too long string for just one line, I guess!",
#'                       line_length = 15)
#' message(msg)
#'
#' msg <- format_message("Much too long string for just one line, I guess!",
#'                       "First new line",
#'                       "Second new line",
#'                       "(both indented)",
#'                       line_length = 30)
#' message(msg)
#' @export
format_message <- function(string, ..., line_length = options()$width) {
  if (is.null(line_length) || is.infinite(line_length) || line_length < 1) {
    line_length <- 70
  }

  string <- .wrap_message_line(string, line_length)
  further_lines <- list(...)

  if (length(further_lines)) {
    further_lines <- lapply(further_lines, function(i) {
      .wrap_message_line(string = i, line_length = line_length, indention = "  ")
    })
    string <- paste0(c(string, unlist(further_lines)), collapse = "\n")
  }

  string
}



.wrap_message_line <- function(string, line_length, indention = NULL) {
  line_separator <- "\\1\n  "
  lsub <- 0
  pattern <- paste("(.{1,", line_length, "})(\\s|$)", sep = "")

  if (line_length > 0 && nchar(string) > line_length) {
    string <- gsub(pattern, line_separator, string)
    l <- nchar(string)
    lc <- substr(string, l - lsub, l)
    if (lc == "\n") {
      string <- substr(string, 0, l - (lsub + 1))
    }
  }

  # remove trailing newline
  if (grepl("\\n  $", string)) {
    string <- gsub("\\n  $", "", string)
  }

  if (!is.null(indention)) {
    string <- paste0(indention, string)
  }

  string
}