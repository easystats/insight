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
#'   line_length = 15
#' )
#' message(msg)
#'
#' msg <- format_message("Much too long string for just one line, I guess!",
#'   "First new line",
#'   "Second new line",
#'   "(both indented)",
#'   line_length = 30
#' )
#' message(msg)
#' @export
format_message <- function(string, ..., line_length = 0.9 * options()$width) {
  if (is.null(line_length) || is.infinite(line_length) || line_length < 1) {
    line_length <- 70
  }

  all_lines <- c(string, ...)

  string <- .wrap_message_line(all_lines[1], line_length)
  further_lines <- all_lines[-1]

  if (length(further_lines)) {
    further_lines <- lapply(further_lines, function(i) {
      .wrap_message_line(string = i, line_length = line_length, indention = "  ")
    })
    string <- paste0(c(string, unlist(further_lines)), collapse = "\n")
  }

  string
}



.wrap_message_line <- function(string, line_length, indention = NULL) {
  line_length <- round(line_length)
  line_separator <- "\\1\n  "
  lsub <- 0
  tmp_string <- string

  # these chars are allowed inside a token, e.g. "{.i allowedchars}"
  allowed_chars <- "([a-zA-Z:\\./\\+-]*)"
  # supported tokens, create regex pattern
  token_pattern <- sprintf("\\{\\.%s %s\\}", c("b", "i", "url", "pkg"), allowed_chars)
  # for line breaks, we "protect" these patterns
  token_protected <- sprintf("\\{\\.%s_\\1\\}", c("b", "i", "url", "pkg"))

  # check if string contains any formatting token
  which_tokens <- .find_tokens(string)

  # check ansi-colors are supported by system. if not, remove tokens from string
  if (!.supports_color() && !is.null(which_tokens)) {
    for (i in token_pattern[which_tokens]) {
      string <- gsub(i, "\\1", string)
    }
    which_tokens <- NULL
  }

  # remove tokens from temporary string, so we can detect the "real" line length
  if (!is.null(which_tokens)) {
    for (i in token_pattern[which_tokens]) {
      tmp_string <- gsub(i, "\\1", tmp_string)
    }
    # protect tokens from line break
    for (i in which(which_tokens)) {
      string <- gsub(token_pattern[i], token_protected[i], string)
    }
  } else {
    tmp_string <- string
  }

  # check if line breaks are required
  if (line_length > 0 && nchar(tmp_string) > line_length) {
    # insert line breaks into string at specified length
    pattern <- paste("(.{1,", line_length, "})(\\s|$)", sep = "")
    string <- gsub(pattern, line_separator, string)

    # remove last line break
    l <- nchar(string)
    lc <- substr(string, l - lsub, l)
    if (lc == "\n") {
      string <- substr(string, 0, l - (lsub + 1))
    }
  }

  # convert tokens into formatting
  if (!is.null(which_tokens)) {
    for (i in which(which_tokens)) {
      if (token_pattern[i] == token_pattern[1]) {
        # bold formatting
        pattern <- paste0("(.*)\\{\\.b_", allowed_chars, "\\}(.*)")
        s1 <- gsub(pattern, "\\1", string)
        s2 <- gsub(pattern, "\\2", string)
        s3 <- gsub(pattern, "\\3", string)
        s2 <- .bold(s2)
      } else if (token_pattern[i] == token_pattern[2]) {
        # italic formatting
        pattern <- paste0("(.*)\\{\\.i_", allowed_chars, "\\}(.*)")
        s1 <- gsub(pattern, "\\1", string)
        s2 <- gsub(pattern, "\\2", string)
        s3 <- gsub(pattern, "\\3", string)
        s2 <- .italic(s2)
      } else if (token_pattern[i] == token_pattern[3]) {
        # url formatting
        pattern <- paste0("(.*)\\{\\.url_", allowed_chars, "\\}(.*)")
        s1 <- gsub(pattern, "\\1", string)
        s2 <- gsub(pattern, "\\2", string)
        s3 <- gsub(pattern, "\\3", string)
        s2 <- .italic(.blue(paste0("<", s2, ">")))
      } else if (token_pattern[i] == token_pattern[4]) {
        # package formatting
        pattern <- paste0("(.*)\\{\\.pkg_", allowed_chars, "\\}(.*)")
        s1 <- gsub(pattern, "\\1", string)
        s2 <- gsub(pattern, "\\2", string)
        s3 <- gsub(pattern, "\\3", string)
        s2 <- .blue(s2)
      }
      string <- paste0(s1, s2, s3)
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



# check whether a string line contains one of the supported format tags
.find_tokens <- function(string) {
  tokens <- c("{.b ", "{.i ", "{.url ", "{.pkg ")
  matches <- sapply(tokens, grepl, string, fixed = TRUE)
  if (any(matches)) {
    matches
  } else {
    return(NULL)
  }
}
