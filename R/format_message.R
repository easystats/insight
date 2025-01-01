#' @title Format messages and warnings
#' @name format_message
#'
#' @description Inserts line breaks into a longer message or warning string.
#'   Line length is adjusted to maximum length of the console, if the width
#'   can be accessed. By default, new lines are indented by two spaces.
#'
#'   `format_alert()` is a wrapper that combines formatting a string with a
#'   call to `message()`, `warning()` or `stop()`. By default, `format_alert()`
#'   creates a `message()`. `format_warning()` and `format_error()` change the
#'   default type of exception to `warning()` and `stop()`, respectively.
#'
#' @param string A string.
#' @param ... Further strings that will be concatenated as indented new lines.
#' @param line_length Numeric, the maximum length of a line.
#'   The default is 90% of the width of the console window.
#' @param indent Character vector. If further lines are specified in `...`, a
#' user-defined string can be specified to indent subsequent lines. Defaults to
#' `"  "` (two white spaces), hence for each start of the line after the first
#' line, two white space characters are inserted.
#'
#' @details
#' There is an experimental formatting feature implemented in this function.
#' You can use following tags:
#' * `{.b text}` for bold formatting
#' * `{.i text}` to use italic font style
#' * `{.url www.url.com}` formats the string as URL (i.e., enclosing URL in
#' `<` and `>`, blue color and italic font style)
#' * `{.pkg packagename}` formats the text in blue color.
#'
#' This features has some limitations: it's hard to detect the exact length for
#' each line when the string has multiple lines (after line breaks) and the
#' string contains formatting tags. Thus, it can happen that lines are wrapped at
#' an earlier length than expected. Furthermore, if you have multiple words in a
#' format tag (`{.b one two three}`), a line break might occur inside this tag,
#' and the formatting no longer works (messing up the message-string).
#'
#' @return For `format_message()`, a formatted string.
#'   For `format_alert()` and related functions, the requested exception,
#'   with the exception formatted using `format_message()`.
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
#'
#' msg <- format_message("Much too long string for just one line, I guess!",
#'   "First new line",
#'   "Second new line",
#'   "(not indented)",
#'   line_length = 30,
#'   indent = ""
#' )
#' message(msg)
#'
#' # Caution, experimental! See 'Details'
#' msg <- format_message(
#'   "This is {.i italic}, visit {.url easystats.github.io/easystats}",
#'   line_length = 30
#' )
#' message(msg)
#'
#' @export
format_message <- function(string,
                           ...,
                           line_length = 0.9 * getOption("width", 80),
                           indent = "  ") {
  if (is.null(line_length) || is.infinite(line_length) || line_length < 1L) {
    line_length <- 70
  }

  all_lines <- c(string, ...)

  string <- .wrap_message_line(all_lines[1], line_length = line_length)
  further_lines <- all_lines[-1]

  if (length(further_lines)) {
    further_lines <- lapply(further_lines, function(i) {
      .wrap_message_line(string = i, line_length = line_length, indent = indent)
    })
    string <- paste0(c(string, unlist(further_lines, use.names = FALSE)), collapse = "\n")
  }

  string
}

#' @name format_alert
#' @rdname format_message
#'
#' @param type Type of exception alert to raise.
#'   Can be `"message"` for `message()`, `"warning"` for `warning()`,
#'   or `"error"` for `stop()`.
#' @param call Logical. Indicating if the call should be included in the the
#'   error message. This is usually confusing for users when the function
#'   producing the warning or error is deep within another function, so the
#'   default is `FALSE`.
#' @param immediate Logical. Indicating if the *warning* should be printed
#'   immediately. Only applies to `format_warning()` or `format_alert()` with
#'   `type = "warning"`. The default is `FALSE`.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # message
#' format_alert("This is a message.")
#' format_alert("This is a warning.", type = "message")
#'
#' # error
#' try(format_error("This is an error."))
#'
#' @examplesIf getOption("warn") < 2L
#' # warning
#' format_warning("This is a warning.")
#'
#' @export
format_alert <- function(string,
                         ...,
                         line_length = 0.9 * getOption("width", 80),
                         indent = "  ",
                         type = "message",
                         call = FALSE,
                         immediate = FALSE) {
  type <- validate_argument(type, c("message", "warning", "error"))
  if (type == "message") {
    message(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ))
  } else if (type == "warning") {
    warning(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ), call. = call, immediate. = immediate)
  } else {
    stop(format_message(
      string = string, ...,
      line_length = line_length, indent = indent
    ), call. = call)
  }
}

#' @name format_warning
#' @rdname format_message
#' @export
format_warning <- function(..., immediate = FALSE) {
  format_alert(..., type = "warning", immediate = immediate)
}

#' @name format_error
#' @rdname format_message
#' @export
format_error <- function(...) {
  format_alert(..., type = "error")
}


# helper -----------------------

.wrap_message_line <- function(string, line_length, indent = NULL) {
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
    for (i in which(which_tokens)) {
      tmp_string <- gsub(token_pattern[i], "\\1", tmp_string)
      # protect tokens from line break
      string <- gsub(token_pattern[i], token_protected[i], string)
    }
  } else {
    tmp_string <- string
  }

  # check if line breaks are required
  if (line_length > 0 && nchar(tmp_string) > line_length) {
    # insert line breaks into string at specified length
    pattern <- paste0("(.{1,", line_length, "})(\\s|$)")
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
        s2 <- .bold(gsub(pattern, "\\2", string))
      } else if (token_pattern[i] == token_pattern[2]) {
        # italic formatting
        pattern <- paste0("(.*)\\{\\.i_", allowed_chars, "\\}(.*)")
        s2 <- .italic(gsub(pattern, "\\2", string))
      } else if (token_pattern[i] == token_pattern[3]) {
        # url formatting
        pattern <- paste0("(.*)\\{\\.url_", allowed_chars, "\\}(.*)")
        s2 <- .italic(.blue(paste0("<", gsub(pattern, "\\2", string), ">")))
      } else if (token_pattern[i] == token_pattern[4]) {
        # package formatting
        pattern <- paste0("(.*)\\{\\.pkg_", allowed_chars, "\\}(.*)")
        s2 <- .blue(gsub(pattern, "\\2", string))
      }
      s1 <- gsub(pattern, "\\1", string)
      s3 <- gsub(pattern, "\\3", string)
      string <- paste0(s1, s2, s3)
    }
  }

  # remove trailing white space
  if (grepl("\\n  $", string)) {
    string <- gsub("\\n  $", "", string)
  }

  if (!is.null(indent)) {
    string <- paste0(indent, string)
  }

  string
}


# check whether a string line contains one of the supported format tags
.find_tokens <- function(string) {
  tokens <- c("{.b ", "{.i ", "{.url ", "{.pkg ")
  matches <- vapply(tokens, grepl, TRUE, string, fixed = TRUE)
  if (!any(matches)) {
    return(NULL)
  }
  matches
}
