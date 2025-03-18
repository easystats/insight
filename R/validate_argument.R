#' @title Validate arguments against a given set of options
#' @name validate_argument
#'
#' @description This is a replacement for `match.arg()`, however, the error
#' string should be more informative for users. The name of the affected argument
#' is shown, and possible typos as well as remaining valid options. Not that
#' the argument `several.ok` is always `FALSE` in `validate_argument()`, i.e.
#' this function - unlike `match.arg()` - does *not* allow evaluating several
#' valid options at once.
#'
#' @param argument The bare name of the argument to be validated.
#' @param options Valid options, usually a character vector.
#'
#' @return `argument` if it is a valid option, else an error is thrown.
#'
#' @examples
#' foo <- function(test = "small") {
#'   validate_argument(test, c("small", "medium", "large"))
#' }
#' foo("small")
#' # errors:
#' # foo("masll")
#' @export
validate_argument <- function(argument, options) {
  # save this information for printin
  argument_name <- deparse(substitute(argument))
  original_argument <- argument
  # catch error, we want our own message
  argument <- .safe(match.arg(argument, options))
  # proceed here if argument option was invalid
  if (is.null(argument)) {
    # check whether we find a typo
    suggestion <- .misspelled_string(options, original_argument)
    msg <- sprintf("Invalid option for argument `%s`.", argument_name)
    if (is.null(suggestion$msg) || !length(suggestion$msg) || !nzchar(suggestion$msg)) {
      msg <- paste(msg, "Please use one of the following options:", .to_string(options))
    } else {
      options <- setdiff(options, suggestion$possible_strings)
      msg <- paste(msg, suggestion$msg)
      if (length(options)) {
        msg <- paste(msg, "Otherwise, use one of the following options:", .to_string(options))
      }
    }
    format_error(msg)
  }
  argument
}


.misspelled_string <- function(valid_strings, searchterm, default_message = NULL) {
  if (is.null(searchterm) || length(searchterm) < 1) {
    return(default_message)
  }
  # used for many matches
  more_found <- ""
  # init default
  msg <- ""
  # remove matching strings
  same <- intersect(valid_strings, searchterm)
  searchterm <- setdiff(searchterm, same)
  valid_strings <- setdiff(valid_strings, same)
  # guess the misspelled string
  possible_strings <- unlist(lapply(searchterm, function(s) {
    valid_strings[.fuzzy_grep(valid_strings, s)] # nolint
  }), use.names = FALSE)
  if (length(possible_strings)) {
    msg <- "Did you mean "
    if (length(possible_strings) > 1) {
      # make sure we don't print dozens of alternatives for larger data frames
      if (length(possible_strings) > 5) {
        more_found <- sprintf(
          " We even found %i more possible matches, not shown here.",
          length(possible_strings) - 5
        )
        possible_strings <- possible_strings[1:5]
      }
      msg <- paste0(msg, "one of ", .to_string(possible_strings))
    } else {
      msg <- paste0(msg, "\"", possible_strings, "\"")
    }
    msg <- paste0(msg, "?", more_found)
  } else {
    msg <- default_message
  }
  # no double white space
  list(msg = trim_ws(msg), possible_strings = possible_strings)
}


.fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) {
    precision <- round(nchar(pattern) / 3)
  }
  if (precision > nchar(pattern)) {
    return(NULL)
  }
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}


.to_string <- function(text, sep = ", ", last = " or ", enclose = "\"") {
  if (length(text) == 1 && !nzchar(text, keepNA = TRUE)) {
    return(text)
  }
  text <- text[text != ""] # nolint
  if (length(text) && !is.null(enclose)) {
    text <- paste0(enclose, text, enclose)
  }
  if (length(text) == 1) {
    s <- text
  } else {
    s <- paste(text[1:(length(text) - 1)], collapse = sep)
    s <- paste(c(s, text[length(text)]), collapse = last)
  }
  s
}
