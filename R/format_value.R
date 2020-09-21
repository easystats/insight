#' Numeric Values Formatting
#'
#' @param x Numeric value.
#' @param digits Number of significant digits.
#' @param protect_integers Should integers be kept as integers (i.e., without decimals)?
#' @param missing Value by which \code{NA} values are replaced. By default, an empty string (i.e. \code{""}) is returned for \code{NA}.
#' @param width Minimum width of the returned string. If not \code{NULL} and \code{width} is larger than the string's length, leading whitespaces are added to the string.
#' @param as_percent Logical, if \code{TRUE}, value is formatted as percentage value.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return A formatted string.
#'
#' @examples
#' format_value(1.20)
#' format_value(1.2)
#' format_value(1.2012313)
#' format_value(c(0.0045, 234, -23))
#' format_value(c(0.0045, .12, .34))
#' format_value(c(0.0045, .12, .34), as_percent = TRUE)
#'
#' format_value(as.factor(c("A", "B", "A")))
#' format_value(iris$Species)
#'
#' format_value(3)
#' format_value(3, protect_integers = TRUE)
#'
#' format_value(iris)
#' @export
format_value <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, as_percent = FALSE, ...) {
  UseMethod("format_value")
}


#' @export
format_value.data.frame <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, as_percent = FALSE, ...) {
  as.data.frame(sapply(x, format_value, digits = digits, protect_integers = protect_integers, missing = missing, width = width, as_percent = as_percent, simplify = FALSE))
}


#' @export
format_value.numeric <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, as_percent = FALSE, ...) {
  if (protect_integers) {
    out <- .format_value_unless_integer(x, digits = digits, .missing = missing, .width = width, .as_percent = as_percent, ...)
  } else {
    out <- .format_value(x, digits = digits, .missing = missing, .width = width, .as_percent = as_percent, ...)
  }

  # Deal with negative zeros
  if (!is.factor(x)) {
    whitespace <- ifelse(is.null(width), "", " ")
    out[out == "-0"] <- paste0(whitespace, "0")
    out[out == "-0.0"] <- paste0(whitespace, "0.0")
    out[out == "-0.00"] <- paste0(whitespace, "0.00")
    out[out == "-0.000"] <- paste0(whitespace, "0.000")
    out[out == "-0.0000"] <- paste0(whitespace, "0.0000")
  }
  out
}

#' @export
format_value.double <- format_value.numeric

#' @export
format_value.character <- format_value.numeric

#' @export
format_value.factor <- format_value.numeric

#' @export
format_value.logical <- format_value.numeric




#' @importFrom stats na.omit
.format_value_unless_integer <- function(x, digits = 2, .missing = "", .width = NULL, .as_percent = FALSE, ...) {
  if (is.numeric(x) && !all(.is.int(stats::na.omit(x)))) {
    .format_value(x, digits = digits, .missing = .missing, .width = .width, .as_percent = .as_percent)
  } else if (anyNA(x)) {
    .convert_missing(x, .missing)
  } else {
    as.character(x)
  }
}



.format_value <- function(x, digits = 2, .missing = "", .width = NULL, .as_percent = FALSE, ...) {
  # proper character NA
  if (is.na(.missing)) .missing <- NA_character_

  if (is.numeric(x)) {
    if (isTRUE(.as_percent)) {
      need_sci <- (abs(100 * x) >= 1e+5 | log10(abs(100 * x)) < -digits) & x != 0
      x <- ifelse(is.na(x), .missing,
                  ifelse(need_sci, sprintf("%.*e%%", digits, 100 * x),
                         sprintf("%.*f%%", digits, 100 * x)))
    } else {
      need_sci <- (abs(x) >= 1e+5 | log10(abs(x)) < -digits) & x != 0
      x <- ifelse(is.na(x), .missing,
                  ifelse(need_sci, sprintf("%.*e", digits, x),
                         sprintf("%.*f", digits, x)))
    }
  } else if (anyNA(x)) {
    x <- .convert_missing(x, .missing)
  }

  if (!is.null(.width)) {
    x <- format(x, justify = "right", width = .width)
  }

  x
}


.convert_missing <- function(x, .missing) {
  if (is.na(.missing)) {
    .missing <- NA_character_
  } else {
    .missing <- as.character(.missing)
  }

  if (length(x) == 1) {
    return(.missing)
  }

  missings <- which(is.na(x))
  x[missings] <- .missing
  x[!missings] <- as.character(x)
  x
}



.is.int <- function(x) {
  tryCatch(
    expr = {
      ifelse(is.infinite(x), FALSE, x %% 1 == 0)
    },
    warning = function(w) {
      is.integer(x)
    },
    error = function(e) {
      FALSE
    }
  )
}
