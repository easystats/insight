#' Confidence/Credible Interval (CI) Formatting
#'
#' @param CI_low Lower CI bound.
#' @param CI_high Upper CI bound.
#' @param ci CI level in percentage.
#' @param digits Number of significant digits.
#' @param brackets Either a logical, and if \code{TRUE} (default), values are encompassed in square brackets. If \code{FALSE} or \code{NULL}, no brackets are used. Else, a character vector of length two, indicating the opening and closing brackets.
#' @param width Minimum width of the returned string. If not \code{NULL} and \code{width} is larger than the string's length, leading whitespaces are added to the string. If \code{width="auto"}, width will be set to the length of the longest string.
#' @param width_low,width_high Like \code{width}, but only applies to the lower or higher confidence interval value. This can be used when the values for the lower and upper CI are of very different length.
#' @inheritParams format_value
#'
#' @return A formatted string.
#' @examples
#' format_ci(1.20, 3.57, ci = 0.90)
#' format_ci(1.20, 3.57, ci = NULL)
#' format_ci(1.20, 3.57, ci = NULL, brackets = FALSE)
#' format_ci(1.20, 3.57, ci = NULL, brackets = c("(", ")"))
#' format_ci(c(1.205645, 23.4), c(3.57, -1.35), ci = 0.90)
#' format_ci(c(1.20, NA, NA), c(3.57, -1.35, NA), ci = 0.90)
#'
#' # automatic alignment of width, useful for printing multiple CIs in columns
#' x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4))
#' cat(x, sep = "\n")
#'
#' x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4), width = "auto")
#' cat(x, sep = "\n")
#' @export
format_ci <- function(CI_low, CI_high, ci = 0.95, digits = 2, brackets = TRUE, width = NULL, width_low = width, width_high = width, missing = "", zap_small = FALSE) {
  # check proper defaults
  if (isTRUE(brackets)) {
    ci_brackets <- c("[", "]")
  } else if (is.null(brackets) || isFALSE(brackets)) {
    ci_brackets <- c("", "")
  } else {
    ci_brackets <- brackets
  }

  if (!is.null(width) && width == "auto") {
    if (is.numeric(CI_low) && is.numeric(CI_high)) {
      CI_low <- round(CI_low, digits)
      CI_high <- round(CI_high, digits)
    }
    if (all(is.na(CI_low))) {
      width_low <- 1
    } else {
      width_low <- max(unlist(lapply(stats::na.omit(CI_low), function(.i) {
        if (.i > 1e+5) {
          6 + digits
        } else {
          nchar(as.character(.i))
        }
      })))
    }
    if (all(is.na(CI_high))) {
      width_high <- 1
    } else {
      width_high <- max(unlist(lapply(stats::na.omit(CI_high), function(.i) {
        if (.i > 1e+5) {
          6 + digits
        } else {
          nchar(as.character(.i))
        }
      })))
    }
  }

  if (is.na(missing)) missing <- NA_character_

  if (!is.null(ci)) {
    ifelse(is.na(CI_low) & is.na(CI_high), missing, paste0(ci * 100, "% CI ", .format_ci(CI_low, CI_high, digits = digits, ci_brackets = ci_brackets, width_low = width_low, width_high = width_high, missing = missing, zap_small = zap_small)))
  } else {
    ifelse(is.na(CI_low) & is.na(CI_high), missing, .format_ci(CI_low, CI_high, digits = digits, ci_brackets = ci_brackets, width_low = width_low, width_high = width_high, missing = missing, zap_small = zap_small))
  }
}

#' @keywords internal
.format_ci <- function(CI_low, CI_high, digits = 2, ci_brackets = c("[", "]"), width_low = NULL, width_high = NULL, missing = "NA", zap_small = FALSE) {
  paste0(
    ci_brackets[1],
    format_value(CI_low, digits = digits, missing = missing, width = width_low, zap_small = zap_small),
    ", ",
    format_value(CI_high, digits = digits, missing = missing, width = width_high, zap_small = zap_small),
    ci_brackets[2]
  )
}
