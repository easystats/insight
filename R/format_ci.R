#' Confidence/Credible Interval (CI) Formatting
#'
#' @param CI_low Lower CI bound.
#' @param CI_high Upper CI bound.
#' @param ci CI level in percentage.
#' @param digits Number of significant digits.
#' @param brackets Logical, if \code{TRUE} (default), values are encompassed in square brackets.
#' @param width Minimum width of the returned string. If not \code{NULL} and \code{width} is larger than the string's length, leading whitespaces are added to the string. If \code{width="auto"}, width will be set to the length of the longest string.
#' @param width_low,width_high Like \code{width}, but only applies to the lower or higher confidence interval value. This can be used when the values for the lower and upper CI are of very different length.
#'
#' @return A formatted string.
#' @examples
#' format_ci(1.20, 3.57, ci = 0.90)
#' format_ci(1.20, 3.57, ci = NULL)
#' format_ci(1.20, 3.57, ci = NULL, brackets = FALSE)
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
format_ci <- function(CI_low, CI_high, ci = 0.95, digits = 2, brackets = TRUE, width = NULL, width_low = width, width_high = width) {
  if (!is.null(width) && width == "auto") {
    width_low <- max(unlist(lapply(stats::na.omit(round(CI_low, digits)), function(.i) nchar(as.character(.i)))))
    width_high <- max(unlist(lapply(stats::na.omit(round(CI_high, digits)), function(.i) nchar(as.character(.i)))))
  }

  if (!is.null(ci)) {
    ifelse(is.na(CI_low) & is.na(CI_high), "", paste0(ci * 100, "% CI ", .format_ci(CI_low, CI_high, digits = digits, brackets = brackets, width_low = width_low, width_high = width_high)))
  } else {
    ifelse(is.na(CI_low) & is.na(CI_high), "", .format_ci(CI_low, CI_high, digits = digits, brackets = brackets, width_low = width_low, width_high = width_high))
  }
}

#' @keywords internal
.format_ci <- function(CI_low, CI_high, digits = 2, brackets = TRUE, width_low = NULL, width_high = NULL) {
  paste0(
    ifelse(isTRUE(brackets), "[", ""),
    format_value(CI_low, digits = digits, missing = "NA", width = width_low),
    ", ",
    format_value(CI_high, digits = digits, missing = "NA", width = width_high),
    ifelse(isTRUE(brackets), "]", "")
  )
}
