#' @title Checks if model has an intercept
#' @name has_intercept
#'
#' @description Checks if model has an intercept.
#'
#' @param x A model object.
#' @param verbose Toggle warnings.
#'
#' @return `TRUE` if `x` has an intercept, `FALSE` otherwise.
#'
#' @examples
#' model <- lm(mpg ~ 0 + gear, data = mtcars)
#' has_intercept(model)
#'
#' model <- lm(mpg ~ gear, data = mtcars)
#' has_intercept(model)
#'
#' if (require("lme4")) {
#'   model <- lmer(Reaction ~ 0 + Days + (Days | Subject), data = sleepstudy)
#'   has_intercept(model)
#'
#'   model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'   has_intercept(model)
#' }
#' @export
has_intercept <- function(x, verbose = TRUE) {
  f <- find_formula(x)
  if (is_multivariate(x)) {
    lapply(f, .check_for_intercept, verbose = verbose)
  } else {
    .check_for_intercept(f, verbose)
  }
}

.check_for_intercept <- function(f, verbose = TRUE) {
  if (!is.null(f$conditional)) {
    f_terms <- stats::terms(f$conditional)
    intercept <- as.vector(attr(f_terms, "intercept"))
    return(intercept == 1)
  } else if (verbose) {
    warning(format_message("Cannot extract terms from model formula."), call. = FALSE)
  }
}
