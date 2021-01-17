#' @title Checks if model has an intercept
#' @name has_intercept
#'
#' @description Checks if model has an intercept.
#'
#' @param x A model object.
#'
#' @return \code{TRUE} if \code{x} has an intercept, \code{FALSE} otherwise.
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
has_intercept <- function(x) {
  if (is_multivariate(x)) {
    unlist(lapply(find_terms(x), .check_for_intercept))
  } else {
    .check_for_intercept(find_terms(x))
  }
}

.check_for_intercept <- function(vars) {
  !any(c("0", "-1") %in% vars[["conditional"]])
}
