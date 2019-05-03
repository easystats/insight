#' @title Checks if model has an intercept
#' @name has_intercept
#'
#' @description Checks if model has an intercept.
#'
#' @param x A model object.
#'
#' @return \code{TRUE} if \code{x} has an intercept, \code{FALSE} othwerwise.
#'
#' @examples
#' model <- lm(mpg ~ 0 + gear, data = mtcars)
#' has_intercept(model)
#'
#' model <- lm(mpg ~ gear, data = mtcars)
#' has_intercept(model)
#'
#' library(lme4)
#' model <- lmer(Reaction ~ 0 + Days + (Days | Subject), data = sleepstudy)
#' has_intercept(model)
#'
#' model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' has_intercept(model)
#'
#' @export
has_intercept <- function(x) {
  !("0" %in% find_variables(x)$conditional)
}
