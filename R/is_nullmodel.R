#' @title Checks if model is a null-model (intercept-only)
#' @name is_nullmodel
#'
#' @description Checks if model is a null-model (intercept-only), i.e. if
#'   the conditional part of the model has no predictors.
#'
#' @param x A model object.
#'
#' @return \code{TRUE} if \code{x} is a null-model, \code{FALSE} othwerwise.
#'
#' @examples
#' model <- lm(mpg ~ 1, data = mtcars)
#' is_nullmodel(model)
#'
#' model <- lm(mpg ~ gear, data = mtcars)
#' is_nullmodel(model)
#'
#' library(lme4)
#' model <- lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
#' is_nullmodel(model)
#'
#' model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' is_nullmodel(model)
#'
#' @export
is_nullmodel <- function(x) {
  if (is_multivariate(x)) {
    unlist(lapply(find_predictors(x, effects = "fixed", component = "conditional"), .check_for_nullmodel))
  } else {
    .check_for_nullmodel(find_predictors(x, effects = "fixed", component = "conditional"))
  }
}

.check_for_nullmodel <- function(preds) {
  is.null(preds[["conditional"]])
}
