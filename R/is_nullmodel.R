#' @title Checks if model is a null-model (intercept-only)
#' @name is_nullmodel
#'
#' @description Checks if model is a null-model (intercept-only), i.e. if
#'   the conditional part of the model has no predictors.
#'
#' @param x A model object.
#'
#' @return `TRUE` if `x` is a null-model, `FALSE` otherwise.
#'
#' @examples
#' model <- lm(mpg ~ 1, data = mtcars)
#' is_nullmodel(model)
#'
#' model <- lm(mpg ~ gear, data = mtcars)
#' is_nullmodel(model)
#'
#' if (require("lme4")) {
#'   model <- lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
#'   is_nullmodel(model)
#'
#'   model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'   is_nullmodel(model)
#' }
#' @export
is_nullmodel <- function(x) {
  UseMethod("is_nullmodel")
}

#' @export
is_nullmodel.default <- function(x) {
  if (is_multivariate(x)) {
    unlist(lapply(find_predictors(x, effects = "fixed", component = "conditional"), .check_for_nullmodel))
  } else {
    .check_for_nullmodel(find_predictors(x, effects = "fixed", component = "conditional"))
  }
}

#' @export
is_nullmodel.afex_aov <- function(x) {
  FALSE
}

.check_for_nullmodel <- function(preds) {
  is.null(preds[["conditional"]])
}
