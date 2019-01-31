#' @title Get the data from a model's predictor variables
#' @name predictors_data
#'
#' @description Returns the data of all predictor variables (fixed effects).
#'
#' @inheritParams model_predictors
#'
#' @return The data of all predictor variables, as data frame.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' predictors_data(m)
#'
#' @export
predictors_data <- function(x, ...) {
  model_data(x)[, model_predictors(x, effects = "fixed", zi = TRUE), drop = FALSE]
}
