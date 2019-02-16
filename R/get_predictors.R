#' @title Get the data from predictor variables
#' @name get_predictors
#'
#' @description Returns the data from all predictor variables (fixed effects).
#'
#' @inheritParams find_predictors
#'
#' @return The data from all predictor variables, as data frame.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' head(get_predictors(m))
#' @export
get_predictors <- function(x) {
  get_data(x)[, find_predictors(x, effects = "fixed", component = "all", flatten = TRUE), drop = FALSE]
}
