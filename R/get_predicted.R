#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values).
#'
#' @inheritParams get_residuals
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' get_predicted(x)
#' @export
get_predicted <- function(x, ...){
  UseMethod("get_predicted")
}

#' @importFrom stats fitted
#' @export
get_predicted.default <- function(x, ...) {
  stats::fitted(x)
}