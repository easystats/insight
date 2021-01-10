#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values).
#'
#' @param ... Not used.
#' @inheritParams get_residuals
#'
#' @return The fitted values (i.e. predictions for the response).
#'
#' @note Currently, this function just calls \code{stats::fitted()}, but will
#' be extended to other objects that don't work with \code{stats::fitted()} in
#' future updates.
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' get_predicted(x)
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}

#' @importFrom stats fitted
#' @export
get_predicted.default <- function(x, ...) {
  stats::fitted(x)
}
