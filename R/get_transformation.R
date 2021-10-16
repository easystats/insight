#' @title Return function of transformed response variables
#' @name get_transformation
#'
#' @description This functions checks whether any transformation, such as log-
#'   or exp-transforming, was applied to the response variable (dependent
#'   variable) in a regression formula, and returns the related function that
#'   was used for transformation.
#'
#' @param x A regression model.
#' @return A list of two functions: `$transformation`, the function that was
#'   used to transform the response variable; `$inverse`, the inverse-function
#'   of `$transformation` (can be used for "back-transformation"). If no
#'   transformation was applied, both list-elements `$transformation` and
#'   `$inverse` just return `function(x) x`.
#'
#' @examples
#' # identity, no transformation
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' get_transformation(model)
#'
#' # log-transformation
#' model <- lm(log(Sepal.Length) ~ Species, data = iris)
#' get_transformation(model)
#'
#' # log-function
#' get_transformation(model)$transformation(.3)
#' log(.3)
#'
#' # inverse function is exp()
#' get_transformation(model)$invserse(.3)
#' exp(.3)
#' @export
get_transformation <- function(x) {

  transform_fun <- find_transformation(x)

  if (transform_fun == "identity") {
    out <- list(transformation = function(x) x, inverse = function(x) x)
  } else if (transform_fun == "log") {
    out <- list(transformation = log, inverse = exp)
  } else if (transform_fun %in% c("log1p", "log(x+1)")) {
    out <- list(transformation = log1p, inverse = expm1)
  } else if (transform_fun == "exp") {
    out <- list(transformation = exp, inverse = log)
  } else if (transform_fun == "sqrt") {
    out <- list(transformation = sqrt, inverse = function(x) x ^ 2)
  } else if (transform_fun == "expm1") {
    out <- list(transformation = expm1, inverse = log1p)
  } else if (transform_fun == "log-log") {
    out <- list(transformation = function(x) log(log(x)),
                inverse = function(x) exp(exp(x)))
  }


  out
}
