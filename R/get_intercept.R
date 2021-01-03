#' @title Get the value at the intercept
#' @name get_intercept
#'
#' @description Returns the value at the intercept (i.e., the intercept parameter), and \code{NA} if there isn't one.
#'
#' @inheritParams get_residuals
#'
#' @return The value of the intercept.
#'
#' @examples
#' get_intercept(lm(Sepal.Length ~ Petal.Width, data = iris))
#' get_intercept(lm(Sepal.Length ~ 0 + Petal.Width, data = iris))
#'
#' if (require("lme4")) {
#'   get_intercept(lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
#' }
#' if (require("rstanarm")) {
#'   get_intercept(rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris, refresh = 0, iter = 200))
#' }
#' if (require("gamm4")) {
#'   get_intercept(gamm4::gamm4(Sepal.Length ~ s(Petal.Width), data = iris))
#' }
#' @export
get_intercept <- function(x) {
  UseMethod("get_intercept")
}

#' @export
get_intercept.default <- function(x) {
  params <- insight::get_parameters(x)
  intercept <- params[params$Parameter == "(Intercept)", 2]
  if (length(intercept) == 0) {
    intercept <- NA
  }
  intercept
}

#' @export
get_intercept.stanreg <- function(x) {
  params <- insight::get_parameters(x)
  if ("(Intercept)" %in% names(params)) {
    params[["(Intercept)"]]
  } else {
    NA
  }
}
