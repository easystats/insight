#' @title Name the model
#' @name model_name
#'
#' @description Returns the value at the intercept (i.e., the intercept parameter), and \code{NA} if there isn't one.
#'
#' @inheritParams get_residuals
#'
#' @return The value of the intercept.
#'
#' @examples
#' model_name(lm(Sepal.Length ~ Petal.Width, data = iris))
#'
#' if (require("lme4")) {
#'   model_name(lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
#' }
#' if (require("rstanarm")) {
#'   model_name(rstanarm::stan_glm(Sepal.Length ~ Petal.Width,
#'   data = iris, refresh = 0, iter = 200))
#' }
#' # if (require("gamm4")) {
#' #   model_name(gamm4::gamm4(Sepal.Length ~ s(Petal.Width), data = iris))
#' #}
#' @export
model_name <- function(x, ...) {
  UseMethod("model_name")
}

#' @export
model_name.default <- function(x, include_formula=TRUE, include_call=FALSE, ...) {
  if(include_call){
    return(format(get_call(x)))
  }

  name <- class(x)[[1]]
  if(include_formula){
    f <- format(find_formula(x))
    name <- paste0(name, "(", f, ")")
  }
  name
}
