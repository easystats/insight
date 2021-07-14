#' @title Checks if a model is a mixed effects model
#' @name is_mixed_model
#'
#' @description Small helper that checks if a model is a mixed effects model,
#' i.e. if it the model has random effects.
#'
#' @param x A model object.
#'
#' @return A logical, \code{TRUE} if \code{x} is a mixed model.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' is_mixed_model(model)
#'
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#'   is_mixed_model(model)
#' }
#' @export
is_mixed_model <- function(x) {
  UseMethod("is_mixed_model")
}

#' @export
is_mixed_model.defult <- function(x) {
  !is.null(find_random(x))
}

#' @export
is_mixed_model.afex_aov <- function(x) {
  as.logical(length(attr(x, "within")))
}
