#' @title Get name of all model terms
#' @name model_terms
#'
#' @description Returns the name(s) of all model terms, including response value
#'   and random effects.
#'
#' @param x A fitted model.
#' @param ... Currently not used.
#'
#' @return Returns the name(s) of all model terms from \code{x} as character vector.
#'
#' @examples
#' library(lme4)
#' data(cbpp)
#' cbpp$trials <- cbpp$size - cbpp$incidence
#'
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' model_terms(m)
#'
#' m <- glmer(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#' model_terms(m)
#'
#' @export
model_terms <- function(x, ...) {
  t1 <- model_response(x, combine = FALSE)
  t2 <- model_predictors(x, effects = "all", zi = TRUE, disp = TRUE)
  unique(c(t1, t2))
}
