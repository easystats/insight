#' @title Find name of all model terms
#' @name find_terms
#'
#' @description Returns a list with the name(s) of all model terms, including
#'   response value and random effects.
#'
#' @inheritParams find_predictors
#'
#' @return A list with (depending on the model) following elements (character
#'    vectors):
#'    \itemize{
#'      \item \code{response}, the name of the response variable
#'      \item \code{conditional}, the name(s) of the predictor variables from the \emph{conditional} model (as opposed to the zero-inflated part of a model)
#'      \item \code{random}, the name of the random effects (grouping factors)
#'      \item \code{zero_inflated}, the name(s) of the predictor variables from the \emph{zero-inflated} part of the model
#'      \item \code{zero_inflated_random}, the name of the random effects (grouping factors)
#'      \item \code{dispersion}, the name of the dispersion terms
#'    }
#'
#' @examples
#' library(lme4)
#' data(cbpp)
#' data(sleepstudy)
#' # some data preparation...
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$mysubgrp <- NA
#' for (i in 1:5) {
#'   filter_group <- sleepstudy$mygrp == i
#'   sleepstudy$mysubgrp[filter_group] <-
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m1 <- glmer(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#' find_terms(m1)
#'
#' m2 <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#' find_terms(m2)
#' find_terms(m2, flatten = TRUE)
#' @export
find_terms <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments"), flatten = FALSE) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (component %in% c("all", "conditional")) {
    resp <- find_response(x, combine = FALSE)
  } else {
    resp <- NULL
  }

  pr <- find_predictors(x, effects = effects, component = component, flatten = flatten)

  if (flatten) {
    unique(c(resp, pr))
  } else if (is.null(resp)) {
    pr
  } else {
    c(list(response = resp), pr)
  }
}
