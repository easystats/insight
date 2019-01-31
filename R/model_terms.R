#' @title Get name of all model terms
#' @name model_terms
#'
#' @description Returns the name(s) of all model terms, including response value
#'   and random effects.
#'
#' @inheritParams model_predictors
#'
#' @return Returns the name(s) of all model terms from \code{x} as character vector.
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
#'   sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' m1 <- glmer(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#' model_terms(m1)
#'
#' m2 <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#' model_terms(m2)
#'
#' @export
model_terms <- function(x, ...) {
  t1 <- model_response(x, combine = FALSE)
  t2 <- model_predictors(x, effects = "all", zi = TRUE, disp = TRUE)
  unique(c(t1, t2))
}
