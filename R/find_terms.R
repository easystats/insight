#' @title Get name of all model terms
#' @name find_terms
#'
#' @description Returns a list with the name(s) of all model terms, including
#'   response value and random effects.
#'
#' @param flatten Logical, if \code{TRUE}, the name of model terms are returned
#'    as a single character, not as list.
#'
#' @inheritParams find_predictors
#'
#' @return A list with three character vectors: \code{response}, the name of
#'    the response value; \code{predictors}, the name(s) of the predictor
#'    variables; and \code{random}, the name of the random effects (grouping
#'    factors).
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
#' find_terms(m1)
#'
#' m2 <- lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#' find_terms(m2)
#' find_terms(m2, flatten = TRUE)
#'
#' @export
find_terms <- function(x, ...) {
  UseMethod("find_terms")
}


#' @rdname find_terms
#' @export
find_terms.default <- function(x, flatten = FALSE, ...) {
  info <- model_info(x)

  t.y <- find_response(x, combine = FALSE)
  t.cond <- find_predictors(x, effects = "fixed", component = "cond")
  t.re <- find_random(x, split_nested = TRUE, component = "cond")

  if (info$is_zeroinf) {
    t.zi <- find_predictors(x, effects = "fixed", component = "zi")
    t.zi.re <- find_random(x, split_nested = TRUE, component = "zi")
  } else {
    t.zi.re <- NULL
    t.zi <- NULL
  }

  t.disp <- suppressWarnings(
    find_predictors(x, effects = "fixed", component = "disp")
  )

  allterms <- compact_list(list(
    response = unname(t.y),
    predictors = unname(t.cond),
    random = unname(t.re),
    zi = unname(t.zi),
    zi_random = unname(t.zi.re),
    disp = unname(t.disp)
  ))

  if (flatten)
    unique(unlist(allterms))
  else
    allterms
}
