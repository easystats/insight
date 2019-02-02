#' @title Find name of all model terms
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
#' @return A list with (depending on the model) several character vectors:
#'    \itemize{
#'      \item \code{response}, the name of the response value
#'      \item \code{conditional}, the name(s) of the predictor variables from the \emph{conditional} model (as opposed to the zero-inflated part of a model)
#'      \item \code{random}, the name of the random effects (grouping factors)
#'      \item \code{zero_inflated}, the name(s) of the predictor variables from the \emph{zero-inflated} part of the model
#'      \item \code{zero_inflated_random}, the name of the random effects (grouping factors)
#'      \item \code{disperion}, the name of the dispersion terms
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
  info <- model_info(x, no_terms = TRUE)

  t.y <- find_response(x, combine = FALSE)
  t.cond <- find_predictors(x, effects = "fixed", component = "conditional")
  t.re <- find_random(x, split_nested = TRUE, component = "conditional")

  if (info$is_zeroinf) {
    t.zi <- find_predictors(x, effects = "fixed", component = "zi")
    t.zi.re <- find_random(x, split_nested = TRUE, component = "zi")
  } else {
    t.zi.re <- NULL
    t.zi <- NULL
  }

  t.disp <- suppressWarnings(
    find_predictors(x, effects = "fixed", component = "dispersion")
  )

  allterms <- compact_list(list(
    response = unname(t.y),
    conditional = unname(t.cond),
    random = unname(t.re),
    zero_inflated = unname(t.zi),
    zero_inflated_random = unname(t.zi.re),
    dispersion = unname(t.disp)
  ))

  if (flatten)
    unique(unlist(allterms))
  else
    allterms
}
