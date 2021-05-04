#' @title Find names of all variables
#' @name find_variables
#'
#' @description Returns a list with the names of all variables, including
#'   response value and random effects.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @note The difference to \code{\link{find_terms}} is that \code{find_variables()}
#'   returns each variable name only once, while \code{find_terms()} may return a
#'   variable multiple times in case of transformations or when arithmetic expressions
#'   were used in the formula.
#'
#' @return A list with (depending on the model) following elements (character
#'    vectors):
#'    \itemize{
#'      \item \code{response}, the name of the response variable
#'      \item \code{conditional}, the names of the predictor variables from the \emph{conditional} model (as opposed to the zero-inflated part of a model)
#'      \item \code{random}, the names of the random effects (grouping factors)
#'      \item \code{zero_inflated}, the names of the predictor variables from the \emph{zero-inflated} part of the model
#'      \item \code{zero_inflated_random}, the names of the random effects (grouping factors)
#'      \item \code{dispersion}, the name of the dispersion terms
#'      \item \code{instruments}, the names of instrumental variables
#'    }
#'
#' @examples
#' if (require("lme4")) {
#'   data(cbpp)
#'   data(sleepstudy)
#'   # some data preparation...
#'   cbpp$trials <- cbpp$size - cbpp$incidence
#'   sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#'   sleepstudy$mysubgrp <- NA
#'   for (i in 1:5) {
#'     filter_group <- sleepstudy$mygrp == i
#'     sleepstudy$mysubgrp[filter_group] <-
#'       sample(1:30, size = sum(filter_group), replace = TRUE)
#'   }
#'
#'   m1 <- glmer(
#'     cbind(incidence, size - incidence) ~ period + (1 | herd),
#'     data = cbpp,
#'     family = binomial
#'   )
#'   find_variables(m1)
#'
#'   m2 <- lmer(
#'     Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'     data = sleepstudy
#'   )
#'   find_variables(m2)
#'   find_variables(m2, flatten = TRUE)
#' }
#' @export
find_variables <- function(x,
                           effects = c("all", "fixed", "random"),
                           component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "instruments", "smooth_terms"),
                           flatten = FALSE,
                           verbose = TRUE) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (component %in% c("all", "conditional")) {
    resp <- find_response(x, combine = FALSE)
  } else {
    resp <- NULL
  }

  pr <- find_predictors(
    x,
    effects = effects,
    component = component,
    flatten = flatten,
    verbose = verbose
  )

  if (flatten) {
    unique(c(resp, pr))
  } else if (is.null(resp)) {
    pr
  } else {
    c(list(response = resp), pr)
  }
}
