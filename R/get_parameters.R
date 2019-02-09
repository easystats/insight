#' @title Get model parameters
#' @name get_parameters
#'
#' @description Get model parameters
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return For non-Bayesian models and if \code{effects = "fixed"}, a data frame
#'    with two columns: the parameters names and the related point estimates; if
#'    \code{effects = "random"}, a list with the random effects (as returned by
#'    \code{ranef()}. For Bayesian models, the posterior samples from the
#'    requested parameters as data frame.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters <- function(x,...) {
  UseMethod("get_parameters")
}


#' @export
get_parameters.default <- function(x, ...) {
  cf <- stats::coef(x)
  data.frame(
    parameter = names(cf),
    estimate = unname(cf),
    stringsAsFactors = FALSE
  )
}


#' @rdname get_parameters
#' @export
get_parameters.merMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x),
    random = lme4::ranef(x)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.MixMod <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  l <- compact_list(list(
    conditional = lme4::fixef(x, sub_model = "main"),
    random = lme4::ranef(x),
    zero_inflated = lme4::fixef(x, sub_model = "zero_part")
  ))


  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    group = "conditional",
    stringsAsFactors = FALSE
  )

  fixedzi <- data.frame(
    parameter = names(l$zero_inflated),
    estimate = unname(l$zero_inflated),
    group = "zero_inflated",
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    switch(
      component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
  } else if (effects == "random") {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.glmmTMB <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  l <- compact_list(list(
    conditional = lme4::fixef(x)$cond,
    random = lme4::ranef(x)$cond,
    zero_inflated = lme4::fixef(x)$zi,
    zero_inflated_random = lme4::ranef(x)$zi,
    dispersion = lme4::fixef(x)$disp
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    group = "conditional",
    stringsAsFactors = FALSE
  )

  fixedzi <- data.frame(
    parameter = names(l$zero_inflated),
    estimate = unname(l$zero_inflated),
    group = "zero_inflated",
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    switch(
      component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
  } else if (effects == "random") {
    switch(
      component,
      all = list(l$random, l$zero_inflated_random),
      conditional = l$random,
      zi = ,
      zero_inflated = l$zero_inflated_random
    )
  }
}


#' @rdname get_parameters
#' @export
get_parameters.brmsfit <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  as.data.frame(x)[get_parms_data(x, effects, component)]
}


#' @rdname get_parameters
#' @export
get_parameters.stanreg <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)
  as.data.frame(x)[get_parms_data(x, effects, "all")]
}


get_parms_data <- function(x, effects, component) {
  elements <- get_elements(effects, component)
  unlist(find_parameters(x)[elements])
}