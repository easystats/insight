#' @title Find names of model parameters from mixed models
#' @name find_parameters.glmmTMB
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output.
#'
#' @param component Which type of parameters to return, such as parameters for
#'   the conditional model, the zero-inflated part of the model or the
#'   dispersion term? Applies to models with zero-inflated and/or dispersion
#'   formula. Note that the *conditional* component is also called
#'   *count* or *mean* component, depending on the model. There are
#'   three convenient shortcuts: `component = "all"` returns all possible
#'   parameters. If `component = "location"`, location parameters such as
#'   `conditional` or `zero_inflated` are returned (everything that
#'   are fixed or random effects - depending on the `effects` argument -
#'   but no auxiliary parameters). For `component = "distributional"` (or
#'   `"auxiliary"`), components like `sigma` or `dispersion` (and
#'   other auxiliary parameters) are returned.
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_parameters.BGGM
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. The returned list may have following
#'   elements:
#'    \itemize{
#'      \item `conditional`, the "fixed effects" part from the model.
#'      \item `random`, the "random effects" part from the model.
#'      \item `zero_inflated`, the "fixed effects" part from the
#'      zero-inflation component of the model.
#'      \item `zero_inflated_random`, the "random effects" part from the
#'      zero-inflation component of the model.
#'      \item `dispersion`, the dispersion parameters (auxiliary parameter)
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters.glmmTMB <- function(x,
                                    effects = c("all", "fixed", "random"),
                                    component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                                    flatten = FALSE,
                                    ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)$cond),
      zero_inflated = names(lme4::fixef(x)$zi),
      dispersion = names(lme4::fixef(x)$disp)
    ))
  } else {
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)$cond),
      random = lapply(lme4::ranef(x)$cond, colnames),
      zero_inflated = names(lme4::fixef(x)$zi),
      zero_inflated_random = lapply(lme4::ranef(x)$zi, colnames),
      dispersion = names(lme4::fixef(x)$disp)
    ))
  }

  .filter_parameters(l,
    effects = effects,
    component = component,
    flatten = flatten
  )
}



#' @export
find_parameters.MixMod <- function(x,
                                   effects = c("all", "fixed", "random"),
                                   component = c("all", "conditional", "zi", "zero_inflated"),
                                   flatten = FALSE,
                                   ...) {
  # installed
  check_if_installed("lme4")

  re.names <- dimnames(lme4::ranef(x))[[2]]

  has_zeroinf <- !is.null(find_formula(x)[["zero_inflated"]])

  if (has_zeroinf) {
    z_inflated <- names(lme4::fixef(x, sub_model = "zero_part"))
    z_inflated_random <- re.names[grepl("^zi_", re.names, perl = TRUE)]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x, sub_model = "main")),
    random = re.names[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects = effects, component = component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.nlmerMod <- function(x,
                                     effects = c("all", "fixed", "random"),
                                     flatten = FALSE,
                                     ...) {
  # installed
  check_if_installed("lme4")

  effects <- match.arg(effects)
  startvectors <- .get_startvector_from_env(x)

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = setdiff(names(lme4::fixef(x)), startvectors),
      nonlinear = startvectors
    ))
  } else {
    l <- .compact_list(list(
      conditional = setdiff(names(lme4::fixef(x)), startvectors),
      nonlinear = startvectors,
      random = lapply(lme4::ranef(x), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @rdname find_parameters.glmmTMB
#' @export
find_parameters.merMod <- function(x,
                                   effects = c("all", "fixed", "random"),
                                   flatten = FALSE,
                                   ...) {
  effects <- match.arg(effects)

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = lapply(lme4::ranef(x), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}

#' @export
find_parameters.rlmerMod <- find_parameters.merMod

#' @export
find_parameters.glmmadmb <- find_parameters.merMod

#' @export
find_parameters.merModList <- function(x,
                                       effects = c("all", "fixed", "random"),
                                       flatten = FALSE,
                                       ...) {
  effects <- match.arg(effects)
  find_parameters(x[[1]], effects = effects, flatten = flatten, ...)
}

#' @export
find_parameters.HLfit <- function(x,
                                  effects = c("all", "fixed", "random"),
                                  flatten = FALSE,
                                  ...) {
  effects <- match.arg(effects)

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    utils::capture.output(s <- summary(x))
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = s$lambda_table$Term
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.sem <- function(x,
                                effects = c("all", "fixed", "random"),
                                flatten = FALSE,
                                ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  effects <- match.arg(effects)

  l <- .compact_list(list(
    conditional = names(x$coef),
    random = colnames(x$ranef)
  ))

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.cpglmm <- function(x,
                                   effects = c("all", "fixed", "random"),
                                   flatten = FALSE,
                                   ...) {
  # installed
  check_if_installed("cplm")

  effects <- match.arg(effects)

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(cplm::fixef(x)))
  } else {
    l <- .compact_list(list(
      conditional = names(cplm::fixef(x)),
      random = lapply(cplm::ranef(x), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.coxme <- function(x,
                                  effects = c("all", "fixed", "random"),
                                  flatten = FALSE,
                                  ...) {
  # installed?
  check_if_installed("lme4")

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = names(lme4::ranef(x))
    ))
  }

  .filter_parameters(l,
    effects = effects,
    flatten = flatten,
    recursive = FALSE
  )
}



#' @export
find_parameters.mixed <- function(x,
                                  effects = c("all", "fixed", "random"),
                                  flatten = FALSE,
                                  ...) {
  # installed
  check_if_installed("lme4")

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x$full_model)))
  } else {
    l <- .compact_list(list(
      conditional = names(lme4::fixef(x$full_model)),
      random = lapply(lme4::ranef(x$full_model), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.lme <- function(x,
                                effects = c("all", "fixed", "random"),
                                flatten = FALSE,
                                ...) {
  # installed?
  check_if_installed("lme4")

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    re <- lme4::ranef(x)
    if (is.data.frame(re)) {
      rn <- colnames(re)
    } else {
      rn <- lapply(re, colnames)
    }

    l <- .compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = rn
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.glmm <- function(x,
                                 effects = c("all", "fixed", "random"),
                                 flatten = FALSE,
                                 ...) {
  effects <- match.arg(effects)
  s <- summary(x)
  fe_params <- rownames(s$coefmat)
  re_params <- rownames(s$nucoefmat)

  l <- .compact_list(list(
    conditional = fe_params,
    random = re_params
  ))

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.BBmm <- function(x,
                                 effects = c("all", "fixed", "random"),
                                 flatten = FALSE,
                                 ...) {
  l <- .compact_list(list(
    conditional = names(x$fixed.coef),
    random = x$namesRand
  ))

  effects <- match.arg(effects)
  .filter_parameters(l,
    effects = effects,
    flatten = flatten,
    recursive = FALSE
  )
}



#' @export
find_parameters.glimML <- function(x,
                                   effects = c("all", "fixed", "random"),
                                   flatten = FALSE,
                                   ...) {
  l <- .compact_list(list(
    conditional = names(x@fixed.param),
    random = names(x@random.param)
  ))

  effects <- match.arg(effects)
  .filter_parameters(l,
    effects = effects,
    flatten = flatten,
    recursive = FALSE
  )
}



#' @export
find_parameters.mixor <- function(x,
                                  effects = c("all", "fixed", "random"),
                                  flatten = FALSE,
                                  ...) {
  effects <- match.arg(effects)
  coefs <- x$Model
  random_start <- grep("(\\(Intercept\\) \\(Intercept\\)|Random\\.\\(Intercept\\))", rownames(coefs))
  thresholds <- grep("Threshold\\d", rownames(coefs))

  l <- list(
    conditional = rownames(coefs)[c(1, thresholds, 2:(random_start - 1))],
    random = rownames(coefs)[random_start:(thresholds[1] - 1)]
  )

  .filter_parameters(l, effects = effects, flatten = flatten)
}
