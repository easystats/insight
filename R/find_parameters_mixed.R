#' @title Find names of model parameters from mixed models
#' @name find_parameters.glmmTMB
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output.
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_parameters.BGGM
#'
#' @inheritSection find_predictors Model components
#'
#' @return A list of parameter names. The returned list may have following
#' elements, usually returned based on the combination of the `effects` and
#' `component` arguments:
#'
#' - `conditional`, the "fixed effects" part from the model.
#' - `random`, the "random effects" part from the model.
#' - `zero_inflated`, the "fixed effects" part from the zero-inflation component
#'   of the model.
#' - `zero_inflated_random`, the "random effects" part from the zero-inflation
#'   component of the model.
#' - `dispersion`, the dispersion parameters (auxiliary parameter)
#' - `dispersion_random`, the "random effects" part from the dispersion
#'   parameters (auxiliary parameter)
#' - `nonlinear`, the parameters from the nonlinear formula.
#'
#' @examplesIf requireNamespace("lme4", quietly = TRUE)
#' data(sleepstudy, package = "lme4")
#' m <- lme4::lmer(
#'   Reaction ~ Days + (1 + Days | Subject),
#'   data = sleepstudy
#' )
#' find_parameters(m)
#' @export
find_parameters.glmmTMB <- function(x, effects = "all", component = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion", "location", "distributional", "auxiliary")
  )

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- compact_list(list(
      conditional = names(lme4::fixef(x)$cond),
      zero_inflated = names(lme4::fixef(x)$zi),
      dispersion = names(lme4::fixef(x)$disp)
    ))
  } else {
    l <- compact_list(list(
      conditional = names(lme4::fixef(x)$cond),
      random = lapply(lme4::ranef(x)$cond, colnames),
      zero_inflated = names(lme4::fixef(x)$zi),
      zero_inflated_random = lapply(lme4::ranef(x)$zi, colnames),
      dispersion = names(lme4::fixef(x)$disp),
      dispersion_random = names(lme4::ranef(x)$disp)
    ))
  }

  .filter_parameters(l,
    effects = effects,
    component = component,
    flatten = flatten
  )
}


#' @export
find_parameters.MixMod <- function(x, effects = "all", component = "all", flatten = FALSE, ...) {
  # installed
  check_if_installed("lme4")

  re.names <- dimnames(lme4::ranef(x))[[2]]

  has_zeroinf <- !is.null(find_formula(x, verbose = FALSE)[["zero_inflated"]])

  if (has_zeroinf) {
    z_inflated <- names(lme4::fixef(x, sub_model = "zero_part"))
    z_inflated_random <- re.names[startsWith(re.names, "zi_")]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
  }

  l <- compact_list(list(
    conditional = names(lme4::fixef(x, sub_model = "main")),
    random = grep("^(?!zi_)", re.names, perl = TRUE, value = TRUE),
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  l <- lapply(l, text_remove_backticks)

  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(component, c("all", "conditional", "zi", "zero_inflated"))

  elements <- .get_elements(effects = effects, component = component)
  l <- compact_list(l[elements])

  if (flatten) {
    unique(unlist(l, use.names = FALSE))
  } else {
    l
  }
}


#' @export
find_parameters.nlmerMod <- function(x, effects = "all", component = "all", flatten = FALSE, ...) {
  # installed
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(component, c("all", "conditional", "nonlinear"))

  startvectors <- .get_startvector_from_env(x)

  if (effects == "fixed") {
    l <- compact_list(list(
      conditional = setdiff(names(lme4::fixef(x)), startvectors),
      nonlinear = startvectors
    ))
  } else {
    l <- compact_list(list(
      conditional = setdiff(names(lme4::fixef(x)), startvectors),
      nonlinear = startvectors,
      random = lapply(lme4::ranef(x), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, component = component, flatten = flatten)
}


#' @export
find_parameters.hglm <- function(x, effects = "all", component = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "dispersion", "location", "distributional", "auxiliary")
  )

  fe <- x$fixef
  re <- x$ranef

  f <- find_formula(x, verbose = FALSE)
  if (is.null(f$dispersion)) {
    disp_name <- NULL
  } else {
    disp <- summary(x)$SummVC1
    disp_name <- rownames(disp)
  }

  l <- compact_list(list(
    conditional = names(fe),
    random = names(re),
    dispersion = disp_name
  ))

  .filter_parameters(l, effects = effects, component = component, flatten = flatten)
}


#' @export
find_parameters.merMod <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    l <- compact_list(list(
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
find_parameters.merModList <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  find_parameters(x[[1]], effects = effects, flatten = flatten, ...)
}


#' @export
find_parameters.svy2lme <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(stats::coef(x)))
  } else {
    l <- compact_list(list(
      conditional = names(stats::coef(x)),
      random = stats::setNames(as.list(unname(x$znames)), names(x$znames))
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.HLfit <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  # installed
  check_if_installed("lme4")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    utils::capture.output(s <- summary(x)) # nolint
    l <- compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = s$lambda_table$Term
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.sem <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  if (!.is_semLme(x)) {
    return(NULL)
  }

  l <- compact_list(list(
    conditional = names(x$coef),
    random = colnames(x$ranef)
  ))

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.cpglmm <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  # installed
  check_if_installed("cplm")

  # we extract random effects only when really necessary, to save
  # computational time. In particular model with large sample and
  # many random effects groups may take some time to return random effects

  if (effects == "fixed") {
    l <- list(conditional = names(cplm::fixef(x)))
  } else {
    l <- compact_list(list(
      conditional = names(cplm::fixef(x)),
      random = lapply(cplm::ranef(x), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.coxme <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  check_if_installed("lme4")

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    l <- compact_list(list(
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
find_parameters.mixed <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  # installed
  check_if_installed("lme4")

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x$full_model)))
  } else {
    l <- compact_list(list(
      conditional = names(lme4::fixef(x$full_model)),
      random = lapply(lme4::ranef(x$full_model), colnames)
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.lme <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  check_if_installed("lme4")

  if (effects == "fixed") {
    l <- list(conditional = names(lme4::fixef(x)))
  } else {
    re <- lme4::ranef(x)
    if (is.data.frame(re)) {
      rn <- colnames(re)
    } else {
      rn <- lapply(re, colnames)
    }

    l <- compact_list(list(
      conditional = names(lme4::fixef(x)),
      random = rn
    ))
  }

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.glmm <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  s <- summary(x)
  fe_params <- rownames(s$coefmat)
  re_params <- rownames(s$nucoefmat)

  l <- compact_list(list(
    conditional = fe_params,
    random = re_params
  ))

  .filter_parameters(l, effects = effects, flatten = flatten)
}


#' @export
find_parameters.BBmm <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  l <- compact_list(list(
    conditional = rownames(x$fixed.coef),
    random = x$namesRand
  ))

  .filter_parameters(l,
    effects = effects,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.glimML <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  l <- compact_list(list(
    conditional = names(x@fixed.param),
    random = names(x@random.param)
  ))

  .filter_parameters(l,
    effects = effects,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.mixor <- function(x, effects = "all", flatten = FALSE, ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  coefs <- x$Model
  random_start <- grep("(\\(Intercept\\) \\(Intercept\\)|Random\\.\\(Intercept\\))", rownames(coefs))
  thresholds <- grep("Threshold\\d", rownames(coefs))

  l <- list(
    conditional = rownames(coefs)[c(1, thresholds, 2:(random_start - 1))],
    random = rownames(coefs)[random_start:(thresholds[1] - 1)]
  )

  .filter_parameters(l, effects = effects, flatten = flatten)
}
