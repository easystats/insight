#' @title Get model parameters from mixed models
#' @name get_parameters.glmmTMB
#'
#' @description Returns the coefficients from a model.
#'
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters.glmmTMB
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @inheritSection find_predictors Model components
#'
#' @return If `effects = "fixed"`, a data frame with two columns: the
#'   parameter names and the related point estimates. If `effects =
#'   "random"`, a list of data frames with the random effects (as returned by
#'   `ranef()`), unless the random effects have the same simplified
#'   structure as fixed effects (e.g. for models from **MCMCglmm**).
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' `effects` and `component` can be used. See details in the section
#' _Model Components_.
#'
#' @examplesIf requireNamespace("glmmTMB", quietly = TRUE)
#' data(Salamanders, package = "glmmTMB")
#' m <- glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' get_parameters(m)
#' @export
get_parameters.glmmTMB <- function(x, effects = "fixed", component = "all", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion", "location", "distributional", "auxiliary") # nolint
  )

  if (effects == "fixed") {
    l <- compact_list(list(
      conditional = lme4::fixef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      dispersion = lme4::fixef(x)$disp
    ))
  } else {
    l <- compact_list(list(
      conditional = lme4::fixef(x)$cond,
      random = lme4::ranef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      zero_inflated_random = lme4::ranef(x)$zi,
      dispersion = lme4::fixef(x)$disp,
      dispersion_random = lme4::ranef(x)$disp
    ))
  }

  # ---- fixed effects (conditional model)

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  # ---- fixed effects (zero_inflated model)

  if (object_has_names(l, "zero_inflated")) {
    fixedzi <- data.frame(
      Parameter = names(l$zero_inflated),
      Estimate = unname(l$zero_inflated),
      Component = "zero_inflated",
      stringsAsFactors = FALSE
    )
  } else {
    fixedzi <- NULL
  }

  # ---- fixed effects (dispersion model)

  if (object_has_names(l, "dispersion")) {
    fixeddisp <- data.frame(
      Parameter = names(l$dispersion),
      Estimate = unname(l$dispersion),
      Component = "dispersion",
      stringsAsFactors = FALSE
    )
  } else {
    fixeddisp <- NULL
  }

  # ---- build result

  if (effects == "fixed") {
    out <- switch(component,
      all = rbind(fixed, fixedzi, fixeddisp),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi,
      dispersion = fixeddisp
    )
    text_remove_backticks(out)
  } else if (effects == "random") {
    switch(component,
      all = compact_list(list(
        random = l$random,
        zero_inflated_random = l$zero_inflated_random,
        dispersion_random = l$dispersion_random
      )),
      conditional = l$random,
      zi = ,
      zero_inflated = l$zero_inflated_random,
      dispersion_random = l$dispersion_random
    )
  }
}


#' @export
get_parameters.glmm <- function(x, effects = "all", ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  params <- data.frame(
    Parameter = names(c(x$beta, x$nu)),
    Estimate = unname(c(x$beta, x$nu)),
    Effects = c(rep("fixed", times = length(x$beta)), rep("random", times = length(x$nu))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects != "all") {
    params <- params[params$Effects == effects, , drop = FALSE]
    params$Effects <- NULL
  }

  text_remove_backticks(params)
}


#' @export
get_parameters.coxme <- function(x, effects = "fixed", ...) {
  check_if_installed("lme4")
  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.wbm <- function(x, effects = "fixed", ...) {
  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    s <- summary(x)

    wbm_params <- c(
      rownames(s$within_table),
      rownames(s$between_table),
      rownames(s$ints_table)
    )

    wt <- s$within_table
    bt <- s$between_table
    it <- s$ints_table

    if (!is.null(wt)) {
      wt <- data.frame(params = wt, component = "within", stringsAsFactors = FALSE)
    }
    if (!is.null(bt)) {
      bt <- data.frame(params = bt, component = "between", stringsAsFactors = FALSE)
    }
    if (!is.null(it)) {
      it <- data.frame(params = it, component = "interactions", stringsAsFactors = FALSE)
    }

    params <- rbind(wt, bt, it)

    out <- data.frame(
      Parameter = wbm_params,
      Estimate = params[[1]],
      Component = params[["component"]],
      stringsAsFactors = FALSE
    )

    text_remove_backticks(out)
  } else {
    check_if_installed("lme4")
    lme4::ranef(x)
  }
}


#' @export
get_parameters.wbgee <- function(x, ...) {
  get_parameters.wbm(x, effects = "fixed")
}


#' @export
get_parameters.nlmerMod <- function(x, effects = "fixed", component = "all", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))
  component <- validate_argument(component, c("all", "conditional", "nonlinear"))

  startvectors <- .get_startvector_from_env(x)
  fx <- lme4::fixef(x)

  if (effects == "fixed") {
    l <- compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors]
    ))
  } else {
    l <- compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors],
      random = lapply(lme4::ranef(x), colnames)
    ))
  }

  fixed_cond <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    Component = rep("fixed", length(l$conditional)),
    stringsAsFactors = FALSE
  )

  fixed_nl <- data.frame(
    Parameter = names(l$nonlinear),
    Estimate = unname(l$nonlinear),
    Component = rep("nonlinear", length(l$nonlinear)),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    params <- switch(component,
      all = rbind(fixed_cond, fixed_nl),
      conditional = fixed_cond,
      nonlinear = fixed_nl
    )
    text_remove_backticks(params)
  } else {
    l$random
  }
}


#' @export
get_parameters.merMod <- function(x, effects = "fixed", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}

#' @export
get_parameters.rlmerMod <- get_parameters.merMod

#' @export
get_parameters.glmmadmb <- get_parameters.merMod

#' @export
get_parameters.lme <- get_parameters.merMod


#' @export
get_parameters.svy2lme <- function(x, ...) {
  l <- list(conditional = stats::coef(x))
  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(fixed)
}


#' @export
get_parameters.merModList <- function(x, ...) {
  s <- suppressWarnings(summary(x))
  fixed <- data.frame(
    Parameter = s$fe$term,
    Estimate = s$fe$estimate,
    stringsAsFactors = FALSE
  )

  text_remove_backticks(fixed)
}


#' @export
get_parameters.HLfit <- function(x, effects = "fixed", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    utils::capture.output(s <- summary(x)) # nolint
    l <- compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.sem <- function(x, effects = "fixed", ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = x$coef)
  } else {
    l <- compact_list(list(
      conditional = x$coef,
      random = x$ranef
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.cpglmm <- function(x, effects = "fixed", ...) {
  check_if_installed("cplm")

  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = cplm::fixef(x))
  } else {
    l <- compact_list(list(
      conditional = cplm::fixef(x),
      random = cplm::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.mixed <- function(x, effects = "fixed", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x$full_model))
  } else {
    l <- compact_list(list(
      conditional = lme4::fixef(x$full_model),
      random = lme4::ranef(x$full_model)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.MixMod <- function(x, effects = "fixed", component = "all", ...) {
  check_if_installed("lme4")

  effects <- validate_argument(effects, c("fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion", "location", "distributional", "auxiliary") # nolint
  )

  has_zeroinf <- !is.null(find_formula(x, verbose = FALSE)[["zero_inflated"]])

  if (component %in% c("zi", "zero_inflated") && !has_zeroinf) {
    format_error("Model has no zero-inflation component.")
  }

  re.names <- dimnames(lme4::ranef(x))[[2]]
  re <- lme4::ranef(x)

  if (has_zeroinf) {
    z_inflated <- lme4::fixef(x, sub_model = "zero_part")
    z_inflated_random <- re[startsWith(re.names, "zi_")]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
    component <- "conditional"
  }


  l <- compact_list(list(
    conditional = lme4::fixef(x, sub_model = "main"),
    random = re[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  if (has_zeroinf) {
    fixedzi <- data.frame(
      Parameter = names(l$zero_inflated),
      Estimate = unname(l$zero_inflated),
      Component = "zero_inflated",
      stringsAsFactors = FALSE
    )
  } else {
    fixedzi <- NULL
  }

  if (effects == "fixed") {
    params <- switch(component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
    text_remove_backticks(params)
  } else if (effects == "random") {
    switch(component,
      all = compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = list(random = l$random),
      zi = ,
      zero_inflated = list(zero_inflated_random = l$zero_inflated_random)
    )
  }
}


#' @export
get_parameters.hglm <- function(x, effects = "fixed", component = "all", ...) {
  effects <- validate_argument(effects, c("fixed", "random"))
  component <- validate_argument(
    component,
    c("all", "conditional", "dispersion", "location", "distributional", "auxiliary")
  )

  fe <- x$fixef
  re <- x$ranef

  f <- find_formula(x, verbose = FALSE)
  if (is.null(f$dispersion)) {
    dispersion <- NULL
  } else {
    disp <- summary(x)$SummVC1
    dispersion <- data.frame(
      Parameter = rownames(disp),
      Estimate = as.vector(disp[, 1]),
      Component = "dispersion",
      stringsAsFactors = FALSE
    )
  }

  fixed <- data.frame(
    Parameter = names(fe),
    Estimate = unname(fe),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  random <- data.frame(
    Parameter = names(re),
    Estimate = unname(re),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  out <- switch(effects,
    fixed = switch(component,
      conditional = fixed,
      dispersion = dispersion,
      rbind(fixed, dispersion)
    ),
    all = switch(component,
      conditional = rbind(fixed, random),
      dispersion = dispersion,
      rbind(fixed, random, dispersion)
    ),
    random
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.mixor <- function(x, effects = "all", ...) {
  coefs <- stats::coef(x)
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  params <- find_parameters(x, effects = "fixed", flatten = TRUE)
  fixed <- data.frame(
    Parameter = params,
    Estimate = unname(coefs[params]),
    Effects = "fixed",
    stringsAsFactors = FALSE
  )

  if (effects != "fixed") {
    params <- find_parameters(x, effects = "random", flatten = TRUE)
    random <- data.frame(
      Parameter = params,
      Estimate = unname(coefs[params]),
      Effects = "random",
      stringsAsFactors = FALSE
    )
  } else {
    random <- NULL
  }

  switch(effects,
    all = rbind(fixed, random),
    fixed = fixed,
    random = random
  )
}


#' @export
get_parameters.BBmm <- function(x, effects = "fixed", ...) {
  effects <- validate_argument(effects, c("fixed", "random"))

  l <- compact_list(list(
    conditional = x$fixed.coef,
    random = x$random.coef
  ))

  fixed <- data.frame(
    Parameter = rownames(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects == "fixed") {
    text_remove_backticks(fixed)
  } else {
    l$random
  }
}


#' @export
get_parameters.glimML <- function(x, effects = "fixed", ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))

  l <- compact_list(list(
    conditional = x@fixed.param,
    random = x@random.param
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  fixed <- text_remove_backticks(fixed)

  random <- data.frame(
    Parameter = names(l$random),
    Estimate = l$random,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  random <- text_remove_backticks(random)

  all_effects <- rbind(
    cbind(fixed, data.frame(Effects = "fixed", stringsAsFactors = FALSE)),
    cbind(random, data.frame(Effects = "random", stringsAsFactors = FALSE))
  )

  if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all_effects
  }
}
