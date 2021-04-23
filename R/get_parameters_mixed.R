#' @title Get model parameters from mixed models
#' @name get_parameters.glmm
#'
#' @description Returns the coefficients from a model.
#'
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters.glmmTMB
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return If \code{effects = "fixed"}, a data frame with two columns: the
#'   parameter names and the related point estimates. If \code{effects =
#'   "random"}, a list of data frames with the random effects (as returned by
#'   \code{ranef()}), unless the random effects have the same simplified
#'   structure as fixed effects (e.g. for models from \pkg{MCMCglmm}).
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' \code{effects} and \code{component} can be used.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.glmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

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

  .remove_backticks_from_parameter_names(params)
}



#' @rdname get_parameters.glmm
#' @export
get_parameters.coxme <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.wbm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  if (effects == "fixed") {
    s <- summary(x)

    terms <- c(
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
      Parameter = terms,
      Estimate = params[[1]],
      Component = params[["component"]],
      stringsAsFactors = FALSE
    )

    .remove_backticks_from_parameter_names(out)
  } else {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("To use this function, please install package 'lme4'.")
    }
    lme4::ranef(x)
  }
}


#' @export
get_parameters.wbgee <- function(x, ...) {
  get_parameters.wbm(x, effects = "fixed")
}



#' @export
get_parameters.nlmerMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  startvectors <- .get_startvector_from_env(x)
  fx <- lme4::fixef(x)

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors]
    ))
  } else {
    l <- .compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors],
      random = lapply(lme4::ranef(x), colnames)
    ))
  }


  fixed <- data.frame(
    Parameter = c(
      names(l$conditional),
      names(l$nonlinear)
    ),
    Estimate = c(unname(l$conditional), unname(l$nonlinear)),
    Component = c(
      rep("fixed", length(l$conditional)),
      rep("nonlinear", length(l$nonlinear))
    ),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @rdname get_parameters.glmm
#' @export
get_parameters.merMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
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
get_parameters.merModList <- function(x, ...) {
  s <- suppressWarnings(summary(x))
  fixed <- data.frame(
    Parameter = s$fe$term,
    Estimate = s$fe$estimate,
    stringsAsFactors = FALSE
  )

  .remove_backticks_from_parameter_names(fixed)
}

#' @export
get_parameters.HLfit <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    utils::capture.output(s <- summary(x))
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.sem <- function(x, effects = c("fixed", "random"), ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = x$coef)
  } else {
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.cpglmm <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = cplm::fixef(x))
  } else {
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.mixed <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x$full_model))
  } else {
    l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.MixMod <- function(x,
                                  effects = c("fixed", "random"),
                                  component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  has_zeroinf <- !is.null(find_formula(x, verbose = FALSE)[["zero_inflated"]])

  if (component %in% c("zi", "zero_inflated") && !has_zeroinf) {
    stop("Model has no zero-inflation component.", call. = FALSE)
  }


  re.names <- dimnames(lme4::ranef(x))[[2]]
  re <- lme4::ranef(x)


  if (has_zeroinf) {
    z_inflated <- lme4::fixef(x, sub_model = "zero_part")
    z_inflated_random <- re[grepl("^zi_", re.names, perl = TRUE)]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
    component <- "conditional"
  }


  l <- .compact_list(list(
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
    .remove_backticks_from_parameter_names(params)
  } else if (effects == "random") {
    switch(component,
      all = .compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = list(random = l$random),
      zi = ,
      zero_inflated = list(zero_inflated_random = l$zero_inflated_random)
    )
  }
}



#' @rdname get_parameters.glmm
#' @export
get_parameters.glmmTMB <- function(x,
                                   effects = c("fixed", "random"),
                                   component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = lme4::fixef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      dispersion = lme4::fixef(x)$disp
    ))
  } else {
    l <- .compact_list(list(
      conditional = lme4::fixef(x)$cond,
      random = lme4::ranef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      zero_inflated_random = lme4::ranef(x)$zi,
      dispersion = lme4::fixef(x)$disp
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

  if (.obj_has_name(l, "zero_inflated")) {
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

  if (.obj_has_name(l, "dispersion")) {
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
    .remove_backticks_from_parameter_names(out)
  } else if (effects == "random") {
    switch(component,
      all = .compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = l$random,
      zi = ,
      zero_inflated = l$zero_inflated_random
    )
  }
}



#' @export
get_parameters.mixor <- function(x, effects = c("all", "fixed", "random"), ...) {
  coefs <- stats::coef(x)
  effects <- match.arg(effects)

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
    "all" = rbind(fixed, random),
    "fixed" = fixed,
    "random" = random
  )
}



#' @export
get_parameters.BBmm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  l <- .compact_list(list(
    conditional = x$fixed.coef,
    random = x$random.coef
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @rdname get_parameters.glmm
#' @export
get_parameters.glimML <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  l <- .compact_list(list(
    conditional = x@fixed.param,
    random = x@random.param
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  fixed <- .remove_backticks_from_parameter_names(fixed)

  random <- data.frame(
    Parameter = names(l$random),
    Estimate = l$random,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  random <- .remove_backticks_from_parameter_names(random)

  all <- rbind(
    cbind(fixed, data.frame(Effects = "fixed", stringsAsFactors = FALSE)),
    cbind(random, data.frame(Effects = "random", stringsAsFactors = FALSE))
  )

  if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all
  }
}
