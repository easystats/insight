#' @title Find term names of model predictors
#' @name find_predictors
#'
#' @description to do...
#'
#' @param x A fitted model.
#' @param effects Should predictor variables for fixed effects, random effects
#'    or both be returned? Only applies to mixed models. May be abbreviated.
#' @param component Should all predictor variables, predictor variables for the
#'    conditional model, the zero-inflated part of the model or the dispersion
#'    model be returned? Only applies to models with zero-inflated and/or
#'    dispersion formula. May be abbreviated.
#' @param ... Currently not used.
#'
#' @return The name(s) of the predictor variables from \code{x} as character vector.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_predictors(m)
#'
#' @export
find_predictors <- function(x, ...) {
  UseMethod("find_predictors")
}


#' @rdname find_predictors
#' @importFrom stats formula terms
#' @export
find_predictors.default <- function(x, ...) {
  dots <- list(...)

  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  if (!obj_has_name(dots, "effects"))
    effects <- "all"
  else
    effects <- dots$effects

  if (effects == "random")
    return(find_random(x, split_nested = FALSE))

  fm <- find_formula(x)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @rdname find_predictors
#' @importFrom stats formula terms
#' @export
find_predictors.merMod <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)
  dots <- list(...)

  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  if (effects == "random")
    return(find_random(x, split_nested = FALSE))

  fm <- find_formula(x)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @rdname find_predictors
#' @export
find_predictors.hurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  find_zeroinf_predictors(x, component)
}


#' @export
find_predictors.zeroinf <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  find_zeroinf_predictors(x, component)
}


#' @export
find_predictors.zerotrunc <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  find_zeroinf_predictors(x, component)
}


#' @rdname find_predictors
#' @importFrom stats formula terms
#' @export
find_predictors.lme <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  if (effects == "random")
    return(find_random(x, split_nested = FALSE))

  fm <- find_formula(x, effects = "fixed")
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "all") {
    fm <- find_formula(x, effects = "random")
    modpred <- c(modpred, all.vars(fm[[2L]]))
  }

  unique(modpred)
}


#' @export
#' @importFrom stats terms
find_predictors.clm2 <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  fm <- find_formula(x)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}


#' @rdname find_predictors
#' @importFrom stats formula terms
#' @export
find_predictors.glmmTMB <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (effects == "random")
    return(find_random(x, split_nested = FALSE, component = component))

  modpred.zi <- NULL
  modpred.disp <- NULL

  fm <- find_formula(x, component = "all")

  # get terms from conditional model
  modpred.cond <- all.vars(fm$conditional[[3L]])

  if (length(modpred.cond) == 1 && modpred.cond == ".")
    modpred.cond <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "fixed" && component != "dispersion")
    modpred.cond <- remove_re_from_terms(x, modpred.cond, component)

  # get terms from zero-inflation
  modpred.zi <- tryCatch(
    {all.vars(fm$zero_inflated[[2L]])},
    error = function(x) { NULL }
  )

  if (!is_empty_object(modpred.zi)) {
    # remove random effects from formula
    if (effects == "fixed" && component != "dispersion")
      modpred.zi <- remove_re_from_terms(x, modpred.zi, component)
  } else {
    modpred.zi <- NULL
  }

  # get terms from zero-inflation
  modpred.disp <- tryCatch(
    {all.vars(fm$disp[[2L]])},
    error = function(x) { NULL }
  )

  if (is_empty_object(modpred.disp)) modpred.disp <- NULL

  switch(
    component,
    all = unique(c(modpred.cond, modpred.zi, modpred.disp)),
    conditional = unique(modpred.cond),
    zi = ,
    zero_inflated = unique(modpred.zi),
    dispersion = unique(modpred.disp)
  )
}


#' @importFrom stats formula terms
#' @export
find_predictors.brmsfit <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  if (effects == "random")
    return(find_random(x, split_nested = FALSE))

  fm <- find_formula(x)

  if (!is.null(fm$responses)) {
    modpred <- unique(unlist(lapply(fm$forms, function(.x) { all.vars(stats::formula(.x)[[3L]]) })))
  } else {
    modpred <- all.vars(fm$formula[[3L]])
  }

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @export
find_predictors.MCMCglmm <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  fm <- find_formula(x, effects = "all")

  fixed.modpred <- all.vars(fm$fixed[[3L]])
  random.modpred <- all.vars(fm$random[[2L]])

  modpred <- switch(
    effects,
    fixed = fixed.modpred,
    random = random.modpred,
    c(fixed.modpred, random.modpred)
  )

  unique(modpred)
}


#' @rdname find_predictors
#' @importFrom stats formula terms
#' @export
find_predictors.MixMod <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  ## TODO needs unit tests!

  if (effects == "random")
    return(find_random(x, split_nested = FALSE, component = component))

  fm <- find_formula(x, effects = "all")

  modpred.cond <- all.vars(fm$conditional[[3L]])
  modpred.zi <- all.vars(fm$zero_inflated[[2L]])
  modpred.ranef <- all.vars(fm$random[[2L]])
  modpred.ziranef <- all.vars(fm$zero_inflated_random[[2L]])

  if (length(modpred.cond) == 1 && modpred.cond == ".")
    modpred.cond <- all.vars(stats::terms(x)[[3L]])

  if (effects == "fixed" && component != "dispersion") {
    modpred.ranef <- NULL
    modpred.ziranef <- NULL
  }

  switch(
    component,
    all = unique(c(modpred.cond, modpred.zi, modpred.ranef, modpred.ziranef)),
    conditional = unique(c(modpred.cond, modpred.ranef)),
    zi = ,
    zero_inflated = unique(c(modpred.zi, modpred.ziranef)),
    NULL
  )
}


#' @importFrom stats formula terms
#' @export
find_predictors.gam <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  fm <- find_formula(x)
  if (is.list(fm)) fm <- fm[[1]]
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
find_predictors.stanmvreg <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  if (effects == "random")
    return(find_random(x, split_nested = FALSE))

  modpred <- unique(unlist(lapply(find_formula(x), function(.x) { all.vars(.x[[3L]]) })))

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (effects == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
find_predictors.felm <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "dispersion"))
      return(NULL)
  }

  modpred <- all.vars(find_formula(x)[[2L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}



#' @importFrom stats na.omit
remove_re_from_terms <- function(x, modpred, component = "conditional") {
  re <- find_random(x, split_nested = TRUE, component = component)
  if (!is_empty_string(re)) {
    pos <- match(re, modpred)
    modpred <- modpred[-stats::na.omit(pos)]
  }

  modpred
}


#' @importFrom stats terms
find_zeroinf_predictors <- function(x, component) {
  if (component %in% c("disp", "dispersion"))
    return(NULL)

  fm.cond <- find_formula(x, component = "conditional")
  fm.zi <- find_formula(x, component = "zi")

  pred.cond <- all.vars(fm.cond[[3L]])
  pred.zi <- all.vars(fm.zi[[2L]])

  if (length(pred.cond) == 1 && pred.cond == ".")
    pred.cond <- all.vars(stats::terms(x)[[3L]])

  switch(
    component,
    all = unique(c(pred.cond, pred.zi)),
    conditional = unique(pred.cond),
    zi = ,
    zero_inflated = unique(pred.zi)
  )
}