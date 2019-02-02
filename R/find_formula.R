#' @title Find model formula
#' @name find_formula
#'
#' @description Get model formula
#'
#' @inheritParams find_predictors
#'
#' @return A formula that describes the model, or a list of formulas (e.g., for
#'    multivariate response models, or models with zero-inflation component).
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_formula(m)
#'
#' @importFrom stats formula terms
#' @export
find_formula <- function(x, ...) {
  UseMethod("find_formula")
}


#' @export
find_formula.default <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )
}


#' @rdname find_formula
#' @export
find_formula.hurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  zeroinf_formula(x, component)
}


#' @export
find_formula.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  zeroinf_formula(x, component)
}


#' @export
find_formula.zerotrunc <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  zeroinf_formula(x, component)
}


#' @export
find_formula.clm2 <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  attr(x$location, "terms", exact = TRUE)
}


#' @rdname find_formula
#' @export
find_formula.glmmTMB <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)

  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, component = "zi")
  f.disp <- stats::formula(x, component = "disp")

  switch(
    component,
    all = compact_list(list(conditional = f.cond, zero_inflated = f.zi, dispersion = f.disp)),
    conditional = f.cond,
    zi = ,
    zero_inflated = f.zi,
    dispersion = f.disp
  )
}


#' @export
find_formula.brmsfit <- function(x, ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  ## TODO check for ZI and multivariate response models
  stats::formula(x)
}


#' @export
find_formula.MCMCglmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  fm <- x$Fixed$formula
  fmr <- x$Random$formula

  switch(
    effects,
    fixed = fm,
    random = fmr,
    compact_list(list(fixed = fm, random = fmr))
  )
}


#' @export
find_formula.lme <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  fm <- eval(x$call$fixed)
  fmr <- eval(x$call$random)

  switch(
    effects,
    fixed = fm,
    random = fmr,
    compact_list(list(fixed = fm, random = fmr))
  )
}


#' @rdname find_formula
#' @export
find_formula.MixMod <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  ## TODO fix it, once MixMod supports dispersion term
  if (component == "dispersion")
    return(NULL)

  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, type = "zi_fixed")
  f.random <- stats::formula(x, type = "random")
  f.zirandom <- stats::formula(x, type = "zi_random")

  if (effects == "fixed") {
    f.random <- NULL
    f.zirandom <- NULL
  } else if (effects == "random") {
    f.cond <- NULL
    f.zi <- NULL
  }

  switch(
    component,
    all = compact_list(list(conditional = f.cond, zero_inflated = f.zi, random = f.random, zero_inflated_random = f.zirandom)),
    conditional = compact_list(list(conditional = f.cond, random = f.random)),
    zi = ,
    zero_inflated = compact_list(list(zero_inflated = f.zi, zero_inflated_random = f.zirandom))
  )
}


#' @export
find_formula.stanmvreg <- function(x, effects = c("all", "fixed", "random"), ...) {
  # make sure we have no invalid component request
  dots <- list(...)
  if (obj_has_name(dots, "component")) {
    if (dots$component %in% c("zero_inflated", "zi", "disp", "dispersion"))
      return(NULL)
  }

  ## TODO check for ZI and multivariate response models
  stats::formula(x)
}


#' @importFrom stats as.formula
zeroinf_formula <- function(x, component) {
  if (component %in% c("disp", "dispersion"))
    return(NULL)

  f <- tryCatch(
    {stats::formula(x)},
    error = function(x) { NULL }
  )

  if (is.null(f))
    return(NULL)

  f <- trim(unlist(strsplit(deparse(f, width.cutoff = 500L), "\\|")))

  c.form <- stats::as.formula(f[1])
  if (length(f) == 2)
    zi.form <- stats::as.formula(paste0("~", f[2]))
  else
    zi.form <- NULL

  switch(
    component,
    all = compact_list(list(conditional = c.form, zero_inflated = zi.form)),
    conditional = c.form,
    zi = ,
    zero_inflated = zi.form
  )
}