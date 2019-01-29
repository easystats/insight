#' @title Get term names of model predictors
#' @name model_predictors
#'
#' @description to do...
#'
#' @param x A fitted model.
#' @param parms Should predictor variables for fixed effects, random effects
#'    or both be returned? Only applies to mixed models.
#' @param zi Logical, if \code{TRUE} and model has a zero-inflation-formula,
#'    the variable(s) used in this formula are also returned.
#' @param disp Logical, if \code{TRUE} and model is of class \code{glmmTMB} and
#'    has a dispersion-formula, the variable(s) used in the dispersion-formula
#'    are also returned.
#' @param ... Currently not used.
#'
#' @return The name(s) of the predictor variables from \code{x} as character vector.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' model_predictors(m)
#'
#' @export
model_predictors <- function(x, ...) {
  UseMethod("model_predictors")
}


#' @rdname model_predictors
#' @importFrom stats formula terms
#' @export
model_predictors.default <- function(x, parms = c("fixed", "random", "all"), ...) {
  parms <- match.arg(parms)

  if (parms == "random")
    return(re_terms(x))

  fm <- stats::formula(x)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (parms == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @export
#' @importFrom stats terms
model_predictors.clm2 <- function(x, ...) {
  fm <- attr(x$location, "terms", exact = TRUE)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}


#' @rdname model_predictors
#' @importFrom stats formula terms
#' @export
model_predictors.glmmTMB <- function(x, parms = c("fixed", "random", "all"), zi = FALSE, disp = FALSE, ...) {
  parms <- match.arg(parms)

  if (parms == "random")
    return(re_terms(x))

  fm <- stats::formula(x)
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (parms == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  # add variables from zero-inflation
  if (isTRUE(zi)) {
    dp <- tryCatch(
      {all.vars(x$modelInfo$allForm$ziformula[[2L]])},
      error = function(x) { NULL}
    )

    if (!is.null(dp)) modpred <- c(modpred, dp)
  }

  # for glmmtmb, check dispersion formula
  if (isTRUE(disp)) {
    dp <- tryCatch(
      {all.vars(x$modelInfo$allForm$dispformula[[2L]])},
      error = function(x) { NULL}
    )

    if (!is.null(dp)) modpred <- c(modpred, dp)
  }

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
model_predictors.brmsfit <- function(x, parms = c("fixed", "random", "all"), ...) {
  parms <- match.arg(parms)

  if (parms == "random")
    return(re_terms(x))

  fm <- stats::formula(x)

  if (!is.null(fm$responses)) {
    modpred <- unique(unlist(lapply(fm$forms, function(.x) { all.vars(stats::formula(.x)[[3L]]) })))
  } else {
    modpred <- all.vars(fm$formula[[3L]])
  }

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (parms == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @export
model_predictors.MCMCglmm <- function(x, parms = c("fixed", "random", "all"), ...) {
  parms <- match.arg(parms)

  fm <- x$Fixed
  fixed.modpred <- all.vars(fm$formula[[3L]])

  fmr <- x$Random
  random.modpred <- all.vars(fmr$formula[[2L]])

  modpred <- switch(
    parms,
    fixed = fixed.modpred,
    random = random.modpred,
    c(fixed.modpred, random.modpred)
  )

  unique(modpred)
}


#' @rdname model_predictors
#' @importFrom stats formula terms
#' @export
model_predictors.MixMod <- function(x, parms = c("fixed", "random", "all"), zi = FALSE, ...) {
  parms <- match.arg(parms)

  fm <- stats::formula(x)
  fixed.modpred <- all.vars(fm[[3L]])

  random.modpred <- x$id_name
  dvrandom <- all.vars(stats::formula(x, type = "random")[[2L]])
  if (!is_empty_string(dvrandom)) random.modpred <- c(random.modpred, dvrandom)

  if (isTRUE(zi)) {
    dvzi <- all.vars(stats::formula(x, type = "zi_fixed")[[2L]])
    if (!is_empty_string(dvzi)) fixed.modpred <- c(fixed.modpred, dvzi)
  }

  modpred <- switch(
    parms,
    fixed = fixed.modpred,
    random = random.modpred,
    c(fixed.modpred, random.modpred)
  )

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
model_predictors.gam <- function(x, ...) {
  fm <- stats::formula(x)
  if (is.list(fm)) fm <- fm[[1]]
  modpred <- all.vars(fm[[3L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
model_predictors.stanmvreg <- function(x, parms = c("fixed", "random", "all"), ...) {
  parms <- match.arg(parms)

  if (parms == "random")
    return(re_terms(x))

  fm <- stats::formula(x)
  modpred <- unique(unlist(lapply(fm, function(.x) { all.vars(.x[[3L]]) })))

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  # remove random effects from formula
  if (parms == "fixed")
    modpred <- remove_re_from_terms(x, modpred)

  unique(modpred)
}


#' @importFrom stats formula terms
#' @export
model_predictors.felm <- function(x, ...) {
  fm <- stats::formula(x)
  modpred <- all.vars(fm[[2L]])

  if (length(modpred) == 1 && modpred == ".")
    modpred <- all.vars(stats::terms(x)[[3L]])

  unique(modpred)
}



remove_re_from_terms <- function(x, modpred) {
  re <- re_terms(x)
  if (!is_empty_string(re)) {
    re <- unique(trim(unlist(strsplit(re, ":", fixed = TRUE))))
    pos <- match(re, modpred)
    modpred <- modpred[-pos]
  }

  modpred
}