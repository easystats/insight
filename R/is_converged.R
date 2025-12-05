#' @title Convergence test for mixed effects models
#' @name is_converged
#'
#' @description `is_converged()` provides an alternative convergence
#'   test for `merMod`-objects.
#'
#' @param x A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan` or
#' `_glm`.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be.
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @return `TRUE` if convergence is fine and `FALSE` if convergence is
#'   suspicious. Additionally, the convergence value is returned as attribute.
#'   For `merMod` models, if the model is singular, convergence is determined by
#'   the optimizer's convergence code. For non-singular models where derivatives
#'   are unavailable, `FALSE` is returned and a message is printed to indicate
#'   that convergence cannot be assessed through the usual gradient-based checks.
#'
#' @section Convergence and log-likelihood:
#' Convergence problems typically arise when the model hasn't converged to a
#' solution where the log-likelihood has a true maximum. This may result in
#' unreliable and overly complex (or non-estimable) estimates and standard
#' errors.
#'
#' @section Inspect model convergence:
#' **lme4** performs a convergence-check (see `?lme4::convergence`), however, as
#' discussed [here](https://github.com/lme4/lme4/issues/120) and suggested by
#' one of the lme4-authors in [this comment](https://github.com/lme4/lme4/issues/120#issuecomment-39920269),
#' this check can be too strict. `is_converged()` (and its wrapper function,
#' `performance::check_convergence()`) thus provides an alternative convergence
#' test for `merMod`-objects.
#'
#' @section Resolving convergence issues:
#' Convergence issues are not easy to diagnose. The help page on
#' `?lme4::convergence` provides most of the current advice about how to resolve
#' convergence issues. In general, convergence issues may be addressed by one or
#' more of the following strategies: 1. Rescale continuous predictors; 2. try a
#' different optimizer; 3. increase the number of iterations; or, if everything
#' else fails, 4. simplify the model. Another clue might be large parameter
#' values, e.g. estimates (on the scale of the linear predictor) larger than 10
#' in (non-identity link) generalized linear model *might* indicate complete
#' separation, which can be addressed by regularization, e.g. penalized
#' regression or Bayesian regression with appropriate priors on the fixed
#' effects.
#'
#' @section Convergence versus Singularity:
#' Note the different meaning between singularity and convergence: singularity
#' indicates an issue with the "true" best estimate, i.e. whether the maximum
#' likelihood estimation for the variance-covariance matrix of the random effects
#' is positive definite or only semi-definite. Convergence is a question of
#' whether we can assume that the numerical optimization has worked correctly
#' or not. A convergence failure means the optimizer (the algorithm) could not
#' find a stable solution (_Bates et. al 2015_).
#'
#' For singular models (see `?lme4::isSingular`), convergence is determined
#' based on the optimizer's convergence code. If the optimizer reports
#' successful convergence (convergence code 0) for a singular model,
#' `is_converged()` returns `TRUE`. For non-singular models, in cases where the
#' gradient and Hessian are not available, `is_converged()` returns `FALSE` and
#' prints a message to indicate that convergence cannot be assessed through the
#' usual gradient-based checks. Note that `performance::check_convergence()` is
#' a wrapper around `insight::is_converged()`.
#'
#' @references
#' Bates, D., Mächler, M., Bolker, B., and Walker, S. (2015). Fitting Linear
#' Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1),
#' 1-48. \doi{10.18637/jss.v067.i01}
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' library(lme4)
#' data(cbpp)
#' set.seed(1)
#' cbpp$x <- rnorm(nrow(cbpp))
#' cbpp$x2 <- runif(nrow(cbpp))
#'
#' model <- glmer(
#'   cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
#'   data = cbpp,
#'   family = binomial()
#' )
#'
#' is_converged(model)
#'
#' @examplesIf getOption("warn") < 2L && require("glmmTMB")
#' \donttest{
#' library(glmmTMB)
#' model <- glmmTMB(
#'   Sepal.Length ~ poly(Petal.Width, 4) * poly(Petal.Length, 4) +
#'     (1 + poly(Petal.Width, 4) | Species),
#'   data = iris
#' )
#'
#' is_converged(model)
#' }
#' @export
is_converged <- function(x, tolerance = 0.001, ...) {
  UseMethod("is_converged")
}


#' @export
is_converged.default <- function(x, tolerance = 0.001, ...) {
  format_alert(sprintf(
    "`is_converged()` does not work for models of class '%s'.",
    class(x)[1]
  ))
}


#' @rdname is_converged
#' @export
is_converged.merMod <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  check_if_installed(c("Matrix", "lme4"))

  # First check for singularity
  # For singular models, if optimizer convergence code is 0, the model has
  # converged. We check singularity first because singular models may not have
  # derivatives available
  if (lme4::isSingular(x)) {
    # check if model converged based on optimizer convergence code
    converged <- isTRUE(x@optinfo$conv$opt == 0)
    if (verbose && !converged) {
      format_alert(
        "Singular model fit. Cannot assess convergence, returning `FALSE` now."
      )
    }
    # optimizer convergence code is zero is necessary for convergence
    return(structure(converged, gradient = NA_real_))
  }

  # Check if derivatives are available
  # In some cases, derivatives may not be available even for non-singular fits.
  derivs <- x@optinfo$derivs
  if (is.null(derivs) || is.null(derivs$Hessian) || is.null(derivs$gradient)) {
    if (verbose) {
      format_alert(
        "Derivatives (gradient and/or Hessian) not available. Cannot assess convergence through gradient-based checks."
      )
    }
    return(structure(FALSE, gradient = NA_real_))
  }

  relgrad <- with(derivs, Matrix::solve(Hessian, gradient))

  # copy logical value, TRUE if convergence is OK
  retval <- max(abs(relgrad)) < tolerance
  # copy convergence value
  attr(retval, "gradient") <- max(abs(relgrad))

  retval
}


#' @export
is_converged.glmmTMB <- function(x, tolerance = 0.001, ...) {
  # https://github.com/glmmTMB/glmmTMB/issues/275
  # https://stackoverflow.com/q/79110546/2094622
  isTRUE(all.equal(x$fit$convergence, 0, tolerance = tolerance)) && isTRUE(x$sdr$pdHess)
}


#' @export
is_converged.glm <- function(x, tolerance = 0.001, ...) {
  if (!is.null(x$converged)) {
    isTRUE(x$converged)
  } else if (!is.null(x$fit$converged)) {
    isTRUE(x$fit$converged)
  } else {
    NULL
  }
}


#' @export
is_converged._glm <- function(x, tolerance = 0.001, ...) {
  isTRUE(x$fit$converged)
}


#' @export
is_converged.lavaan <- function(x, tolerance = 0.001, ...) {
  check_if_installed("lavaan")
  lavaan::lavInspect(x, "converged")
}
