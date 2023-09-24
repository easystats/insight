#' @title Convergence test for mixed effects models
#' @name is_converged
#'
#' @description `is_converged()` provides an alternative convergence
#'   test for `merMod`-objects.
#'
#' @param x A `merMod` or `glmmTMB`-object.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be.
#' @param ... Currently not used.
#'
#' @return `TRUE` if convergence is fine and `FALSE` if convergence
#'   is suspicious. Additionally, the convergence value is returned as attribute.
#'
#' @section Convergence and log-likelihood:
#' Convergence problems typically arise when the model hasn't converged to a
#' solution where the log-likelihood has a true maximum. This may result in
#' unreliable and overly complex (or non-estimable) estimates and standard
#' errors.
#'
#' @section Inspect model convergence:
#' {lme4} performs a convergence-check (see `?lme4::convergence`), however, as
#' discussed [here](https://github.com/lme4/lme4/issues/120) and suggested by
#' one of the lme4-authors in [this comment](https://github.com/lme4/lme4/issues/120#issuecomment-39920269),
#' this check can be too strict. `is_converged()` thus provides an alternative
#' convergence test for `merMod`-objects.
#'
#' @section Resolving convergence issues:
#' Convergence issues are not easy to diagnose. The help page on `?lme4::convergence`
#' provides most of the current advice about how to resolve convergence issues.
#' Another clue might be large parameter values, e.g. estimates (on the scale of
#' the linear predictor) larger than 10 in (non-identity link) generalized linear
#' model *might* indicate complete separation, which can be addressed by
#' regularization, e.g. penalized regression or Bayesian regression with
#' appropriate priors on the fixed effects.
#'
#' @section Convergence versus Singularity:
#' Note the different meaning between singularity and convergence: singularity
#' indicates an issue with the "true" best estimate, i.e. whether the maximum
#' likelihood estimation for the variance-covariance matrix of the random effects
#' is positive definite or only semi-definite. Convergence is a question of
#' whether we can assume that the numerical optimization has worked correctly
#' or not.
#'
#' @examplesIf require("lme4", quietly = TRUE)
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
  format_alert(sprintf("`is_converged()` does not work for models of class '%s'.", class(x)[1]))
}


#' @export
is_converged.merMod <- function(x, tolerance = 0.001, ...) {
  insight::check_if_installed("Matrix")

  relgrad <- with(x@optinfo$derivs, Matrix::solve(Hessian, gradient))

  # copy logical value, TRUE if convergence is OK
  retval <- max(abs(relgrad)) < tolerance
  # copy convergence value
  attr(retval, "gradient") <- max(abs(relgrad))

  retval
}


#' @export
is_converged.glmmTMB <- function(x, ...) {
  # https://github.com/glmmTMB/glmmTMB/issues/275
  isTRUE(x$sdr$pdHess)
}
