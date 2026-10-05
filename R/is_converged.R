#' @title Convergence test for mixed effects and Stan models
#' @name is_converged
#'
#' @description `is_converged()` provides an alternative convergence
#'   test for `merMod`-objects. For models fitted with Stan (`stanfit`,
#'   `brmsfit` and `stanreg`), it checks the diagnostics of the sampler.
#'
#' @param x A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan`,
#' `_glm`, `stanfit`, `brmsfit` or `stanreg`.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be. Not
#'   used for Stan models.
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @return `TRUE` if convergence is fine and `FALSE` if convergence is
#'   suspicious or cannot be assessed. For `merMod` models, the convergence
#'   value is returned as attribute `gradient`. If the model is singular,
#'   convergence is determined by the optimizer's convergence code. For
#'   non-singular models where derivatives are unavailable, `FALSE` is returned
#'   and a message is printed to indicate that convergence cannot be assessed
#'   through the usual gradient-based checks.
#'
#'   For Stan models, the attribute `diagnostics` is a data frame with the
#'   value, the threshold and the result of each check. For Stan models whose
#'   convergence cannot be assessed, `FALSE` is returned without this attribute,
#'   and a message gives the reason: models without MCMC draws from the NUTS
#'   sampler (for example, models fitted with variational inference or
#'   optimization), models without draws after warmup, and models fitted with
#'   `brms::brm_multiple()`, whose chains come from different data sets (such
#'   as imputed ones).
#'
#' @section Stan models:
#' For models fitted with Stan, `is_converged()` returns `FALSE` if at least one
#' of the checks below fails. The checks and thresholds are those of the
#' warnings that *rstan* gives after sampling, and the values are computed with
#' functions from *rstan*:
#'
#' - Divergent transitions after warmup (`rstan::get_num_divergent()`): the
#'   check fails if there is at least one.
#' - Transitions after warmup that reach the maximum treedepth
#'   (`rstan::get_num_max_treedepth()`): the check fails if there is at least
#'   one.
#' - E-BFMI (`rstan::get_bfmi()`): the check fails if at least one chain has
#'   a value below 0.2. This is the E-BFMI of `rstan::check_hmc_diagnostics()`,
#'   which can differ from the warning that *rstan* prints after sampling.
#' - R-hat (`rstan::Rhat()`): the check fails if the largest value over all
#'   parameters is above 1.05.
#' - Bulk and tail effective sample size (`rstan::ess_bulk()` and
#'   `rstan::ess_tail()`): each check fails if the smallest value over all
#'   parameters is below 100 times the number of chains.
#'
#' Missing values (for example, R-hat of a constant parameter) are ignored. Stan
#' also prints messages about rejected proposals ("exception thrown") during
#' sampling. These messages are not checked, because the model object does not
#' store them.
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
#'
#' @examplesIf all(check_if_installed(c("curl", "brms", "rstan", "httr2"), quietly = TRUE)) && curl::has_internet()
#' \donttest{
#' # a model fitted with brms
#' model <- download_model("brms_1")
#' result <- is_converged(model)
#' result
#' attributes(result)$diagnostics
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


# Stan models ------------------------------------------------------------------

#' @rdname is_converged
#' @export
is_converged.stanfit <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  .is_converged_stan(x, verbose = verbose)
}


#' @export
is_converged.brmsfit <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  if (inherits(x, "brmsfit_multiple")) {
    return(.is_converged_stan_not_assessed(
      paste(
        "The chains of models fitted with `brm_multiple()` come from different",
        "data sets (such as imputed ones). Check the convergence of the chains",
        "of each data set separately, as shown in",
        "`vignette(\"brms_missings\", package = \"brms\")`."
      ),
      verbose
    ))
  }
  if (!.is_stan_sampling(x)) {
    return(.is_converged_stan_not_assessed(.stan_no_nuts_reason, verbose))
  }
  .is_converged_stan(x$fit, verbose = verbose)
}


#' @export
is_converged.stanreg <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  if (!.is_stan_sampling(x)) {
    return(.is_converged_stan_not_assessed(.stan_no_nuts_reason, verbose))
  }
  .is_converged_stan(x$stanfit, verbose = verbose)
}


# brmsfit and stanreg objects store the algorithm; a missing value is treated
# as sampling, the default of both packages
.is_stan_sampling <- function(x) {
  is.null(x$algorithm) || identical(x$algorithm, "sampling")
}


.stan_no_nuts_reason <- "The model has no MCMC draws from the NUTS sampler."


# checks and thresholds follow the warnings that rstan gives after sampling,
# see `rstan:::throw_sampler_warnings()` and `rstan::check_hmc_diagnostics()`
.is_converged_stan <- function(x, verbose = TRUE) {
  check_if_installed("rstan")

  # rstan stores the algorithm "NUTS". brms with the cmdstanr backend stores
  # the algorithm "hmc" and the engine "nuts" (`brms:::read_csv_as_stanfit()`)
  stan_args <- .safe(x@stan_args[[1]], list())
  is_nuts <- identical(stan_args$algorithm, "NUTS") ||
    (identical(stan_args$algorithm, "hmc") && identical(stan_args$engine, "nuts"))
  if (!is_nuts) {
    return(.is_converged_stan_not_assessed(.stan_no_nuts_reason, verbose))
  }

  # `as.array()` has length 0 if the model has no draws after warmup
  # (`numeric(0)` if rstan stored no samples, an array with no iterations
  # otherwise)
  draws <- as.array(x)
  if (!length(draws)) {
    return(.is_converged_stan_not_assessed(
      "The model has no draws after warmup.",
      verbose
    ))
  }
  n_chains <- dim(draws)[2]

  # largest or smallest value, or NA if all values are NA
  .na_or <- function(values, fun) {
    values <- values[!is.na(values)]
    if (length(values)) fun(values) else NA_real_
  }

  diagnostics <- data.frame(
    Diagnostic = c(
      "Divergences",
      "Treedepth",
      "E-BFMI",
      "Rhat",
      "ESS_bulk",
      "ESS_tail"
    ),
    Value = c(
      rstan::get_num_divergent(x),
      rstan::get_num_max_treedepth(x),
      .na_or(.safe(rstan::get_bfmi(x), NA_real_), min),
      .na_or(apply(draws, 3, rstan::Rhat), max),
      .na_or(apply(draws, 3, rstan::ess_bulk), min),
      .na_or(apply(draws, 3, rstan::ess_tail), min)
    ),
    Threshold = c(0, 0, 0.2, 1.05, 100 * n_chains, 100 * n_chains),
    stringsAsFactors = FALSE
  )

  # divergences, treedepth and Rhat must not exceed the threshold, E-BFMI and
  # ESS must not fall below it. Checks with a missing value pass, as in rstan.
  upper <- diagnostics$Diagnostic %in% c("Divergences", "Treedepth", "Rhat")
  diagnostics$Passed <- is.na(diagnostics$Value) |
    ifelse(
      upper,
      diagnostics$Value <= diagnostics$Threshold,
      diagnostics$Value >= diagnostics$Threshold
    )

  converged <- all(diagnostics$Passed)

  if (verbose && !converged) {
    failed <- diagnostics[!diagnostics$Passed, ]
    # counts as integers, other values with three significant digits, or
    # more if the rounded value would look like the threshold
    .format_value <- function(value, diagnostic, threshold = NULL) {
      if (diagnostic %in% c("Divergences", "Treedepth")) {
        return(format(as.integer(value)))
      }
      out <- format(value, digits = 3)
      if (!is.null(threshold) && out == format(threshold, digits = 3)) {
        out <- format(value, digits = 6)
      }
      out
    }
    msg <- sprintf(
      "%s: %s (%s, threshold %s)",
      failed$Diagnostic,
      c(
        Divergences = "divergent transitions after warmup",
        Treedepth = "transitions at the maximum treedepth",
        `E-BFMI` = "low E-BFMI in at least one chain",
        Rhat = "R-hat too high",
        ESS_bulk = "bulk ESS too low",
        ESS_tail = "tail ESS too low"
      )[failed$Diagnostic],
      vapply(
        seq_len(nrow(failed)),
        function(i) {
          .format_value(
            failed$Value[i],
            failed$Diagnostic[i],
            failed$Threshold[i]
          )
        },
        character(1)
      ),
      vapply(
        seq_len(nrow(failed)),
        function(i) .format_value(failed$Threshold[i], failed$Diagnostic[i]),
        character(1)
      )
    )
    format_alert(
      "The model has not converged. These checks failed:",
      paste0("- ", msg)
    )
  }

  structure(converged, diagnostics = diagnostics)
}


# as for other models, `FALSE` is returned if convergence cannot be assessed
# (see https://github.com/easystats/insight/pull/1154)
.is_converged_stan_not_assessed <- function(reason, verbose = TRUE) {
  if (verbose) {
    format_alert(
      paste("Convergence cannot be assessed.", reason, "Returning `FALSE`.")
    )
  }
  FALSE
}
