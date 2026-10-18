#' @title Convergence test for mixed effects, Stan and Cox models
#' @name is_converged
#'
#' @description `is_converged()` provides an alternative convergence
#'   test for `merMod`-objects. For models fitted with Stan (`stanfit`,
#'   `brmsfit`, `stanreg` and `CmdStanMCMC`), it checks the diagnostics of the
#'   sampler. For `coxph` models, it recomputes the checks of the *survival*
#'   package.
#'
#' @param x A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan`,
#' `_glm`, `stanfit`, `brmsfit`, `stanreg`, `CmdStanMCMC` or `coxph`.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be. Not
#'   used for Stan and `coxph` models.
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
#'   For `coxph` models, the attribute `diagnostics` is a data frame with the
#'   result of each check. If convergence cannot be assessed, `FALSE` is
#'   returned without this attribute, and, if `verbose = TRUE`, a message gives
#'   the reason.
#'
#' @section Stan models:
#' For models fitted with Stan, `is_converged()` returns `FALSE` if at least one
#' of the checks below fails. The checks and thresholds are those of the
#' warnings that *rstan* gives after sampling. For `stanfit`, `brmsfit` and
#' `stanreg` models, the values are computed with functions from *rstan*:
#'
#' - Divergent transitions after warmup (`rstan::get_num_divergent()`): the
#'   check fails if there is at least one.
#' - Transitions after warmup that reach the maximum treedepth stored in the
#'   model (`rstan::get_sampler_params()`): the check fails if there is at
#'   least one. The maximum is read from the `control` settings. Models from
#'   `rstan::read_stan_csv()` store it outside `control`, and
#'   `is_converged()` reads it from there. For these models,
#'   `rstan::get_num_max_treedepth()` counts against the default maximum of
#'   10. If the model stores no maximum, 10 is used.
#' - E-BFMI (`rstan::get_bfmi()`): the check fails if at least one chain has
#'   a value below 0.2. This is the E-BFMI of `rstan::check_hmc_diagnostics()`,
#'   which can differ from the warning that *rstan* prints after sampling.
#' - R-hat (`rstan::Rhat()`): the check fails if the largest value over all
#'   parameters is above 1.05.
#' - Bulk and tail effective sample size (`rstan::ess_bulk()` and
#'   `rstan::ess_tail()`): each check fails if the smallest value over all
#'   parameters is below 100 times the number of chains with draws after
#'   warmup.
#'
#' For `CmdStanMCMC` models from *cmdstanr*, the same checks and thresholds
#' are used. Divergent transitions, transitions at the maximum treedepth and
#' E-BFMI come from the `$diagnostic_summary()` method of the model, and R-hat
#' and effective sample size from its `$summary()` method. The E-BFMI warning
#' of *cmdstanr* uses a threshold of 0.3, not 0.2.
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
#' @section Cox proportional hazards models:
#' *survival* warns about convergence problems when a `coxph` model is fitted,
#' but does not store the warnings in the model object. For `coxph` models,
#' `is_converged()` therefore computes the two checks of *survival* again,
#' with the `iter.max`, `eps` and `toler.inf` values of the model call:
#'
#' - Iterations: the check fails if the model did not converge within
#'   `iter.max` iterations ("Ran out of iterations and did not converge").
#' - Infinite coefficient: the check fails for a coefficient if the
#'   log-likelihood converged before the coefficient did ("Loglik converged
#'   before variable ...; coefficient may be infinite", or "beta may be
#'   infinite" for counting-process data). This happens, for
#'   example, if a factor level has no events. As in *survival*, this check
#'   runs only if the first check passed.
#'
#' `is_converged()` returns `FALSE` if a check fails. The attribute
#' `diagnostics` is a data frame with the value, the threshold and the result
#' of each check, with one row for each coefficient for the second check. The
#' `tolerance` argument is not used for `coxph` models.
#'
#' Convergence cannot be assessed, and `FALSE` is returned, for penalized
#' models (with `frailty()`, `ridge()` or `pspline()` terms), for models with
#' `ties = "exact"`, for models with `tt()` terms, for models with `iter.max`
#' of 1 or less, for right-censored and multi-state models fitted with
#' `y = FALSE`, for models without events, for model objects without a call,
#' and if the control arguments of the model call or the score residuals cannot
#' be computed. Convergence also cannot be assessed if the model call does not
#' write the formula as a `~` expression, for example
#' `coxph(f, data = d, iter.max = n)`, and a control argument refers to a
#' variable. The control arguments are evaluated
#' in the environment of the formula, which can differ from the environment in
#' which the model was fitted, for example inside a function. This also applies
#' to models fitted in top-level code, where the two environments are the same.
#' Single-state counting-process models fitted with `y = FALSE`
#' are assessed, because their checks use only the stored score vector and
#' convergence flag. A model without events and without predictors returns
#' `TRUE`, as other null models.
#'
#' Models from `survival::clogit()` are assessed as other `coxph` models. The
#' default method of `clogit()` is `"exact"`, so convergence can only be
#' assessed for models fitted with `method = "efron"` or `method = "breslow"`
#' (or `"approximate"`). Objects of other classes that inherit from `coxph`,
#' for example from `survey::svycoxph()`, are not supported: `NULL` is
#' returned with a message (no message if `verbose = FALSE`).
#'
#' For models with right-censored data, the score residuals are computed from
#' the data of the model call, unless the model was fitted with `model = TRUE`,
#' or with `x = TRUE` for single-state models. If these data were removed after
#' the model was fitted,
#' convergence cannot be assessed. If the predictor data were changed, so that
#' the linear predictor computed from them differs from the one of the model,
#' convergence cannot be assessed either. Single-state models with a
#' `strata()` term are also fitted again from these data, at the coefficients
#' of the model and without iterations. If the log-likelihood of this fit
#' differs from the one of the model, for example because the observations
#' were moved to other strata, convergence cannot be assessed. For multi-state
#' models, the linear predictor is compared within each transition. A change
#' that adds the same constant to all values of a predictor is not detected,
#' but it leaves the score residuals the same. The check is not done for
#' multi-state models in which transitions share a baseline hazard, or that
#' were fitted with `x = TRUE`. Strata that are relabeled without a change of
#' grouping are not detected, but they leave the result the same. Other
#' changes that stay undetected can give a wrong result: in multi-state models,
#' changes to a `strata()` or `istate` variable, and changes to the data of
#' models without the check. In these cases, refit the model, or fit it with
#' `model = TRUE`, which avoids them.
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
is_converged.default <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  if (verbose) {
    format_alert(sprintf(
      "`is_converged()` does not work for models of class '%s'.",
      class(x)[1]
    ))
  }
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
  } else if (is.null(x$fit$converged)) {
    NULL
  } else {
    isTRUE(x$fit$converged)
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


#' @export
is_converged.CmdStanMCMC <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  check_if_installed("cmdstanr")

  # cmdstanr stores the algorithm "hmc" and the engine "nuts", or the
  # algorithm "fixed_param"
  metadata <- x$metadata()
  is_nuts <- identical(metadata$algorithm, "hmc") &&
    identical(metadata$engine, "nuts")
  if (!is_nuts) {
    return(.is_converged_stan_not_assessed(.stan_no_nuts_reason, verbose))
  }
  if (!isTRUE(metadata$iter_sampling > 0)) {
    return(.is_converged_stan_not_assessed(
      "The model has no draws after warmup.",
      verbose
    ))
  }

  # one value per chain for the sampler diagnostics, one row per variable in
  # the summary. cmdstanr warns if E-BFMI cannot be computed (for example from
  # fewer than three draws); the missing value then passes, as in rstan
  sampler <- .stan_quiet_unless(
    x$diagnostic_summary(quiet = TRUE),
    verbose
  )
  draws_summary <- .stan_quiet_unless(
    x$summary(NULL, "rhat", "ess_bulk", "ess_tail"),
    verbose
  )
  .stan_diagnostics(
    values = c(
      sum(sampler$num_divergent),
      sum(sampler$num_max_treedepth),
      .stan_na_or(sampler$ebfmi, min),
      .stan_na_or(draws_summary$rhat, max),
      .stan_na_or(draws_summary$ess_bulk, min),
      .stan_na_or(draws_summary$ess_tail, min)
    ),
    # `x$num_chains()` also counts chains that failed and have no draws
    n_chains = dim(x$draws("lp__", format = "draws_array"))[2],
    verbose = verbose
  )
}


# warnings from cmdstanr and posterior are shown only if `verbose = TRUE`
.stan_quiet_unless <- function(expr, verbose) {
  if (verbose) expr else suppressWarnings(expr)
}


# other cmdstanr fits, for example from `$variational()` or `$optimize()`,
# have no MCMC draws
#' @export
is_converged.CmdStanFit <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  .is_converged_stan_not_assessed(.stan_no_nuts_reason, verbose)
}


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
  .stan_diagnostics(
    values = c(
      rstan::get_num_divergent(x),
      .stan_treedepth_hits(x, stan_args),
      .stan_na_or(.safe(rstan::get_bfmi(x), NA_real_), min),
      .stan_na_or(apply(draws, 3, rstan::Rhat), max),
      .stan_na_or(apply(draws, 3, rstan::ess_bulk), min),
      .stan_na_or(apply(draws, 3, rstan::ess_tail), min)
    ),
    n_chains = dim(draws)[2],
    verbose = verbose
  )
}


# largest or smallest value, or NA if all values are NA
.stan_na_or <- function(values, fun) {
  values <- values[!is.na(values)]
  if (length(values)) fun(values) else NA_real_
}


# the six checks for the values of divergences, treedepth hits, E-BFMI, R-hat,
# bulk ESS and tail ESS, with the `diagnostics` attribute and the alert
.stan_diagnostics <- function(values, n_chains, verbose = TRUE) {
  diagnostics <- data.frame(
    Diagnostic = c(
      "Divergences",
      "Treedepth",
      "E-BFMI",
      "Rhat",
      "ESS_bulk",
      "ESS_tail"
    ),
    Value = values,
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


# number of transitions after warmup at the maximum treedepth. rstan reads the
# maximum only from `control` (`rstan:::get_treedepth_threshold()`, default
# 10), but `rstan::read_stan_csv()` stores it as `max_depth` or
# `max_treedepth` outside `control`. A stored limit that is not one finite
# number is skipped, because a missing limit would make the check pass
.stan_treedepth_hits <- function(x, stan_args) {
  max_depth <- 10
  for (limit in list(
    stan_args[["control"]][["max_treedepth"]],
    stan_args[["max_depth"]],
    stan_args[["max_treedepth"]]
  )) {
    if (is.numeric(limit) && length(limit) == 1 && is.finite(limit)) {
      max_depth <- limit
      break
    }
  }
  sampler_params <- rstan::get_sampler_params(x, inc_warmup = FALSE)
  treedepth <- unlist(lapply(sampler_params, function(sp) sp[, "treedepth__"]))
  sum(treedepth >= max_depth)
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


# the checks are those that `survival:::coxph.fit()` (right-censored data) and
# `survival:::agreg.fit()` (counting-process data) apply after the fit, and
# that give the warnings "Ran out of iterations and did not converge" and
# "Loglik converged before variable ...; coefficient (or beta) may be infinite"
#' @rdname is_converged
#' @export
is_converged.coxph <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  # survival::clogit() stores the call of coxph() that it makes. Objects of
  # other classes that inherit from "coxph" (for example from
  # survey::svycoxph(), which stores its own call) go to the default method.
  if (!class(x)[1] %in% c("coxph", "coxphms", "coxph.penal", "coxph.null", "clogit")) {
    return(NextMethod())
  }
  check_if_installed("survival")

  # a null model has no coefficients that could diverge
  if (.coxph_is_null(x)) {
    return(structure(TRUE, diagnostics = .coxph_diagnostics()))
  }
  reason <- .coxph_not_assessed_reason(x)
  if (!is.null(reason)) {
    return(.is_converged_coxph_not_assessed(reason, verbose))
  }
  control <- .coxph_control(x)
  if (is.character(control)) {
    return(.is_converged_coxph_not_assessed(control, verbose))
  }
  # survival applies no check for `iter.max` <= 1
  if (control$iter.max <= 1) {
    return(.is_converged_coxph_not_assessed(
      "The model was fitted with `iter.max` <= 1, so survival applies no checks.",
      verbose
    ))
  }
  # a model with events has at least one iteration
  if (x$iter < 1) {
    return(.is_converged_coxph_not_assessed(
      "The model object has no valid number of iterations.",
      verbose
    ))
  }

  # coxph() sends right-censored responses to coxph.fit() and all other
  # responses to agreg.fit(). Without the response (`y = FALSE`), only
  # single-state counting-process models get here, see
  # .coxph_not_assessed_reason().
  y_type <- attr(x$y, "type")
  counting <- is.null(y_type) || !grepl("right", y_type, fixed = TRUE)

  # check (a): coxph.fit() returns one iteration more than `iter.max` if it
  # ran out of iterations, agreg.fit() stores a convergence flag
  if (counting) {
    convergence <- unname(x$info["convergence"])
    if (length(convergence) != 1 || is.na(convergence)) {
      return(.is_converged_coxph_not_assessed(
        "The model object has no convergence flag.",
        verbose
      ))
    }
    iterations_passed <- convergence == 0
  } else {
    iterations_passed <- x$iter <= control$iter.max
  }
  diagnostics <- .coxph_diagnostics(
    Diagnostic = "Iterations",
    Parameter = NA_character_,
    Value = x$iter,
    Threshold = control$iter.max,
    Passed = iterations_passed
  )

  # check (b), only if check (a) passed, as in survival
  if (iterations_passed) {
    infinite <- .coxph_check_infinite(x, control, counting)
    if (is.character(infinite)) {
      return(.is_converged_coxph_not_assessed(infinite, verbose))
    }
    diagnostics <- rbind(diagnostics, infinite)
  }

  converged <- all(diagnostics$Passed)

  if (verbose && !converged) {
    .coxph_alert(diagnostics, control$iter.max)
  }

  structure(converged, diagnostics = diagnostics)
}


# a null model fitted without events has class "coxph", not "coxph.null", and
# no coefficients and no terms. A null model from clogit() is handled below.
.coxph_is_null <- function(x) {
  term_labels <- attr(x$terms, "term.labels")
  # clogit() replaces the class "coxph.null" of a model with only `strata()`
  # terms, which has no coefficients
  only_strata <- all(grepl("^(survival::)?strata\\(", term_labels))
  inherits(x, "coxph.null") ||
    (!is.null(x$coefficients) && !length(x$coefficients) && !length(term_labels)) ||
    (inherits(x, "clogit") && is.null(x$coefficients) && only_strata)
}


# check (b): the diagnostics, or the reason why the check cannot be computed
.coxph_check_infinite <- function(x, control, counting) {
  # the score residuals of right-censored models are computed from the data
  # object of the call, unless the model frame (`model = TRUE`) or the model
  # matrix (`x = TRUE`) is stored. `[[` because `$` would match `xlevels`
  # for `x`.
  if (
    !counting &&
      is.null(x[["model"]]) &&
      is.null(x[["x"]]) &&
      .coxph_data_changed(x, control)
  ) {
    return(paste(
      "The predictor data changed after the model was fitted.",
      "Refit the model, or fit it with `model = TRUE`."
    ))
  }
  infinite <- tryCatch(
    .coxph_infinite(x, control, counting),
    error = function(e) e
  )
  if (inherits(infinite, "error")) {
    reason <- paste0(
      "The score vector could not be computed (",
      conditionMessage(infinite),
      ")."
    )
    # the hint only if the data cannot be read: a "not found" error does not
    # always mean that the data are gone
    if (
      is.null(x[["model"]]) &&
        grepl("not found", conditionMessage(infinite), fixed = TRUE) &&
        is.null(tryCatch(stats::model.frame(x), error = function(e) NULL))
    ) {
      reason <- paste(
        reason,
        "If the data of the model are no longer available, refit the model with `model = TRUE`."
      )
    }
    return(reason)
  }
  # for example, if the variance matrix has missing values
  if (anyNA(infinite$Value[!is.na(stats::coef(x))])) {
    return("The check for infinite coefficients gives missing values.")
  }
  infinite
}


# one alert that names each failed check
.coxph_alert <- function(diagnostics, iter_max) {
  msg <- NULL
  if (!diagnostics$Passed[1]) {
    msg <- sprintf(
      "Iterations: the model did not converge within `iter.max` = %i iterations",
      as.integer(iter_max)
    )
  }
  flagged <- diagnostics$Parameter[
    diagnostics$Diagnostic == "Infinite coefficient" & !diagnostics$Passed
  ]
  if (length(flagged)) {
    msg <- c(
      msg,
      sprintf(
        "Infinite coefficient: the log-likelihood converged before %s, so the %s may be infinite",
        toString(flagged),
        ngettext(length(flagged), "coefficient", "coefficients")
      )
    )
  }
  format_alert(
    "The model has not converged. These checks failed:",
    paste0("- ", msg)
  )
}


# the reason why the checks cannot be recomputed, or NULL
.coxph_not_assessed_reason <- function(x) {
  if (is.null(x$iter) || !length(x$coefficients)) {
    return("The model object has no iterations or coefficients.")
  }
  # penalized models store the outer and inner iterations
  if (.coxph_is_penalized(x)) {
    return(paste(
      "The checks do not apply to penalized models, for example models",
      "with `frailty()`, `ridge()` or `pspline()` terms."
    ))
  }
  if (length(x$iter) != 1 || is.na(x$iter)) {
    return("The model object has no valid number of iterations.")
  }
  # without events, survival fits no coefficients and gives no warning
  if (isTRUE(x$nevent == 0)) {
    return("The model has no events.")
  }
  # with `ties = "exact"`, survival gives no score residuals for right-censored
  # data and stores no score vector or convergence flag for counting-process
  # data, so the checks cannot be recomputed
  if (!isTRUE(x$method %in% c("efron", "breslow"))) {
    return(paste(
      "The checks can only be recomputed for the Efron and Breslow",
      "approximations of ties."
    ))
  }
  # unless the model was fitted with `x = TRUE`, survival cannot compute the
  # score residuals of models with `tt()` terms; the checks are not recomputed
  # for these models
  if (!is.null(attr(x$terms, "specials")$tt)) {
    return("The checks cannot be recomputed for models with `tt()` terms.")
  }
  if (is.null(x$call)) {
    return("The model has no call, so the control arguments of the fit are unknown.")
  }
  if (.coxph_no_response(x)) {
    return("The model has no response. Refit the model with `y = TRUE`.")
  }
  NULL
}


# a penalized clogit() model has class c("clogit", "coxph") and stores the
# penalized terms in `pterms`
.coxph_is_penalized <- function(x) {
  inherits(x, "coxph.penal") || !is.null(x[["pterms"]])
}


# without the response, the checks of agreg.fit() can still be recomputed
# from the stored score vector (`first`) and convergence flag (`info`) of a
# single-state counting-process model. Right-censored models store neither.
.coxph_no_response <- function(x) {
  is.null(attr(x$y, "type")) &&
    (inherits(x, "coxphms") || is.null(x$first) || is.null(x$info))
}


.coxph_diagnostics <- function(
  Diagnostic = character(0),
  Parameter = character(0),
  Value = numeric(0),
  Threshold = numeric(0),
  Passed = logical(0)
) {
  data.frame(
    Diagnostic = Diagnostic,
    Parameter = Parameter,
    Value = as.numeric(Value),
    Threshold = as.numeric(Threshold),
    Passed = Passed,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


# one row for each coefficient: `infs` and its bound, as in coxph.fit() or
# agreg.fit(). For right-censored data, the score vector is recomputed from
# the score residuals.
.coxph_infinite <- function(x, control, counting) {
  coefs <- stats::coef(x)
  if (counting) {
    u <- x$first
  } else {
    score <- as.matrix(stats::residuals(x, type = "score", weighted = TRUE))
    # with `na.action = na.exclude`, the residuals have NA rows for the
    # observations that the fit did not use
    if (inherits(x$na.action, "exclude")) {
      score <- score[-x$na.action, , drop = FALSE]
    }
    u <- colSums(score)
  }
  # the robust variance replaces `var` and the model-based variance is kept as
  # `naive.var`; the fitters use the model-based variance
  if (is.null(x$naive.var)) {
    v <- x$var
  } else {
    v <- x$naive.var
  }
  infs <- abs(drop(u %*% v))
  if (counting) {
    threshold <- control$toler.inf * (1 + abs(coefs))
  } else {
    threshold <- pmax(control$eps, control$toler.inf * abs(coefs))
  }
  flagged <- !is.na(coefs) & (!is.finite(u) | infs > threshold)
  flagged[is.na(flagged)] <- FALSE
  .coxph_diagnostics(
    Diagnostic = rep("Infinite coefficient", length(coefs)),
    Parameter = names(coefs),
    Value = infs,
    Threshold = threshold,
    Passed = !flagged
  )
}


# TRUE if the linear predictor, computed again from the data object of the
# call, differs from the one of the fit, or if the strata changed. If the data
# cannot be read, FALSE is returned, and check (b) gives the error.
.coxph_data_changed <- function(x, control = .coxph_control(x)) {
  mf <- tryCatch(stats::model.frame(x), error = function(e) NULL)
  mm <- tryCatch(stats::model.matrix(x), error = function(e) NULL)
  if (is.null(mf) || is.null(mm)) {
    return(FALSE)
  }
  if (inherits(x, "coxphms")) {
    return(.coxphms_data_changed(x, mf, mm))
  }
  coefs <- stats::coef(x)
  if (!all(names(coefs) %in% colnames(mm)) || nrow(mm) != length(x$linear.predictors)) {
    return(TRUE)
  }
  # aliased coefficients are missing and do not enter the linear predictor
  coefs[is.na(coefs)] <- 0
  means <- x$means[names(coefs)]
  means[is.na(means)] <- 0
  mm <- mm[, names(coefs), drop = FALSE]
  lp <- drop(mm %*% coefs) - sum(means * coefs)
  model_offset <- stats::model.offset(mf)
  if (!is.null(model_offset)) {
    lp <- lp + model_offset
  }
  if (!isTRUE(all.equal(unname(lp), unname(x$linear.predictors)))) {
    return(TRUE)
  }
  # a change of the strata leaves the linear predictor the same
  !is.null(attr(x$terms, "specials")[["strata"]]) && .coxph_strata_changed(x, control)
}


# A fit stores no strata, so a stratified single-state fit is refitted from the
# data object of the call, at the stored coefficients and with zero iterations.
# The refit reads the data as survival's model.frame() does for the guard
# above. If the grouping into strata changed, the log-likelihood of the refit
# differs from the one of the fit. Strata relabeled without a change of
# grouping are not detected, but they leave the score residuals the same.
.coxph_strata_changed <- function(x, control) {
  env <- environment(x$terms)
  if (!is.list(control) || is.null(env)) {
    return(TRUE)
  }
  # aliased coefficients are missing and do not enter the linear predictor
  coefs <- stats::coef(x)
  coefs[is.na(coefs)] <- 0
  control$iter.max <- 0
  refit_call <- x$call
  # the call can name `coxph` without the namespace (always so for clogit),
  # and survival need not be attached
  refit_call[[1]] <- quote(survival::coxph)
  refit_call$formula <- x$terms
  refit_call$init <- unname(coefs)
  refit_call$control <- control
  refit <- tryCatch(
    suppressWarnings(eval(refit_call, env)),
    error = function(e) NULL
  )
  is.null(refit) || !isTRUE(all.equal(refit$loglik[2], x$loglik[2]))
}


# Multi-state models store one linear predictor for each row of the stacked
# data: `rmap` gives the row of the data and the transition of each stacked
# row, and `cmap` the coefficient of each column of the model matrix in each
# transition (0 if the column does not enter the transition). The stored
# predictor is centered in another way, so the two are compared up to a
# constant within each transition. A change that adds the same constant to all
# values of a predictor is therefore not detected, but it does not change the
# score residuals either. Each transition uses only its own columns: with
# transition-specific formulas, a column of another transition can have
# missing values in the rows of this transition.
.coxphms_data_changed <- function(x, mf, mm) {
  if (!.coxphms_has_maps(x)) {
    return(FALSE)
  }
  cmap <- x$cmap
  rmap <- x$rmap
  lp_stored <- x$linear.predictors
  baseline <- x$smap[1, ]
  # `rmap[, 2]` is the baseline hazard of a stacked row, numbered in the order
  # of the transitions that have coefficients (survival drops the others from
  # the stacked data). If transitions share a baseline hazard, the transition
  # of a row is not stored, and the data are not compared.
  if (anyDuplicated(baseline)) {
    return(FALSE)
  }
  has_coefs <- colSums(cmap > 0) > 0
  transition <- match(unique(baseline[has_coefs]), baseline)[rmap[, 2]]
  if (
    nrow(mm) != x$n ||
      anyNA(transition) ||
      length(lp_stored) != nrow(rmap) ||
      !all(rownames(cmap) %in% colnames(mm))
  ) {
    return(TRUE)
  }
  coefs <- stats::coef(x)
  # aliased coefficients are missing and do not enter the linear predictor
  coefs[is.na(coefs)] <- 0
  # one column of linear predictors for each transition, from its own columns
  lp_rows <- vapply(
    seq_len(ncol(cmap)),
    function(i) {
      used <- cmap[, i] > 0
      drop(mm[, rownames(cmap)[used], drop = FALSE] %*% coefs[cmap[used, i]])
    },
    numeric(nrow(mm))
  )
  lp_rows <- matrix(lp_rows, nrow = nrow(mm))
  lp <- lp_rows[cbind(rmap[, 1], transition)]
  model_offset <- stats::model.offset(mf)
  if (!is.null(model_offset)) {
    lp <- lp + model_offset[rmap[, 1]]
  }
  shift <- stats::ave(lp - lp_stored, transition)
  !isTRUE(all.equal(unname(lp - shift), unname(lp_stored)))
}


# the maps of the stacked data that .coxphms_data_changed() reads; a model
# object without them is not compared
.coxphms_has_maps <- function(x) {
  is.matrix(x$cmap) && is.matrix(x$rmap) && is.matrix(x$smap) && nrow(x$smap) > 0
}


# the control values that coxph() used: `control` if given, else the other
# arguments of the call, which coxph() passes to coxph.control(). They are
# evaluated in the formula environment. Only if the call writes the formula as
# a `~` expression is this sure to be the frame of the call: a formula object
# can come from another environment. Otherwise, a control
# argument that refers to a variable is not evaluated. If the values are not
# evaluated, the reason is returned.
.coxph_control <- function(x) {
  call_args <- as.list(x$call)[-1]
  if (is.null(call_args[["control"]])) {
    control_args <- call_args[!names(call_args) %in% names(formals(survival::coxph))]
  } else {
    control_args <- call_args["control"]
  }
  has_vars <- vapply(control_args, function(i) length(all.vars(i)) > 0, logical(1))
  if (any(has_vars) && !.coxph_formula_is_literal(x)) {
    return(paste0(
      "The control arguments of the model call (",
      toString(paste0("`", names(control_args)[has_vars], "`")),
      ") refer to variables, and the call does not write the formula as a",
      " `~` expression, so the values that the fit used are unknown."
    ))
  }
  tryCatch(
    {
      env <- environment(stats::formula(x))
      if (is.null(env)) {
        stop("The model formula has no environment.", call. = FALSE)
      }
      control <- lapply(control_args, eval, envir = env)
      if (!is.null(call_args[["control"]])) {
        control <- as.list(control[["control"]])
      }
      do.call(survival::coxph.control, control)
    },
    error = function(e) "The control arguments of the model call could not be evaluated."
  )
}


# TRUE if the call writes the formula as a `~` expression. clogit() stores the
# coxph() call that it makes, with the formula as an object, and the call of
# the user as `userCall`.
.coxph_formula_is_literal <- function(x) {
  if (is.null(x[["userCall"]])) {
    model_call <- tryCatch(match.call(survival::coxph, x$call), error = function(e) NULL)
  } else {
    model_call <- tryCatch(
      match.call(survival::clogit, x[["userCall"]]),
      error = function(e) NULL
    )
  }
  model_formula <- model_call$formula
  is.call(model_formula) &&
    !inherits(model_formula, "formula") &&
    identical(model_formula[[1]], as.name("~"))
}


# as for other models, `FALSE` is returned if convergence cannot be assessed
# (see https://github.com/easystats/insight/pull/1154)
.is_converged_coxph_not_assessed <- function(reason, verbose = TRUE) {
  if (verbose) {
    format_alert(
      paste("Convergence cannot be assessed.", reason, "Returning `FALSE`.")
    )
  }
  FALSE
}
