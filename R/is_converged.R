#' @title Convergence test for mixed effects and Cox models
#' @name is_converged
#'
#' @description `is_converged()` provides an alternative convergence
#'   test for `merMod`-objects. For `coxph` models, it recomputes the checks
#'   of the *survival* package.
#'
#' @param x A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan`,
#' `_glm` or `coxph`.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be. Not
#'   used for `coxph` models.
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @return `TRUE` if convergence is fine and `FALSE` if convergence is
#'   suspicious. Additionally, the convergence value is returned as attribute.
#'   For `merMod` models, if the model is singular, convergence is determined by
#'   the optimizer's convergence code. For non-singular models where derivatives
#'   are unavailable, `FALSE` is returned and a message is printed to indicate
#'   that convergence cannot be assessed through the usual gradient-based checks.
#'   For `coxph` models, the attribute `diagnostics` is a data frame with the
#'   result of each check. If convergence cannot be assessed, `FALSE` is returned
#'   without this attribute, and, if `verbose = TRUE`, a message gives the
#'   reason.
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
#' of 1 or less, for models fitted with `y = FALSE`, for model objects without
#' a call, and if the control arguments of the model call or the score
#' residuals cannot be computed. Objects of other classes that inherit from
#' `coxph`, for example from `survival::clogit()` or `survey::svycoxph()`, are
#' not supported: `NULL` is returned with a message.
#'
#' For models with right-censored data, the score residuals are computed from
#' the data of the model call, unless the model was fitted with `model = TRUE`
#' or `x = TRUE`. If these data were removed after the model was fitted,
#' convergence cannot be assessed. If they were changed, the result can be
#' wrong. In both cases, refit the model with `model = TRUE`.
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


# the checks are those that `survival:::coxph.fit()` (right-censored data) and
# `survival:::agreg.fit()` (counting-process data) apply after the fit, and
# that give the warnings "Ran out of iterations and did not converge" and
# "Loglik converged before variable ...; coefficient (or beta) may be infinite"
#' @rdname is_converged
#' @export
is_converged.coxph <- function(x, tolerance = 0.001, verbose = TRUE, ...) {
  # objects of other classes that inherit from "coxph" (for example from
  # survey::svycoxph(), which stores its own call, or survival::clogit()) go to
  # the default method, as before
  if (!class(x)[1] %in% c("coxph", "coxphms", "coxph.penal", "coxph.null")) {
    return(NextMethod())
  }
  check_if_installed("survival")

  # a null model has no coefficients that could diverge
  if (inherits(x, "coxph.null")) {
    return(structure(TRUE, diagnostics = .coxph_diagnostics()))
  }
  reason <- .coxph_not_assessed_reason(x)
  if (!is.null(reason)) {
    return(.is_converged_coxph_not_assessed(reason, verbose))
  }
  control <- .coxph_control(x)
  if (is.null(control)) {
    return(.is_converged_coxph_not_assessed(
      "The control arguments of the model call could not be evaluated.",
      verbose
    ))
  }
  # survival applies no check for `iter.max` <= 1
  if (control$iter.max <= 1) {
    return(.is_converged_coxph_not_assessed(
      "The model was fitted with `iter.max` <= 1, so survival applies no checks.",
      verbose
    ))
  }

  # coxph() sends right-censored responses to coxph.fit() and all other
  # responses to agreg.fit()
  counting <- !grepl("right", attr(x$y, "type"), fixed = TRUE)

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
      if (
        is.null(x$model) && grepl("not found", conditionMessage(infinite), fixed = TRUE)
      ) {
        reason <- paste(
          reason,
          "If the data of the model are no longer available, refit the model with `model = TRUE`."
        )
      }
      return(.is_converged_coxph_not_assessed(reason, verbose))
    }
    diagnostics <- rbind(diagnostics, infinite)
  }

  converged <- all(diagnostics$Passed)

  if (verbose && !converged) {
    .coxph_alert(diagnostics, control$iter.max)
  }

  structure(converged, diagnostics = diagnostics)
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
  if (is.null(x$iter) || is.null(x$coefficients)) {
    return("The model object has no iterations or coefficients.")
  }
  if (inherits(x, "coxph.penal")) {
    return(paste(
      "The checks do not apply to penalized models, for example models",
      "with `frailty()`, `ridge()` or `pspline()` terms."
    ))
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
  if (is.null(attr(x$y, "type"))) {
    return("The model has no response. Refit the model with `y = TRUE`.")
  }
  NULL
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


# the control values that coxph() used: `control` if given, else the other
# arguments of the call, which coxph() passes to coxph.control()
.coxph_control <- function(x) {
  tryCatch(
    {
      env <- environment(stats::formula(x))
      if (is.null(env)) {
        stop("The model formula has no environment.", call. = FALSE)
      }
      call_args <- as.list(x$call)[-1]
      if (is.null(call_args$control)) {
        extra <- call_args[!names(call_args) %in% names(formals(survival::coxph))]
        extra <- lapply(extra, eval, envir = env)
        do.call(survival::coxph.control, extra)
      } else {
        control <- eval(call_args$control, envir = env)
        do.call(survival::coxph.control, as.list(control))
      }
    },
    error = function(e) NULL
  )
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
