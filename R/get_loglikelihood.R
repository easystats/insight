#' @title Log-Likelihood and Log-Likelihood correction
#' @name get_loglikelihood
#'
#' @description
#' A robust function to compute the log-likelihood of a model, as well as
#' individual log-likelihoods (for each observation) whenever possible. Can be
#' used as a replacement for `stats::logLik()` out of the box, as the
#' returned object is of the same class (and it gives the same results by
#' default).
#'
#' `get_loglikelihood_adjustment()` can be used to correct the log-likelihood
#' for models with transformed response variables. The adjustment value can
#' be added to the log-likelihood to get the corrected value. This is done
#' automatically in `get_loglikelihood()` if `check_response = TRUE`.
#'
#' @param estimator Corresponds to the different estimators for the standard
#' deviation of the errors. If `estimator="ML"` (default), the scaling is
#' done by n (the biased ML estimator), which is then equivalent to using
#' `stats::logLik()`. If `estimator="OLS"`, it returns the unbiased
#' OLS estimator. `estimator="REML"` will give same results as
#' `logLik(..., REML=TRUE)`.
#' @param REML Only for linear models. This argument is present for
#' compatibility with `stats::logLik()`. Setting it to `TRUE` will
#' overwrite the `estimator` argument and is thus equivalent to setting
#' `estimator="REML"`. It will give the same results as
#' `stats::logLik(..., REML=TRUE)`. Note that individual log-likelihoods
#' are not available under REML.
#' @param check_response Logical, if `TRUE`, checks if the response variable
#' is transformed (like `log()` or `sqrt()`), and if so, returns a corrected
#' log-likelihood. To get back to the original scale, the likelihood of the
#' model is multiplied by the Jacobian/derivative of the transformation.
#' @param ... Passed down to `logLik()`, if possible.
#' @inheritParams get_residuals
#'
#' @return `get_loglikelihood()` returns an object of class `"logLik"`, also
#' containing the log-likelihoods for each observation as a `per_observation`
#' attribute (`attributes(get_loglikelihood(x))$per_observation`) when
#' possible. The code was partly inspired from the **nonnest2** package.
#'
#' `get_loglikelihood_adjustment()` returns the adjustment value to be added to
#' the log-likelihood to correct for transformed response variables, or `NULL`
#' if the adjustment could not be computed.
#'
#' @examples
#' x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#'
#' get_loglikelihood(x, estimator = "ML") # Equivalent to stats::logLik(x)
#' get_loglikelihood(x, estimator = "REML") # Equivalent to stats::logLik(x, REML=TRUE)
#' get_loglikelihood(x, estimator = "OLS")
#' @export
get_loglikelihood <- function(x, ...) {
  UseMethod("get_loglikelihood")
}


#' @rdname get_loglikelihood
#' @export
loglikelihood <- get_loglikelihood


#' @export
get_loglikelihood.default <- function(x, ...) {
  .loglikelihood_prep_output(x, lls = NA, ...)
}


#' @rdname get_loglikelihood
#' @export
get_loglikelihood_adjustment <- function(x) {
  weights <- get_weights(x, remove_na = TRUE)
  tryCatch(
    {
      trans <- find_transformation(x)

      if (trans == "identity") { # nolint
        .weighted_sum(log(get_response(x, as_proportion = TRUE)), w = weights)
      } else if (trans == "log") {
        .weighted_sum(log(1 / get_response(x, as_proportion = TRUE)), w = weights)
      } else if (trans == "log1p") {
        .weighted_sum(log(1 / (get_response(x, as_proportion = TRUE) + 1)), w = weights)
      } else if (trans == "log2") {
        .weighted_sum(log(1 / (get_response(x, as_proportion = TRUE) * log(2))), w = weights)
      } else if (trans == "log10") {
        .weighted_sum(log(1 / (get_response(x, as_proportion = TRUE) * log(10))), w = weights)
      } else if (trans == "exp") {
        .weighted_sum(get_response(x, as_proportion = TRUE), w = weights)
      } else if (trans == "expm1") {
        .weighted_sum((get_response(x, as_proportion = TRUE) - 1), w = weights)
      } else if (trans == "sqrt") {
        .weighted_sum(log(0.5 / sqrt(get_response(x, as_proportion = TRUE))), w = weights)
      } else if (trans == "inverse") {
        # first derivative of 1/x is -1/x^2 - we cannot take the log from negative
        # values, so this won't work here, and we return NULL
        NULL
      } else if (trans == "box-cox") {
        # not yet supported
        NULL
      } else if (trans == "scale") {
        scale_denominator <- .extract_scale_denominator(x)
        .weighted_sum(log(1 / rep.int(scale_denominator, n_obs(x))), w = weights) # nolint
      } else if (trans == "power") {
        trans_power <- .extract_power_transformation(x)
        .weighted_sum(log(trans_power * (get_response(x, as_proportion = TRUE)^(trans_power - 1))), w = weights) # nolint
      } else if (is.null(weights)) {
        .ll_log_adjustment(x)
      } else {
        .ll_jacobian_adjustment(x, weights)
      }
    },
    # for negative log-values, we get a warning and NaN is returned
    # capture this here and return NULL instead
    warning = function(e) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )
}


#' @rdname get_loglikelihood
#' @export
get_loglikelihood.lm <- function(x,
                                 estimator = "ML",
                                 REML = FALSE,
                                 check_response = FALSE,
                                 verbose = TRUE,
                                 ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
    x <- x$gam
  }

  info <- model_info(x, verbose = FALSE)
  if (info$is_tweedie) {
    check_if_installed("tweedie")
    ll <- .loglikelihood_prep_output(x, lls = tweedie::logLiktweedie(x))
  } else if (info$is_linear) {
    ll <- .get_loglikelihood_lm(x,
      estimator = estimator,
      REML = REML,
      check_response = check_response,
      verbose = verbose,
      ...
    )
  } else {
    ll <- .get_loglikelihood_glm(x, info = info, verbose = verbose, ...)
  }
  ll
}

#' @export
get_loglikelihood.ivreg <- get_loglikelihood.lm

#' @export
get_loglikelihood.glm <- get_loglikelihood.lm

#' @export
get_loglikelihood.gam <- get_loglikelihood.lm

#' @export
get_loglikelihood.gamm <- get_loglikelihood.lm

#' @export
get_loglikelihood.list <- get_loglikelihood.lm


#' @export
get_loglikelihood.lmerMod <- function(x,
                                      estimator = NULL,
                                      REML = FALSE,
                                      check_response = FALSE,
                                      verbose = TRUE,
                                      ...) {
  # use defaults for REML?
  if ((missing(estimator) || is.null(estimator)) && missing(REML)) {
    lls <- stats::logLik(x)
  } else {
    # else, explicitly set REML for lme4 models
    if (identical(estimator, "REML")) {
      REML <- TRUE
    }
    lls <- stats::logLik(x, REML = REML)
  }

  .loglikelihood_prep_output(
    x,
    lls,
    check_response = check_response,
    verbose = verbose,
    REML = REML,
    lls2 = .per_observation_ll(x),
    ...
  )
}


#' @export
get_loglikelihood.glmerMod <- function(x, check_response = FALSE, verbose = TRUE, ...) {
  .loglikelihood_prep_output(
    x,
    lls = stats::logLik(x),
    check_response = check_response,
    verbose = verbose,
    REML = FALSE,
    lls2 = .per_observation_ll(x),
    ...
  )
}

#' @export
get_loglikelihood.glmmTMB <- get_loglikelihood.lmerMod


#' @export
get_loglikelihood.hglm <- function(x,
                                   check_response = FALSE,
                                   verbose = TRUE,
                                   ...) {
  lls <- x$likelihood
  .loglikelihood_prep_output(
    x,
    lls,
    check_response = check_response,
    verbose = verbose,
    REML = FALSE,
    lls2 = .per_observation_ll(x),
    ...
  )
}


#' @export
get_loglikelihood.mblogit <- function(x, verbose = TRUE, ...) {
  .loglikelihood_prep_output(
    x,
    lls = stats::logLik(x),
    check_response = FALSE,
    verbose = verbose,
    REML = FALSE,
    lls2 = .per_observation_ll(x),
    ...
  )
}

#' @export
get_loglikelihood.mclogit <- get_loglikelihood.mblogit

#' @export
get_loglikelihood.mlogit <- get_loglikelihood.mblogit


#' @export
get_loglikelihood.model_fit <- function(x,
                                        estimator = "ML",
                                        REML = FALSE,
                                        check_response = FALSE,
                                        verbose = TRUE,
                                        ...) {
  get_loglikelihood(
    x$fit,
    estimator = estimator,
    REML = REML,
    check_response = check_response,
    verbose = verbose,
    ...
  )
}


#' @export
get_loglikelihood.afex_aov <- function(x, ...) {
  get_loglikelihood(x$lm, ...)
}


#' @export
get_loglikelihood.stanreg <- function(x, centrality = stats::median, ...) {
  check_if_installed("rstanarm")

  # Get posterior distribution of logliks
  mat <- rstanarm::log_lik(x)
  # Point estimate using the function passed as the centrality argument
  lls <- vapply(as.data.frame(mat), centrality, numeric(1))

  .loglikelihood_prep_output(x, lls)
}


# Methods WITHOUT individual LLs ---------------------------------------------


#' @export
get_loglikelihood.iv_robust <- function(x, verbose = TRUE, ...) {
  res <- get_residuals(x)
  w <- get_weights(x, null_as_ones = TRUE)

  # drop weights that are exactly zero
  excl <- w == 0
  if (any(excl)) {
    res <- res[!excl]
    w <- w[!excl]
  }

  N <- length(res)
  lls <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  .loglikelihood_prep_output(
    x,
    lls,
    df = get_df(x, type = "model"),
    verbose = verbose
  )
}

#' @export
get_loglikelihood.lm_robust <- get_loglikelihood.iv_robust


#' @export
get_loglikelihood.svycoxph <- function(x, ...) {
  .loglikelihood_prep_output(x, lls = x$ll[2], df = x$degf.resid)
}


#' @export
get_loglikelihood.crr <- function(x, ...) {
  x$loglik
}


#' @export
get_loglikelihood.plm <- function(x, check_response = FALSE, verbose = TRUE, ...) {
  res <- get_residuals(x)
  w <- get_weights(x, null_as_ones = TRUE)
  N <- n_obs(x)

  ll <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  .loglikelihood_prep_output(
    x,
    lls = ll,
    df = get_df(x, type = "model"),
    check_response = check_response,
    verbose = verbose
  )
}

#' @export
get_loglikelihood.cpglm <- get_loglikelihood.plm


#' @export
get_loglikelihood.phylolm <- function(x, check_response = FALSE, verbose = TRUE, ...) {
  .loglikelihood_prep_output(
    x,
    lls = stats::logLik(x)$logLik,
    df = get_df(x, type = "model"),
    check_response = check_response,
    verbose = verbose
  )
}

#' @export
get_loglikelihood.phyloglm <- get_loglikelihood.phylolm


# Methods WITH individual LLs ---------------------------------------------

# TODO: Complete for other families with https://github.com/cran/nonnest2/blob/master/R/llcont.R
# https://stats.stackexchange.com/questions/322038/input-format-for-response-in-binomial-glm-in-r


.get_loglikelihood_lm <- function(x,
                                  estimator = "ML",
                                  REML = FALSE,
                                  check_response = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # Replace arg if compatibility base R is activated
  if (REML) estimator <- "REML"
  if (is.null(estimator)) estimator <- "ML"

  # Get weights
  w <- get_weights(x, null_as_ones = TRUE)
  res <- get_residuals(x, verbose = verbose)

  excl <- w == 0
  if (any(excl)) {
    res <- res[!excl]
    w <- w[!excl]
  }

  # Get LogLikelihood
  estimator <- tolower(estimator)

  # REML (directly returned)
  # TODO: Find a way of reversing this formula to pull the sums out and get individual lls
  if (estimator == "reml") {
    if (!"qr" %in% names(x)) {
      format_error("REML estimation not available for this model.")
    }
    N <- get_df(x, type = "residual") # n_obs - p
    val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))
    p <- n_parameters(x, remove_nonestimable = TRUE)
    ll <- val - sum(log(abs(diag(x$qr$qr)[1:p])))
    return(.loglikelihood_prep_output(x, ll, check_response = check_response, REML = REML, verbose = verbose))
  }

  # Get S2
  s <- as.numeric(get_sigma(x))
  if (estimator == "ols") {
    s2 <- s^2
  } else if (estimator == "ml") {
    s2 <- (s * sqrt(get_df(x, type = "residual") / n_obs(x)))^2
  } else {
    format_error("`estimator` should be one of \"ML\", \"REML\" or \"OLS\".")
  }
  # Get individual log-likelihoods
  lls <- 0.5 * (log(w) - (log(2 * pi) + log(s2) + (w * res^2) / s2))

  .loglikelihood_prep_output(x, lls, check_response = check_response, REML = REML, verbose = verbose)
}


.get_loglikelihood_glm <- function(x, info, verbose = TRUE, ...) {
  fam <- info$family
  resp <- get_response(x, as_proportion = TRUE, verbose = verbose)
  w <- get_weights(x, null_as_ones = TRUE)
  dev <- stats::deviance(x)
  disp <- dev / sum(w)
  predicted <- get_predicted(x, ci = NULL, verbose = verbose)

  # Make adjustment for binomial models with matrix as input
  if (info$is_binomial) {
    resp <- .factor_to_numeric(resp, lowest = 0)
    if (is.null(ncol(resp))) {
      n <- rep.int(1, length(resp))
    } else {
      n <- rowSums(resp)
      resp <- ifelse(n == 0, 0, resp[, 1] / n)
    }
    n <- if (any(n > 1L)) n else w
    w <- ifelse(n > 0, (w / n), 0)
  }

  # Calculate Log Likelihoods depending on the family
  lls <- switch(fam,
    binomial = ,
    categorical = ,
    multinomial = ,
    ordinal = {
      stats::dbinom(round(n * resp), round(n), predicted, log = TRUE) * w
    },
    quasibinomial = {
      NA
    },
    poisson = {
      stats::dpois(round(resp), predicted, log = TRUE) * w
    },
    quasipoisson = {
      NA
    },
    gaussian = {
      model_nobs <- length(resp)
      -((log(dev / model_nobs * 2 * pi) + 1) - log(w)) / 2
    },
    inverse.gaussian = {
      -((log(disp * 2 * pi) + 1) + 3 * log(resp)) / 2
    },
    Gamma = {
      stats::dgamma(resp, shape = 1 / disp, scale = predicted * disp, log = TRUE) * w
    }
  )

  .loglikelihood_prep_output(x, lls)
}


# Helpers -----------------------------------------------------------------

.loglikelihood_prep_output <- function(x,
                                       lls = NA,
                                       df = NULL,
                                       check_response = FALSE,
                                       verbose = FALSE,
                                       lls2 = NULL,
                                       ...) {
  # Prepare output
  if (all(is.na(lls))) {
    out <- stats::logLik(x, ...)
    if (is.null(lls2)) {
      attr(out, "per_obs") <- NA
    } else {
      attr(out, "per_obs") <- lls2
    }
  } else if (length(lls) == 1) {
    out <- lls
  } else {
    out <- sum(lls)
    attr(out, "per_obs") <- lls # This is useful for some models comparison tests
  }


  if (isTRUE(check_response)) {
    # check if we have transformed response, and if so, adjust LogLik
    response_transform <- find_transformation(x)
    if (!is.null(response_transform) && !identical(response_transform, "identity")) {
      # get log-likelihood adjustment-value
      ll_adjustment <- get_loglikelihood_adjustment(x)
      if (is.null(ll_adjustment) && isTRUE(verbose)) {
        format_warning("Could not compute corrected log-likelihood for models with transformed response. Log-likelihood value is probably inaccurate.") # nolint
      } else if (!is.null(ll_adjustment)) {
        out[1] <- out[1] + ll_adjustment
        if (isTRUE(list(...)$REML) && isTRUE(verbose)) {
          format_warning("Log-likelihood is corrected for models with transformed response. However, this ignores `REML=TRUE`. Log-likelihood value is probably inaccurate.") # nolint
        }
      }
    }
  }

  # Some attributes present in stats::logLik (not sure what nall does)
  attr(out, "nall") <- attr(out, "nobs") <- n_obs(x)

  # See https://stats.stackexchange.com/q/393016/54740
  if (is.null(df)) df <- get_df(x, type = "model")
  attr(out, "df") <- df

  # Make of same class as returned by stats::logLik(x)
  class(out) <- c("logLik", class(x))
  out
}


.ll_log_adjustment <- function(x) {
  out <- tryCatch(
    {
      suppressWarnings(sum(stats::dlnorm(
        x = get_response(x, as_proportion = TRUE),
        meanlog = stats::fitted(x),
        sdlog = get_sigma(x, ci = NULL, verbose = FALSE),
        log = TRUE
      )))
    },
    error = function(e) {
      NULL
    }
  )
  # if adjustment failed, e.g. due to negative numbers for the log, return NULL instead
  if (is.na(out) || is.infinite(out)) {
    out <- NULL
  }
  out
}


.ll_jacobian_adjustment <- function(model, weights = NULL) {
  out <- tryCatch(
    {
      trans <- get_transformation(model)$transformation
      suppressWarnings(.weighted_sum(log(
        diag(attr(with(
          get_data(model, verbose = FALSE),
          stats::numericDeriv(
            expr = quote(trans(
              get(find_response(model))
            )),
            theta = find_response(model)
          )
        ), "gradient"))
      ), weights))
    },
    error = function(e) {
      NULL
    }
  )
  # if adjustment failed, e.g. due to negative numbers for the log, return NULL instead
  if (is.na(out) || is.infinite(out)) {
    out <- NULL
  }
  out
}


.weighted_sum <- function(x, w = NULL, ...) {
  if (is.null(w)) {
    mean(x) * length(x)
  } else {
    stats::weighted.mean(x, w) * length(x)
  }
}


.per_observation_ll <- function(x) {
  # per observation lls
  .safe({
    w <- get_weights(x, null_as_ones = TRUE)
    s2 <- (get_sigma(x) * sqrt(get_df(x, type = "residual") / n_obs(x)))^2
    0.5 * (log(w) - (log(2 * pi) + log(s2) + (w * get_residuals(x, verbose = FALSE)^2) / s2))
  })
}
