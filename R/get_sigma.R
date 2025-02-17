#' @title Get residual standard deviation from models
#'
#' @description Returns `sigma`, which corresponds the estimated standard
#' deviation of the residuals. This function extends the `sigma()` base R
#' generic for models that don't have implemented it. It also computes the
#' confidence interval (CI), which is stored as an attribute.
#'
#' Sigma is a key-component of regression models, and part of the so-called
#' auxiliary parameters that are estimated. Indeed, linear models for instance
#' assume that the residuals comes from a normal distribution with mean 0 and
#' standard deviation `sigma`. See the details section below for more
#' information about its interpretation and calculation.
#'
#' @name get_sigma
#'
#' @param x A model.
#' @param ci Scalar, the CI level. The default (`NULL`) returns no CI.
#' @param ... For internal use.
#' @inheritParams find_parameters
#'
#' @return The residual standard deviation (sigma), or `NULL` if this
#' information could not be accessed.
#'
#' @section Interpretation of Sigma:
#' The residual standard deviation, \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}},
#' indicates that the predicted outcome will be within +/-
#' \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}} units of the linear predictor for
#' approximately `68%` of the data points (_Gelman, Hill & Vehtari 2020, p.84_).
#' In other words, the residual standard deviation indicates the accuracy for a
#' model to predict scores, thus it can be thought of as "a measure of the
#' average distance each observation falls from its prediction from the model"
#' (_Gelman, Hill & Vehtari 2020, p.168_).
#' \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}} can be considered as a measure of
#' the unexplained variation in the data, or of the precision of inferences
#' about regression coefficients.
#'
#' @section Calculation of Sigma:
#' By default, `get_sigma()` tries to extract sigma by calling `stats::sigma()`.
#' If the model-object has no `sigma()` method, the next step is calculating
#' sigma as square-root of the model-deviance divided by the residual degrees of
#' freedom. Finally, if even this approach fails, and `x` is a mixed model, the
#' residual standard deviation is accessed using the square-root from
#' `get_variance_residual()`.
#'
#' @references Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other
#' Stories. Cambridge University Press.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_sigma(m)
#' @export
get_sigma <- function(x, ci = NULL, verbose = TRUE, ...) {
  s <- .get_sigma(x, verbose = verbose, ...)

  # Confidence interval for sigma
  ci <- .safe(.get_sigma_ci(x, ci = ci))

  if (!is.null(ci)) {
    attributes(s) <- c(attributes(s), ci)
  }

  s
}


# Retrieve sigma ----------------------------------------------------------


.get_sigma <- function(x, ...) {
  UseMethod(".get_sigma")
}


# special handling ---------------


.get_sigma.glmerMod <- function(x, ...) {
  check_if_installed("lme4")
  if (startsWith(stats::family(x)$family, "Negative Binomial(")) {
    lme4::getME(x, "glmer.nb.theta")
  } else {
    stats::sigma(x)
  }
}


.get_sigma.glmmadmb <- function(x, ...) {
  check_if_installed("lme4")
  vc <- lme4::VarCorr(x)
  s <- attr(vc, "sc")
  # sanity check
  if (is.null(s)) {
    s <- .safe(x$alpha)
  }
  s
}


.get_sigma.glmmTMB <- function(x, ...) {
  # The commented code is what MuMIn returns for sigma for nbinom1 models.
  # However, I think this is wrong. Nakagawa et al. (2017) used this in their
  # code because glmmadmb models with nbinom1 family are actually Quasi-Poisson
  # models (see also Supplement 2). Thus, we revert and just use "sigma()" again.
  # This will return results for `get_variance()` that are in line with the code
  # in the Supplement 2 from Nakaawa et al. (2017).
  # if (stats::family(x)$family == "nbinom1") {
  #   add_value <- 1
  # } else {
  #   add_value <- 0
  # }
  # stats::sigma(x) + add_value
  stats::sigma(x)
}


.get_sigma.merMod <- function(x, ...) {
  stats::sigma(x)
}


.get_sigma.svy2lme <- function(x, ...) {
  sqrt(as.vector(x$s2))
}


.get_sigma.model_fit <- function(x, verbose = TRUE, ...) {
  .get_sigma(x$fit, verbose = verbose)
}


.get_sigma.lrm <- function(x, ...) {
  s <- stats::sigma(x)
  s <- s[length(s)]
  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.VGAM <- function(x, ...) {
  s <- .safe(exp(stats::coef(x)[["(Intercept):2"]]))
  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.merModList <- function(x, ...) {
  s <- suppressWarnings(summary(x))
  s <- s$residError
  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.summary.lm <- function(x, ...) {
  s <- x$sigma
  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.selection <- function(x, ...) {
  s <- unname(stats::coef(x)["sigma"])
  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.cgam <- function(x, verbose = TRUE, ...) {
  s <- .safe(
    sqrt(get_deviance(x, verbose = verbose) / get_df(x, type = "residual", verbose = verbose))
  )

  if (is_empty_object(s)) {
    return(NULL)
  }

  class(s) <- c("insight_aux", class(s))
  s
}


.get_sigma.cpglmm <- function(x, ...) {
  s <- .safe(stats::deviance(x)[["sigmaML"]])
  if (!is.null(s)) {
    class(s) <- c("insight_aux", class(s))
  }
  s
}


.get_sigma.lme <- function(x, ...) {
  .safe(x$sigma)
}


.get_sigma.geeglm <- function(x, ...) {
  out <- .safe(stats::sigma(x))
  if (is.data.frame(out)) {
    out <- out$Estimate
  }
  out
}


.get_sigma.mjoint <- function(x, ...) {
  .safe(x$coef$sigma2[[1]])
}


.get_sigma.glmmPQL <- function(x, ...) {
  switch(x$family$family,
    gaussian = ,
    Gamma = x$sigma,
    x$sigma^2
  )
}


.get_sigma.brmsfit <- function(x, ...) {
  s <- tryCatch(
    {
      dat <- as.data.frame(x)
      sigma_column <- grep("sigma", colnames(dat), fixed = TRUE)
      if (length(sigma_column) == 1) {
        mean(dat[[sigma_column]])
      } else if (length(sigma_column)) {
        # if more than one sigma column,
        # there isn't a traditional sigma for the model
        return(NULL)
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )
  # compute sigma manually ---------------
  if (is_empty_object(s)) {
    # default sigma ---------------
    s <- .safe(stats::sigma(x))
  }

  if (is_empty_object(s)) {
    info <- model_info(x, verbose = FALSE)
    if (!is.null(info) && info$is_mixed) {
      dots <- list(...)
      # in "get_variance()", we call "get_sigma()" - make sure we avoid
      # recursion and infinite loops
      if (!isTRUE(dots$no_recursion)) {
        s <- .safe(sqrt(get_variance_residual(x, verbose = FALSE)))
      }
    }
  }

  if (is_empty_object(s)) {
    return(NULL)
  }
  class(s) <- c("insight_aux", class(s))
  s
}


# default handling ---------------

.get_sigma.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, c("mipo", "mira", "riskRegression"))) {
    return(NULL)
  }

  # default sigma ---------------
  s <- .safe(stats::sigma(x))


  # compute sigma manually ---------------
  if (is_empty_object(s)) {
    info <- model_info(x, verbose = FALSE)
    if (!is.null(info) && info$is_dispersion) {
      return(NULL)
    }
  }

  if (is_empty_object(s)) {
    s <- .safe({
      model_deviance <- get_deviance(x, verbose = verbose)
      residual_df <- get_df(x, type = "residual", verbose = verbose)
      sqrt(abs(model_deviance) / residual_df)
    })
  }

  if (is_empty_object(s)) {
    return(NULL)
  }

  class(s) <- c("insight_aux", class(s))
  s
}


# Methods -----------------------------------------------------------------

.get_sigma_ci <- function(x, ci = 0.95, ...) {
  # TODO: What does it work for Bayesian models?

  if (is.null(ci) || is.na(ci)) {
    return(NULL)
  }

  alpha <- 1 - ci
  dev <- get_deviance(x)
  n <- n_obs(x)
  low <- dev / stats::qchisq(1 - alpha / 2, n)
  high <- dev / stats::qchisq(alpha / 2, n)
  list(CI_low = sqrt(low), CI_high = sqrt(high))
}


#' @export
as.double.insight_aux <- function(x, ...) {
  if (is.null(x) || all(is.na(x)) || all(is.infinite(x)) || !is.numeric(x)) {
    return(NULL)
  }
  mean(x, na.rm = TRUE)
}

#' @export
as.numeric.insight_aux <- as.double.insight_aux
