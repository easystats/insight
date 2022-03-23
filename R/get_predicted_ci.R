#' @inheritParams get_predicted
#' @param predictions A vector of predicted values (as obtained by
#'   `stats::fitted()`, `stats::predict()` or
#'   [get_predicted()]).
#' @param ci The interval level (default `0.95`, i.e., `95%` CI).
#' @param ci_type Can be `"prediction"` or `"confidence"`. Prediction
#'   intervals show the range that likely contains the value of a new
#'   observation (in what range it would fall), whereas confidence intervals
#'   reflect the uncertainty around the estimated parameters (and gives the
#'   range of the link; for instance of the regression line in a linear
#'   regressions). Prediction intervals account for both the uncertainty in the
#'   model's parameters, plus the random variation of the individual values.
#'   Thus, prediction intervals are always wider than confidence intervals.
#'   Moreover, prediction intervals will not necessarily become narrower as the
#'   sample size increases (as they do not reflect only the quality of the fit).
#'   This applies mostly for "simple" linear models (like `lm`), as for
#'   other models (e.g., `glm`), prediction intervals are somewhat useless
#'   (for instance, for a binomial model for which the dependent variable is a
#'   vector of 1s and 0s, the prediction interval is... `[0, 1]`).
#' @param se Numeric vector of standard error of predicted values. If `NULL`,
#'   standard errors are calculated based on the variance-covariance matrix.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates (e.g., for robust standard errors). This argument accepts a covariance matrix, a function which returns a covariance matrix, or a string which identifies the function to be used to compute the covariance matrix.
#'  * A covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"vcovHC"`, `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`, `"CR2"`, `"CR3"`. See `?clubSandwich::vcovCR()`
#'    - Bootstrap: `"vcovBS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`, `"webb"`. See `?sandwich::vcovBS`
#'    - Other `sandwich` package functions: `"vcovHAC"`, `"vcovPC"`, `"vcovCL"`, `"vcovPL"`.
#' @param vcov_args List of arguments to be passed to the function identified by
#'   the `vcov` argument. This function is typically supplied by the **sandwich**
#'   or **clubSandwich** packages. Please refer to their documentation (e.g.,
#'   `?sandwich::vcovHAC`) to see the list of available arguments.
#' @param dispersion_method,ci_method These arguments are only used in
#'   the context of bootstrapped and Bayesian models. Possible values are
#'   `dispersion_method = c("sd", "mad")` and
#'   `ci_method = c("quantile", "hdi", "eti")`. For the latter, the
#'   **bayestestR** package is required.
#'
#' @examples
#' # Confidence Intervals for Model Predictions
#' # ------------------------------------------
#'
#' data(mtcars)
#'
#' # Linear model
#' # ------------
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' predictions <- predict(x)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
#' head(ci_vals)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
#' head(ci_vals)
#' ci_vals <- get_predicted_ci(x, predictions, ci = c(0.8, 0.9, 0.95))
#' head(ci_vals)
#'
#' # Bootstrapped
#' # ------------
#' if (require("boot")) {
#'   predictions <- get_predicted(x, iterations = 500)
#'   get_predicted_ci(x, predictions)
#' }
#'
#' if (require("datawizard") && require("bayestestR")) {
#'   ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
#'   head(ci_vals)
#'   datawizard::reshape_ci(ci_vals)
#'
#'   ci_vals <- get_predicted_ci(x,
#'     predictions,
#'     dispersion_method = "MAD",
#'     ci_method = "HDI"
#'   )
#'   head(ci_vals)
#' }
#'
#'
#' # Logistic model
#' # --------------
#' x <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' predictions <- predict(x, type = "link")
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
#' head(ci_vals)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
#' head(ci_vals)
#' @rdname get_predicted
#' @export
get_predicted_ci <- function(x, predictions = NULL, ...) {
  UseMethod("get_predicted_ci")
}




# General method ----------------------------------------------------------

#' @rdname get_predicted
#' @export
get_predicted_ci.default <- function(x,
                                     predictions = NULL,
                                     data = NULL,
                                     se = NULL,
                                     ci = 0.95,
                                     ci_type = "confidence",
                                     ci_method = "quantile",
                                     dispersion_method = "sd",
                                     vcov = NULL,
                                     vcov_args = NULL,
                                     ...) {
  # sanity check, if CI should be skipped
  if (is.null(ci)) {
    return(ci)
  }

  # If draws are present (bootstrapped or Bayesian)
  if ("iterations" %in% names(attributes(predictions))) {
    iter <- attributes(predictions)$iteration
    se <- .get_predicted_se_from_iter(iter = iter, dispersion_method)
    out <- .get_predicted_ci_from_iter(iter = iter, ci = ci, ci_method)
    out <- cbind(se, out)
    # outcome is multinomial/ordinal/cumulative
    if (inherits(predictions, "data.frame") &&
      "Response" %in% colnames(predictions) &&
      "Row" %in% colnames(predictions)) {
      out <- cbind(predictions[, c("Row", "Response")], out)
    }
    return(out)
  }

  # Analytical solution
  # 1. Find appropriate interval function
  if (!is.null(se)) {
    ci_function <- .get_predicted_se_to_ci
  } else if (ci_type == "confidence" || get_family(x)$family %in% c("gaussian") || (!is.null(vcov) && is.matrix(vcov))) { # gaussian or CI
    se <- get_predicted_se(
      x,
      data = data,
      ci_type = ci_type,
      vcov = vcov,
      vcov_args = vcov_args,
      ...
    )
    ci_function <- .get_predicted_se_to_ci
  } else {
    se <- rep(NA, length(predictions))
    ci_function <- .get_predicted_pi_glm
  }

  # 2. Run it once or multiple times if multiple CI levels are requested
  if (is.null(ci)) {
    out <- data.frame(SE = se)
  } else if (length(ci) == 1) {
    out <- ci_function(x, predictions, ci = ci, se = se)
  } else {
    out <- data.frame(SE = se)
    for (ci_val in ci) {
      temp <- ci_function(x, predictions, ci = ci_val, se = se)
      temp$SE <- NULL
      names(temp) <- paste0(names(temp), "_", ci_val)
      out <- cbind(out, temp)
    }
  }

  out
}

#' @export
get_predicted_ci.mlm <- function(x, ...) {
  stop("TBD")
}




## Convert to CI -----------

.get_predicted_se_to_ci <- function(x,
                                    predictions = NULL,
                                    se = NULL,
                                    ci = 0.95,
                                    ...) {

  # TODO: Prediction interval for binomial: https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/
  # TODO: Prediction interval for poisson: https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-ii/

  # Sanity checks
  if (!is.null(se)) {
    se <- as.numeric(se)
  }
  if (is.null(predictions)) {
    return(data.frame(SE = se))
  }

  if (is.null(ci)) {
    return(data.frame(CI_low = predictions, CI_high = predictions))
  } # Same as predicted

  dof <- get_df(x, type = "residual")

  # Return NA
  if (is.null(se)) {
    se <- ci_low <- ci_high <- rep(NA, length(predictions))

    # Get CI
    # TODO: Does this cover all the model families?
  } else {
    if (is.null(dof) || is.infinite(dof) || find_statistic(x) == "z-statistic") {
      crit_val <- stats::qnorm(p = (1 + ci) / 2)
    } else {
      crit_val <- stats::qt(p = (1 + ci) / 2, df = dof)
    }

    if (length(predictions) != length(se)) {
      # multiple length?
      if (length(predictions) %% length(se) == 0) {
        # for multiple length, SE and predictions may match, could be intended?
        # could there be any cases where we have twice or x times the length of
        # predictions as standard errors?
        warning(format_message("Predictions and standard errors are not of the same length. Please check if you need the 'data' argument."), call. = FALSE)
      } else {
        stop(format_message("Predictions and standard errors are not of the same length. Please specify the 'data' argument."), call. = FALSE)
      }
    }

    ci_low <- as.numeric(predictions - (se * crit_val))
    ci_high <- as.numeric(predictions + (se * crit_val))
  }

  data.frame(SE = se, CI_low = ci_low, CI_high = ci_high)
}




.get_predicted_se_to_ci_zeroinfl <- function(x,
                                             predictions = NULL,
                                             se = NULL,
                                             ci = 0.95,
                                             link_inv = NULL,
                                             ...) {

  # Sanity checks
  if (is.null(predictions)) {
    return(data.frame(SE = se))
  }

  if (is.null(ci)) {
    return(data.frame(CI_low = predictions, CI_high = predictions))
  } # Same as predicted

  # Return NA
  if (is.null(se)) {
    se <- ci_low <- ci_high <- rep(NA, length(predictions))

    # Get CI
    # TODO: Does this cover all the model families?
  } else {
    crit_val <- stats::qnorm(p = (1 + ci) / 2)

    if (length(predictions) != length(se)) {
      # multiple length?
      if (length(predictions) %% length(se) == 0) {
        # for multiple length, SE and predictions may match, could be intended?
        # could there be any cases where we have twice or x times the length of
        # predictions as standard errors?
        warning(format_message("Predictions and standard errors are not of the same length. Please check if you need the 'data' argument."), call. = FALSE)
      } else {
        stop(format_message("Predictions and standard errors are not of the same length. Please specify the 'data' argument."), call. = FALSE)
      }
    }

    ci_low <- link_inv(predictions - (se * crit_val))
    ci_high <- link_inv(predictions + (se * crit_val))
  }

  data.frame(SE = se, CI_low = ci_low, CI_high = ci_high)
}




# Get PI ------------------------------------------------------------------

.get_predicted_pi_glm <- function(x, predictions, ci = 0.95, ...) {
  info <- model_info(x)
  linkfun <- link_function(x)
  linkinv <- link_inverse(x)
  alpha <- 1 - ci
  prob <- c(alpha / 2, 1 - alpha / 2)

  if (info$is_binomial) {
    p <- linkinv(predictions)
    ci_low <- stats::qbinom(prob[1], size = 1, prob = p)
    ci_high <- stats::qbinom(prob[2], size = 1, prob = p)
  } else if (info$is_poisson) {
    rate <- linkinv(predictions)
    ci_low <- stats::qpois(prob[1], lambda = rate)
    ci_high <- stats::qpois(prob[2], lambda = rate)
  }

  data.frame(
    CI_low = linkfun(ci_low),
    CI_high = linkfun(ci_high)
  )
}




# Interval helpers --------------------------------------------------------

.get_predicted_se_from_iter <- function(iter, dispersion_method = "SD") {
  data <- as.data.frame(t(iter)) # Reshape

  # Dispersion
  if (is.character(dispersion_method)) {
    dispersion_method <- match.arg(tolower(dispersion_method), c("sd", "mad"))
    if (dispersion_method == "sd") {
      se <- apply(data, 2, stats::sd)
    } else if (dispersion_method == "mad") {
      se <- apply(data, 2, stats::mad)
    } else {
      stop("`dispersion_method` argument not recognized.")
    }
  } else {
    se <- apply(data, 2, dispersion_method)
  }
  data.frame(SE = se, row.names = 1:length(se))
}




.get_predicted_ci_from_iter <- function(iter, ci = 0.95, ci_method = "quantile") {

  # Interval
  ci_method <- match.arg(tolower(ci_method), c("quantile", "hdi", "eti"))
  if (ci_method == "quantile") {
    out <- data.frame(Parameter = 1:nrow(iter))
    for (i in ci) {
      temp <- data.frame(
        CI_low = apply(iter, 1, stats::quantile, probs = (1 - i) / 2, na.rm = TRUE),
        CI_high = apply(iter, 1, stats::quantile, probs = (1 + i) / 2, na.rm = TRUE)
      )
      names(temp) <- paste0(c("CI_low_", "CI_high_"), i)
      out <- cbind(out, temp)
    }
    if (length(ci) == 1) names(out) <- c("Parameter", "CI_low", "CI_high")
  } else {
    # installed?
    check_if_installed(c("bayestestR", "datawizard"))
    out <- as.data.frame(bayestestR::ci(as.data.frame(t(iter)), ci = ci, method = ci_method))
    if (length(ci) > 1) out <- datawizard::reshape_ci(out)
  }
  out$Parameter <- out$CI <- NULL
  row.names(out) <- NULL
  out
}
