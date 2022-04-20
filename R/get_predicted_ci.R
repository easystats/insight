#' Confidence intervals around predicted values
#'
#' @inheritParams get_predicted
#' @param predictions A vector of predicted values (as obtained by
#'   `stats::fitted()`, `stats::predict()` or
#'   [get_predicted()]).
#' @param se Numeric vector of standard error of predicted values. If `NULL`,
#'   standard errors are calculated based on the variance-covariance matrix.
#' @inheritParams get_predicted
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
#' @export
get_predicted_ci <- function(x, predictions = NULL, ...) {
  UseMethod("get_predicted_ci")
}




# General method ----------------------------------------------------------

#' @rdname get_predicted_ci
#' @export
get_predicted_ci.default <- function(x,
                                     predictions = NULL,
                                     data = NULL,
                                     se = NULL,
                                     ci = 0.95,
                                     ci_type = "confidence",
                                     ci_method = NULL,
                                     dispersion_method = "sd",
                                     vcov = NULL,
                                     vcov_args = NULL,
                                     ...) {

  # sanity check, if CI should be skipped
  if (is.null(ci)) {
    return(ci)
  }

  # default ci_method depends on the type of predictions and model
  if (is.null(ci_method)) {
    if ("iterations" %in% names(attributes(predictions))) {
      ci_method <- "quantile"
    } else {
      ci_method <- "gaussian"
    }
  }

  # If draws are present (bootstrapped or Bayesian)
  if ("iterations" %in% names(attributes(predictions))) {
    iter <- attributes(predictions)$iteration
    se <- .get_predicted_se_from_iter(iter = iter, dispersion_method)
    out <- .get_predicted_ci_from_iter(iter = iter, ci = ci, ci_method = ci_method)
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
      ci_method = ci_method,
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
    out <- ci_function(x, predictions, ci = ci, se = se, ci_method = ci_method, data = data, ...)
  } else {
    out <- data.frame(SE = se)
    for (ci_val in ci) {
      temp <- ci_function(x, predictions, ci = ci_val, se = se, ci_method = ci_method, data = data, ...)
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
                                    ci_method = "quantile",
                                    data = NULL,
                                    ...) {

  # TODO: Prediction interval for binomial: https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/
  # TODO: Prediction interval for poisson: https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-ii/

  dots <- list(...)

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

  # data is required for satterthwaite
  if (isTRUE(ci_method %in% c("satterthwaite", "kenward-roger"))) {
    dof <- get_df(x, type = ci_method, data = data)
  } else {
    dof <- get_df(x)
  }

  # Return NA
  if (is.null(se)) {
    se <- ci_low <- ci_high <- rep(NA, length(predictions))

    # Get CI
    # TODO: Does this cover all the model families?
  } else {
    # use `all()` because `dof` can be a vector with satterthwaite
    if (is.null(dof) || all(is.infinite(dof)) || find_statistic(x) == "z-statistic") {
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
  ci_method <- match.arg(
    tolower(ci_method),
    c("quantile", "hdi", "eti", "satterthwaite", "gaussian")
  )

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
