#' Confidence and Prediction Interval for Model Predictions
#'
#' Returns the Confidence (or Prediction) Interval (CI) associated with
#' predictions made by a model.
#'
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
#' @param vcov_estimation Either a matrix, or a string, indicating the suffix
#'   of the `vcov*()`-function from the \pkg{sandwich} or \pkg{clubSandwich}
#'   package, e.g. `vcov_estimation = "CL"` (which calls
#'   [sandwich::vcovCL()] to compute clustered covariance matrix
#'   estimators), or `vcov_estimation = "HC"` (which calls
#'   [sandwich::vcovHC()] to compute heteroskedasticity-consistent covariance
#'   matrix estimators).
#' @param vcov_type Character vector, specifying the estimation type for the
#'   robust covariance matrix estimation (see
#'   [sandwich::vcovHC()] or `clubSandwich::vcovCR()`
#'   for details). Only applies if `vcov_estimation` is a string, and not a matrix.
#' @param vcov_args List of named vectors, used as additional arguments that are
#'   passed down to the \pkg{sandwich}-function specified in
#'   `vcov_estimation`. Only applies if `vcov_estimation` is a string, and not
#'   a matrix.
#' @param dispersion_method,ci_method These arguments are only used in
#'   the context of bootstrapped and Bayesian models. Possible values are
#'   `dispersion_method = c("sd", "mad")` and
#'   `ci_method = c("quantile", "hdi", "eti")`. For the latter, the
#'   \pkg{bayestestR} package is required.
#' @param ... Not used for now.
#'
#'
#' @return The Confidence (or Prediction) Interval (CI).
#'
#'
#' @examples
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
#' predictions <- get_predicted(x, iterations = 500)
#' get_predicted_ci(x, predictions)
#'
#' if (require("datawizard")) {
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
                                     ci = 0.95,
                                     ci_type = "confidence",
                                     vcov_estimation = NULL,
                                     vcov_type = NULL,
                                     vcov_args = NULL,
                                     dispersion_method = "sd",
                                     ci_method = "quantile",
                                     ...) {
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
  if (ci_type == "confidence" || get_family(x)$family %in% c("gaussian") || (!is.null(vcov_estimation) && is.matrix(vcov_estimation))) { # gaussian or CI
    se <- get_predicted_se(
      x,
      predictions,
      data = data,
      ci_type = ci_type,
      vcov_estimation = vcov_estimation,
      vcov_type = vcov_type,
      vcov_args = vcov_args
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


# Specific definitions ----------------------------------------------------

#' @export
get_predicted_ci.hurdle <- function(x,
                                    predictions = NULL,
                                    data = NULL,
                                    ci = 0.95,
                                    ci_type = "confidence",
                                    vcov_estimation = NULL,
                                    vcov_type = NULL,
                                    vcov_args = NULL,
                                    predict_arg = "count",
                                    ...) {

  if (inherits(x, "hurdle") && predict_arg == "zero") {
    # nothing...
    linv <- function(x) x
  } else if (predict_arg == "zero") {
    linv <- stats::plogis
    # need back-transformation
    predictions <- stats::qlogis(as.vector(predictions))
  } else {
    linv <- exp
    # need back-transformation
    predictions <- log(as.vector(predictions))
  }

  # Analytical solution
  se <- get_predicted_se(
    x,
    predictions,
    data = data,
    ci_type = ci_type,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )

  # 2. Run it once or multiple times if multiple CI levels are requested
  if (is.null(ci)) {
    out <- data.frame(SE = se)
  } else if (length(ci) == 1) {
    out <- .get_predicted_se_to_ci_zeroinfl(x, predictions, ci = ci, se = se, link_inv = linv)
  } else {
    out <- data.frame(SE = se)
    for (ci_val in ci) {
      temp <- .get_predicted_se_to_ci_zeroinfl(x, predictions, ci = ci, se = se, link_inv = linv)
      temp$SE <- NULL
      names(temp) <- paste0(names(temp), "_", ci_val)
      out <- cbind(out, temp)
    }
  }
  out
}

#' @export
get_predicted_ci.zeroinfl <- get_predicted_ci.hurdle


#' @export
get_predicted_ci.mlm <- function(x, ...) {
  stop("TBD")
}


# Get Variance-covariance Matrix ---------------------------------------------------


.get_predicted_ci_vcov <- function(x,
                                   vcov_estimation = NULL,
                                   vcov_type = NULL,
                                   vcov_args = NULL) {

  # (robust) variance-covariance matrix
  if (!is.null(vcov_estimation) && !is.matrix(vcov_estimation)) {
    # check for existing vcov-prefix
    if (!grepl("^vcov", vcov_estimation)) {
      vcov_estimation <- paste0("vcov", vcov_estimation)
    }

    # set default for clubSandwich
    if (vcov_estimation == "vcovCR" && is.null(vcov_type)) {
      vcov_type <- "CR0"
    }

    if (!is.null(vcov_type) && vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
      # installed?
      check_if_installed("clubSandwich")
      robust_package <- "clubSandwich"
      vcov_estimation <- "vcovCR"
    } else {
      # installed?
      check_if_installed("sandwich")
      robust_package <- "sandwich"
    }

    # compute robust standard errors based on vcov
    if (robust_package == "sandwich") {
      vcov_estimation <- get(vcov_estimation, asNamespace("sandwich"))
      vcovmat <- as.matrix(do.call(vcov_estimation, c(list(x = x, type = vcov_type), vcov_args)))
    } else {
      vcov_estimation <- clubSandwich::vcovCR
      vcovmat <- as.matrix(do.call(vcov_estimation, c(list(obj = x, type = vcov_type), vcov_args)))
    }
  } else if (!is.matrix(vcov_estimation)) {
    # get variance-covariance-matrix, depending on model type
    vcovmat <- get_varcov(x, component = "conditional")
  } else {
    vcovmat <- vcov_estimation
  }

  vcovmat
}


# Get Model matrix ------------------------------------------------------------


.get_predicted_ci_modelmatrix <- function(x, data = NULL, vcovmat = NULL, ...) {
  resp <- find_response(x)
  if (is.null(vcovmat)) vcovmat <- .get_predicted_ci_vcov(x, ...)


  if (is.null(data)) {
    mm <- get_modelmatrix(x)
  } else {
    if (!all(resp %in% data)) data[[resp]] <- 0 # fake response
    # else, model.matrix below fails, e.g. for log-terms
    attr(data, "terms") <- NULL

    # In these models we need to drop offset from model_terms. To do this, we
    # must construct the mm by calling `get_modelmatrix` on modified model
    # terms.  When we do not need to drop offset terms, we call get_modelmatrix
    # on the model itself. The latter strategy is safer in cases where `data`
    # does not include all the levels of a factor variable.
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc"))) {

      # model terms, required for model matrix
      model_terms <- tryCatch(
        {
          stats::terms(x)
        },
        error = function(e) {
          find_formula(x)$conditional
        }
      )

      all_terms <- find_terms(x)$conditional
      off_terms <- grepl("^offset\\((.*)\\)", all_terms)
      if (any(off_terms)) {
        all_terms <- all_terms[!off_terms]
        # TODO: preserve interactions
        vcov_names <- dimnames(vcovmat)[[1]][grepl(":", dimnames(vcovmat)[[1]], fixed = TRUE)]
        if (length(vcov_names)) {
          vcov_names <- gsub(":", "*", vcov_names, fixed = TRUE)
          all_terms <- unique(c(all_terms, vcov_names))
        }
        off_terms <- grepl("^offset\\((.*)\\)", all_terms)
        model_terms <- stats::reformulate(all_terms[!off_terms], response = find_response(x))
      }

      # check for at least to factor levels, in order to build contrasts
      single_factor_levels <- sapply(data, function(i) is.factor(i) && nlevels(i) == 1)
      if (any(single_factor_levels)) {
        warning(format_message("Some factors in the data have only one level. Cannot compute model matrix for standard errors and confidence intervals."), call. = FALSE)
        return(NULL)
      }

      mm <- get_modelmatrix(model_terms, data = data)
    } else {
      mm <- get_modelmatrix(x, data = data)
    }
  }

  # fix rank deficiency
  if (ncol(vcovmat) < ncol(mm)) {
    mm <- mm[, intersect(colnames(mm), colnames(vcovmat))]
  }

  mm
}



# Get SE ------------------------------------------------------------------

get_predicted_se <- function(x,
                             predictions = NULL,
                             data = NULL,
                             ci_type = "confidence",
                             vcov_estimation = NULL,
                             vcov_type = NULL,
                             vcov_args = NULL) {

  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.

  vcovmat <- .get_predicted_ci_vcov(
    x,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )
  mm <- .get_predicted_ci_modelmatrix(x, data = data, vcovmat = vcovmat)

  # return NULL for fail
  if (is.null(mm)) {
    return(NULL)
  }

  if (ncol(mm) != ncol(vcovmat)) {
    # last desperate try
    mm_full <- get_modelmatrix(x)
    mm <- tryCatch(
      {
        mm_full[as.numeric(row.names(get_modelmatrix(x, data = data))), , drop = FALSE]
      },
      error = function(e) {
        NULL
      }
    )
    # still no match?
    if (isTRUE(ncol(mm) != ncol(vcovmat))) {
      warning(format_message("Could not compute standard errors or confidence intervals because the model and variance-covariance matrices are non-conformable. This can sometimes happen when the `data` used to make predictions fails to include all the levels of a factor variable or all the interaction components."), call. = FALSE)
      return(NULL)
    }
  }

  # compute vcov for predictions
  # Next line equivalent to: diag(M V M')
  var_diag <- colSums(t(mm %*% vcovmat) * t(mm))

  # add sigma to standard errors, i.e. confidence or prediction intervals
  ci_type <- match.arg(ci_type, c("confidence", "prediction"))
  if (ci_type == "prediction") {
    if (is_mixed_model(x)) {
      se <- sqrt(var_diag + get_variance_residual(x))
    } else {
      se <- sqrt(var_diag + get_sigma(x)^2)
    }
  } else {
    se <- sqrt(var_diag)
  }

  se
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

    ci_low <- predictions - (se * crit_val)
    ci_high <- predictions + (se * crit_val)
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
