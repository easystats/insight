#' Confidence and Prediction Interval for Model Predictions
#'
#' Returns the Confidence (or Prediction) Interval (CI) associated with
#' predictions made by a model.
#'
#' @inheritParams get_predicted
#' @param predictions A vector of predicted values (as obtained by
#'   \code{stats::fitted()}, \code{stats::predict()} or
#'   \code{\link{get_predicted}}).
#' @param ci The interval level (default \code{0.95}, i.e., 95\% CI).
#' @param ci_type Can be \code{"prediction"} or \code{"confidence"}. Prediction
#'   intervals show the range that likely contains the value of a new
#'   observation (in what range it would fall), whereas confidence intervals
#'   reflect the uncertainty around the estimated parameters (and gives the
#'   range of the link; for instance of the regression line in a linear
#'   regressions). Prediction intervals account for both the uncertainty in the
#'   model's parameters, plus the random variation of the individual values.
#'   Thus, prediction intervals are always wider than confidence intervals.
#'   Moreover, prediction intervals will not necessarily become narrower as the
#'   sample size increases (as they do not reflect only the quality of the fit).
#'   This applies mostly for "simple" linear models (like \code{lm}), as for
#'   other models (e.g., \code{glm}), prediction intervals are somewhat useless
#'   (for instance, for a binomial model for which the dependent variable is a
#'   vector of 1s and 0s, the prediction interval is... \code{[0, 1]}).
#' @param vcov_estimation String, indicating the suffix of the
#'   \code{vcov*()}-function from the \pkg{sandwich} or \pkg{clubSandwich}
#'   package, e.g. \code{vcov_estimation = "CL"} (which calls
#'   \code{\link[sandwich]{vcovCL}} to compute clustered covariance matrix
#'   estimators), or \code{vcov_estimation = "HC"} (which calls
#'   \code{\link[sandwich:vcovHC]{vcovHC()}} to compute
#'   heteroskedasticity-consistent covariance matrix estimators).
#' @param vcov_type Character vector, specifying the estimation type for the
#'   robust covariance matrix estimation (see
#'   \code{\link[sandwich:vcovHC]{vcovHC()}} or \code{clubSandwich::vcovCR()}
#'   for details).
#' @param vcov_args List of named vectors, used as additional arguments that are
#'   passed down to the \pkg{sandwich}-function specified in
#'   \code{vcov_estimation}.
#' @param dispersion_function,interval_function These arguments are only used in
#'   the context of bootstrapped and Bayesian models. Possible values are
#'   \code{dispersion_function = c("sd", "mad")} and
#'   \code{interval_function = c("quantile", "hdi", "eti")}. For the latter, the
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
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' predictions <- predict(x)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
#' head(ci_vals)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
#' head(ci_vals)
#'
#' predictions <- get_predicted(x, iterations = 500)
#' get_predicted_ci(x, predictions)
#'
#' if (require("bayestestR")) {
#'   ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
#'   head(ci_vals)
#'   bayestestR::reshape_ci(ci_vals)
#'
#'   ci_vals <- get_predicted_ci(x,
#'     predictions,
#'     dispersion_function = "MAD",
#'     interval_function = "HDI"
#'   )
#'   head(ci_vals)
#' }
#'
#'
#' # Logistic model
#' x <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' predictions <- predict(x, type = "link")
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
#' head(ci_vals)
#' ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
#' head(ci_vals)
#' @export
get_predicted_ci <- function(x,
                             predictions = NULL,
                             data = NULL,
                             ci = 0.95,
                             ci_type = "confidence",
                             vcov_estimation = NULL,
                             vcov_type = NULL,
                             vcov_args = NULL,
                             dispersion_function = "sd",
                             interval_function = "quantile",
                             ...) {

  # If draws are present
  if ("iterations" %in% names(attributes(predictions))) {
    iter <- attributes(predictions)$iteration

    se <- .get_predicted_se_from_iter(iter = iter, dispersion_function)
    out <- .get_predicted_interval_from_iter(iter = iter, ci = ci, interval_function)

    return(cbind(se, out))
  }

  # Analytical solution
  if (ci_type == "confidence" || get_family(x)$family %in% c("gaussian")) { # gaussian or CI
    se <- .get_predicted_ci_se(x, predictions, data = data, ci_type = ci_type, vcov_estimation = vcov_estimation, vcov_type = vcov_type, vcov_args = vcov_args)
    return(.get_predicted_se_to_ci(x, predictions, se = se, ci = ci))
  } else {
    return(.get_predicted_pi_glm(x, predictions, ci = ci))
  }
}



# Get Variance-covariance Matrix ---------------------------------------------------


.get_predicted_ci_vcov <- function(x, vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL) {

  # (robust) variance-covariance matrix
  if (!is.null(vcov_estimation)) {
    # check for existing vcov-prefix
    if (!grepl("^vcov", vcov_estimation)) {
      vcov_estimation <- paste0("vcov", vcov_estimation)
    }
    # set default for clubSandwich
    if (vcov_estimation == "vcovCR" && is.null(vcov_type)) {
      vcov_type <- "CR0"
    }
    if (!is.null(vcov_type) && vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
      if (!requireNamespace("clubSandwich", quietly = TRUE)) {
        stop("Package `clubSandwich` needed for this function. Please install and try again.")
      }
      robust_package <- "clubSandwich"
      vcov_estimation <- "vcovCR"
    } else {
      if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("Package `sandwich` needed for this function. Please install and try again.")
      }
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
  } else {
    # get variance-covariance-matrix, depending on model type
    vcovmat <- get_varcov(x, component = "conditional")
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

    # model terms, required for model matrix
    model_terms <- tryCatch(
      {
        stats::terms(x)
      },
      error = function(e) {
        find_formula(x)$conditional
      }
    )

    # drop offset from model_terms
    if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc"))) {
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
    }
    mm <- get_modelmatrix(model_terms, data = data)
  }
  mm
}








# Get SE ------------------------------------------------------------------




.get_predicted_ci_se <- function(x, predictions = NULL, data = NULL, ci_type = "confidence", vcov_estimation = NULL, vcov_type = NULL, vcov_args = NULL) {

  # Matrix-multiply X by the parameter vector B to get the predictions, then
  # extract the variance-covariance matrix V of the parameters and compute XVX'
  # to get the variance-covariance matrix of the predictions. The square-root of
  # the diagonal of this matrix represent the standard errors of the predictions,
  # which are then multiplied by 1.96 for the confidence intervals.

  vcovmat <- .get_predicted_ci_vcov(x, vcov_estimation = vcov_estimation, vcov_type = vcov_type, vcov_args = vcov_args)
  mm <- .get_predicted_ci_modelmatrix(x, data = data, vcovmat = vcovmat)

  # compute vcov for predictions
  var_matrix <- mm %*% vcovmat %*% t(mm)

  # add sigma to standard errors, i.e. confidence or prediction intervals
  ci_type <- match.arg(ci_type, c("confidence", "prediction"))
  if (ci_type == "prediction") {
    if (is_mixed_model(x)) {
      se <- sqrt(diag(var_matrix) + get_variance_residual(x))
    } else {
      se <- sqrt(diag(var_matrix) + get_sigma(x)^2)
    }
  } else {
    se <- sqrt(diag(var_matrix))
  }

  se
}



## Convert to CI -----------



.get_predicted_se_to_ci <- function(x, predictions = NULL, se = NULL, ci = 0.95) {

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
    ci_low <- ci_high <- rep(NA, length(predictions))

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



# Get PI ------------------------------------------------------------------

.get_predicted_pi_glm <- function(x, predictions, ci = 0.95) {
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

.get_predicted_se_from_iter <- function(iter, dispersion_function = "SD") {
  data <- as.data.frame(t(iter)) # Reshape

  # Dispersion
  if (is.character(dispersion_function)) {
    dispersion_function <- match.arg(tolower(dispersion_function), c("sd", "mad"))
    if (dispersion_function == "sd") {
      se <- apply(data, 2, stats::sd)
    } else if (dispersion_function == "mad") {
      se <- apply(data, 2, stats::mad)
    } else {
      stop("`dispersion_function` argument not recognized.")
    }
  } else {
    se <- apply(data, 2, dispersion_function)
  }
  data.frame(SE = se)
}



.get_predicted_interval_from_iter <- function(iter, ci = 0.95, interval_function = "quantile") {

  # Interval
  interval_function <- match.arg(tolower(interval_function), c("quantile", "hdi", "eti"))
  if (interval_function == "quantile") {
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
    if (!requireNamespace("bayestestR", quietly = TRUE)) {
      stop("Package `bayestestR` needed for this function. Please install and try again.")
    }
    out <- as.data.frame(bayestestR::ci(as.data.frame(t(iter)), ci = ci, method = interval_function))
    if (length(ci) > 1) out <- reshape_ci(out)
  }
  out$Parameter <- out$CI <- NULL
  row.names(out) <- NULL
  out
}
