#' Compute Model's Predictions
#'
#' Compute Model's Predictions.
#'
#' @param x A statistical model (can also be a data.frame, in which case the second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which to predict. If omitted, the data used to fit the model is used.
#' @param predict Can be \code{"link"}, \code{"relation"} (default), or \code{"prediction"}. This modulates the scale of the output as well as the type of certainty interval. More specifically, \code{"link"} gives an output on the link-scale (for logistic models, that means the log-odds scale) with a confidence interval (CI). \code{"relation"} (default) also returns confidence intervals, but this time the output is on the response scale (for logistic models, that means probabilities). Finally, \code{"predict"} also gives an output on the response scale, but this time associated with a prediction interval (PI), which is larger than a confidence interval (though it mostly make sense for linear models). gives Read more about in the \strong{Details} section below.
#' @param iterations For Bayesian models, this corresponds to the number of posterior draws. If \code{NULL}, will return all the draws (one for each iteration of the model). For frequentist models, if not \code{NULL}, will generate bootstrapped draws, from which bootstrapped CIs will be computed.
#' @param include_random If \code{TRUE} (default), include all random effects in the prediction. If \code{FALSE}, don't take them into account. Can also be a formula to specify which random effects to condition on when predicting (passed to the \code{re.form} argument). If \code{include_random = TRUE} and \code{newdata} is provided, make sure to include the random effect variables in \code{newdata} as well.
#' @param include_smooth For General Additive Models (GAMs). If \code{FALSE}, will fix the value of the smooth to its average, so that the predictions are not depending on it.
#' @param centrality_function The function used to obtain a centrality estimate for bootstrapped or Bayesian models. Usually \code{median()} (default), \code{mean()}, or \code{bayestestR::map_estimate()}.
#' @param ... Other argument to be passed for instance to \code{\link{get_predicted_ci}}.
#'
#' @seealso get_predicted_ci
#'
#' @return The fitted values (i.e. predictions for the response). For Bayesian or bootstrapped models (when \code{iterations != NULL}), this will be a dataframe with all iterations as columns (observations are still rows).
#'
#' @details
#' The \code{predict} argument jointly modulates two separate concepts, the \strong{scale} and the \strong{uncertainty interval}.
#'
#' \subsection{Confidence Interval vs. Prediction Interval)}{
#' \itemize{
#'   \item \strong{Linear models} - \code{lm()}: For linear models, Prediction intervals (\code{predict = "prediction"}) show the range that likely contains the value of a new observation (in what range it is likely to fall), whereas confidence intervals (\code{predict = "relation"} or \code{predict = "link"}) reflect the uncertainty around the estimated parameters (and gives the range of uncertainty of the regression line). lIn general, Prediction Intervals (PIs) account for both the uncertainty in the model's parameters, plus the random variation of the individual values. Thus, prediction intervals are always wider than confidence intervals. Moreover, prediction intervals will not necessarily become narrower as the sample size increases (as they do not reflect only the quality of the fit, but also the variability within the data).
#'   \item \strong{General Linear models} - \code{glm()}: For binomial models, prediction intervals are somewhat useless (for instance, for a binomial model for which the dependent variable is a vector of 1s and 0s, the prediction interval is... \code{[0, 1]}).
#' }}
#'
#'
#' \subsection{Link scale vs. Response scale}{
#' Having the output is on the scale of the response variable is arguably the most convenient to understand and visualize the relationships. If on the link-scale, no transformation is applied and the values are on the scale of the model's predictors. For instance, for a logistic model, the response scale corresponds to the predicted probabilities, whereas the link-scale makes predictions of log-odds (probabilities on the logit scale).
#' }
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' predictions <- get_predicted(x)
#' predictions
#'
#' get_predicted(x, predict = "prediction")
#'
#' # Get CI
#' as.data.frame(predictions)
#'
#' # Bootsrapped
#' as.data.frame(get_predicted(x, iterations = 100), include_iterations = FALSE)
#'
#' \dontrun{
#' # Bayesian models
#' if (require("rstanarm")) {
#'   x <- stan_glm(mpg ~ am, data = mtcars, refresh = 0)
#'   predictions <- get_predicted(x)
#'   predictions
#'   as.data.frame(predictions, include_iterations = FALSE)
#' }
#' }
#' @export
get_predicted <- function(x, data = NULL, ...) {
  UseMethod("get_predicted")
}





# default methods ---------------------------


#' @importFrom stats fitted predict
#' @export
get_predicted.default <- function(x, data = NULL, ...) {
  out <- tryCatch(
    {
      if (!is.null(data)) {
        stats::predict(x, newdata = data, ...)
      } else {
        stats::predict(x, ...)
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    out <- tryCatch(
      {
        stats::fitted(x)
      },
      error = function(e) {
        NULL
      }
    )
  }
  out
}

#' @export
get_predicted.data.frame <- function(x, data = NULL, ...) {
  # This makes it pipe friendly; data %>% get_predicted(model)
  if (is.null(data)) {
    stop("Please provide a model to base the estimations on.")
  } else {
    get_predicted(data, x, ...)
  }
}



# LM and GLM --------------------------------------------------------------
# =========================================================================


#' @rdname get_predicted
#' @export
get_predicted.lm <- function(x, data = NULL, predict = "relation", iterations = NULL, ...) {
  args <- .get_predicted_args(x, data = data, predict = predict, ...)

  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, interval = "none", type = args$type, ...)
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x, data = args$data)
  } else {
    predictions <- .get_predicted_boot(x, data = args$data, predict_function = predict_function, iterations = iterations, ...)
  }

  ci_data <- get_predicted_ci(x, predictions, data = args$data, ci_type = args$ci_type, ...)

  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.glm <- get_predicted.lm







# Mixed Models (lme4, glmmTMB) ------------------------------------------
# =======================================================================

#' @export
get_predicted.lmerMod <- function(x, data = NULL, predict = "relation", ci = 0.95, include_random = TRUE, iterations = NULL, ...) {

  # Sanitize input
  args <- .get_predicted_args(x, data = data, predict = predict, ci = ci, include_random = include_random, ...)

  # Make prediction only using random if only random
  if (all(names(args$data) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Prediction function
  predict_function <- function(x, ...) {
    stats::predict(x, newdata = args$data, type = args$type, re.form = args$re.form, random.only = random.only, ...)
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x)
  } else {
    predictions <- .get_predicted_boot(x, data = args$data, predict_function = predict_function, iterations = iterations, ...)
  }

  ci_data <- get_predicted_ci(x, predictions, data = args$data, ci_type = args$ci_type, ...)

  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.merMod <- get_predicted.lmerMod







#' @export
get_predicted.glmmTMB <- function(x, data = NULL, predict = "relation", ci = 0.95, include_random = TRUE, iterations = NULL, ...) {

  # Sanity checks
  if(predict == "prediction") {
    warning("predict = 'prediction' is currently not available for glmmTMB models. Changing to 'relation'.")
    predict <- "relation"
  }

  # Sanitize input
  args <- .get_predicted_args(x, data = data, predict = predict, ci = ci, include_random = include_random, ...)

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, type = args$type, re.form = args$re.form, unconditional = FALSE, ...)
  }

  # Get prediction
  rez <- predict_function(x, data = args$data, se.fit = TRUE)
  if (is.null(iterations)) {
    predictions <- as.numeric(rez$fit)
  } else {
    predictions <- .get_predicted_boot(x, data = args$data, predict_function = predict_function, iterations = iterations, ...)
  }

  # Get CI
  ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}






# GAM -------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.gam <- function(x, data = NULL, predict = "relation", ci = 0.95, include_random = TRUE, include_smooth = TRUE, iterations = NULL, ...) {

  # Sanity checks
  if(predict == "prediction") {
    warning("predict = 'prediction' is currently not available for GAM models. Changing to 'relation'.")
    predict <- "relation"
  }
  # TODO: check this for prediction intervals:
  # https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
  # https://github.com/gavinsimpson/gratia/blob/master/R/confint-methods.R
  # https://github.com/gavinsimpson/gratia/blob/master/R/posterior-samples.R

  # Sanitize input
  args <- .get_predicted_args(x, data = data, predict = predict, ci = ci, include_random = include_random, include_smooth = include_smooth, ...)
  if (inherits(x, c("gamm", "list"))) x <- x$gam



  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, type = args$type, re.form = args$re.form, unconditional = FALSE, ...)
  }

  # Get prediction
  rez <- predict_function(x, data = args$data, se.fit = TRUE)
  if (is.null(iterations)) {
    predictions <- rez$fit
  } else {
    predictions <- .get_predicted_boot(x, data = args$data, predict_function = predict_function, iterations = iterations, ...)
  }

  # Get CI
  ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4



# Bayesian --------------------------------------------------------------
# =======================================================================


#' @rdname get_predicted
#' @export
get_predicted.stanreg <- function(x, data = NULL, predict = "relation", iterations = NULL, include_random = TRUE, include_smooth = TRUE, centrality_function = stats::median, ...) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("Package `rstantools` needed for this function to work. Please install it.")
  }

  args <- .get_predicted_args(x, data = data, predict = predict, include_random = include_random, include_smooth = include_smooth, ...)

  # Get draws
  if (args$predict %in% c("link")) {
    draws <- rstantools::posterior_linpred(x, newdata = args$newdata, re.form = args$re.form, draws = iterations, ...)
  } else {
    draws <- rstantools::posterior_epred(x, newdata = args$newdata, re.form = args$re.form, draws = iterations, ...)
  }
  draws <- as.data.frame(t(draws))
  names(draws) <- gsub("^V(\\d+)$", "iter_\\1", names(draws))

  # Get predictions (summarize)
  predictions <- apply(draws, 1, centrality_function)
  attr(predictions, "iterations") <- draws

  # Output
  ci_data <- get_predicted_ci(x, predictions = predictions, data = args$data, ci_type = args$ci_type, ...)

  .get_predicted_out(predictions, args = args, ci_data = ci_data)
}


#' @export
get_predicted.brmsfit <- get_predicted.stanreg




# Other models ----------------------------------------------------------
# =======================================================================


#' @export
get_predicted.crr <- function(x, ...) {
  out <- as.data.frame(unclass(stats::predict(x, ...)))
  class(out) <- c("get_predicted", class(out))
  out
}




# ====================================================================
# Utils --------------------------------------------------------------
# ====================================================================


.format_reform <- function(include_random = TRUE) {

  # Format re.form
  if (is.null(include_random) || is.na(include_random)) {
    re.form <- include_random
  } else if (include_random == TRUE) {
    re.form <- NULL
  } else if (include_random == FALSE) {
    re.form <- NA
  } else {
    re.form <- include_random
  }
  re.form
}



.get_predicted_args <- function(x, data = NULL, predict = "relation", include_random = TRUE, include_smooth = TRUE, ci = 0.95, newdata = NULL, ...) {

  # Sanitize input
  predict <- match.arg(predict, c("link", "relation", "prediction"))
  # Other names: "response", "expected", "distribution", "observations"

  # Get info
  info <- model_info(x)

  # Data
  if(!is.null(newdata) && is.null(data)) data <- newdata
  if (is.null(data)) data <- get_data(x)

  # CI
  if (is.null(ci)) ci <- 0

  # Prediction and CI type
  if (predict == "link") {
    ci_type <- "confidence"
    scale <- "link"
  } else if (predict == "relation") {
    ci_type <- "confidence"
    scale <- "response"
  } else if (predict == "prediction") {
    ci_type <- "prediction"
    scale <- "response"
  }

  # Type (that's for the initial call to stats::predict)
  if (info$is_linear) {
    type <- "response"
  } else {
    type <- "link"
  }

  # Transform
  if(info$is_linear == FALSE && scale == "response") {
    transform <- TRUE
  } else {
    transform <- FALSE
  }

  # Smooth
  smooths <- clean_names(find_smooth(x, flatten = TRUE))
  if (!is.null(smooths)) {
    for (smooth in smooths) {
      # Fix smooth to average value
      if (!smooth %in% names(data) || include_smooth == FALSE) {
        include_smooth <- FALSE
        data[[smooth]] <- mean(get_data(x)[[smooth]], na.rm = TRUE)
      }
    }
  }

  # Random
  # In case include_random is TRUE, but there's actually no random factors in data
  if (include_random && !is.null(data) && !is.null(x) && !all(find_random(x, flatten = TRUE) %in% names(data))) {
    include_random <- FALSE
  }
  # Add (or set) random variables to "NA"
  if (include_random == FALSE) {
    data[find_variables(x, effects = "random")$random] <- NA
  }
  re.form <- .format_reform(include_random)

  # Return all args
  list(data = data, include_random = include_random, re.form = re.form, include_smooth = include_smooth, ci_type = ci_type, ci = ci, type = type, predict = predict, scale = scale, transform = transform, info = info)
}




.get_predicted_transform <- function(x, predictions, args = NULL, ci_data = NULL, ...) {

  # Transform to response scale
  if (args$transform == TRUE) {
    if (!is.null(ci_data)) {
      # Transform CI
      se_col <- names(ci_data) == "SE"
      ci_data[!se_col] <- as.data.frame(sapply(ci_data[!se_col], link_inverse(x)))

      # Transform SE (https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/predict.glm.R#L60)
      mu_eta <- abs(get_family(x)$mu.eta(predictions))
      ci_data[se_col] <- ci_data[se_col] * mu_eta
    }

    # Transform predictions
    predictions <- link_inverse(x)(predictions)

    # Transform iterations
    if ("iterations" %in% names(attributes(predictions))) {
      attr(predictions, "iterations") <- as.data.frame(sapply(attributes(predictions)$iterations, link_inverse(x)))
    }
  }

  list(predictions = predictions, ci_data = ci_data)
}


.get_predicted_out <- function(predictions, args = NULL, ci_data = NULL, ...) {

  if (!is.null(ci_data)) {
    attr(predictions, "ci_data") <- ci_data
  }
  if (!is.null(args)) {
    attr(predictions, "data") <- args$data
    attr(predictions, "ci") <- args$ci
    attr(predictions, "predict") <- args$predict
  }

  class(predictions) <- c("get_predicted", class(predictions))
  predictions
}

# Bootstrap ---------------------------------------------------------------



#' @importFrom stats predict update
.get_predicted_boot <- function(x, data = NULL, predict_function = NULL, iterations = 500, centrality_function = stats::median, ...) {
  if (is.null(data)) data <- get_data(x)

  # TODO: how to make it work with the seed argument??

  # Using bootMer
  if (inherits(x, "merMod")) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` needed for this function to work. Please install it.")
    }
    draws <- lme4::bootMer(x, predict_function, nsim = iterations, use.u = TRUE, ...)

    # Using boot
  } else {
    if (!requireNamespace("boot", quietly = TRUE)) {
      stop("Package `boot` needed for this function to work. Please install it.")
    }
    boot_fun <- function(data, indices, ...) {
      model <- stats::update(x, data = data[indices, ])
      predict_function(model, data = data, ...)
    }
    draws <- boot::boot(data, boot_fun, R = iterations, ...)
  }

  draws <- as.data.frame(t(draws$t))
  names(draws) <- paste0("iter_", 1:ncol(draws))
  draws

  predictions <- apply(draws, 1, centrality_function)
  attr(predictions, "iterations") <- draws
  predictions
}
