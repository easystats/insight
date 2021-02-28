#' Compute Model's Predictions
#'
#' Compute Model's Predictions.
#'
#' @param x A statistical model (can also be a data.frame, in which case the second argument has to be a model).
#' @param data An optional data frame in which to look for variables with which to predict. If omitted, the data used to fit the model is used.
#' @param type Can be \code{"response"} (default) or \code{"link"}. This is an important argument, read more about in the \strong{Details} section below.
#' @param iterations For Bayesian models, this corresponds to the number of posterior draws. If \code{NULL}, will return all the draws (one for each iteration of the model). For frequentist models, if not \code{NULL}, will generate bootstrapped draws, from which bootstrapped CIs will be computed.
#' @param ... Other args to be passed for instance to \code{\link{get_predicted_ci}}.
#'
#' @seealso get_predicted_ci
#'
#' @return The fitted values (i.e. predictions for the response). For Bayesian or bootstrapped models (when \code{iterations != NULL}), this will be a dataframe with all iterations as columns (observations are still rows).
#'
#' @details
#' \subsection{Prediction type ("response" vs. "link")}{
#' This argument has different effects depending on the model.
#' \itemize{
#'   \item \strong{Linear models} - \code{lm()}: For linear models, this argument impacts mainly the nature of the uncertainty interval (CI). Prediction intervals (\code{type = "response"}) show the range that likely contains the value of a new observation (in what range it is likely to fall), whereas confidence intervals (\code{type = "link"}) reflect the uncertainty around the estimated parameters (and gives the range of uncertainty of the regression line). Prediction intervals account for both the uncertainty in the model's parameters, plus the random variation of the individual values. Thus, prediction intervals are always wider than confidence intervals. Moreover, prediction intervals will not necessarily become narrower as the sample size increases (as they do not reflect only the quality of the fit, but also the variability within the data).
#'   \item \strong{General Linear models} - \code{glm()}: For other types of models (e.g., \code{glm}), this argument ... prediction intervals are somewhat useless (for instance, for a binomial model for which the dependent variable is a vector of 1s and 0s, the prediction interval is... \code{[0, 1]})
#' }}
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' predicted <- get_predicted_new(x)
#' predicted
#'
#'
#' get_predicted_new(x, type = "link")
#'
#' # Get CI
#' attributes(predicted)$CI_low # Or CI_high
#' as.data.frame(predicted) # To get everything
#'
#' get_predicted_new(x, iterations = 100)
#'
#'
#' @export
get_predicted_new <- function(x, data = NULL, ...) {
  UseMethod("get_predicted_new")
}


#' @rdname get_predicted_new
#' @export
get_predicted_new.lm <- function(x, data = NULL, type = "response", iterations = NULL, ...) {

  args <- .get_predicted_args(x, data = data, type = type)

  predict_function <- function(x, data, ...) {
    stats::predict(x, newdata = data, interval = "none", se.fit = FALSE, ...)
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x, data = args$data)
  } else {
    predictions <- .get_predicted_boot(x, data = args$data, predict_function = predict_function, iterations = iterations, ...)
  }

  ci_vals <- get_predicted_ci(x, predictions, data = args$data, ci_type = args$ci_type, ...)

  .get_predicted_out(predictions, as.list(ci_vals), args)
}




# Initialize --------------------------------------------------------------


.get_predicted_args <- function(x, data = NULL, type = "response", include_random = TRUE, include_smooth = TRUE, ...) {

  # Data
  if (is.null(data)) data <- get_data(x)

  # Prediction and CI type
  if (type == "response") {
    ci_type <- "prediction"
  } else {
    ci_type <- "confidence"
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

  re.form <- .format_reform(include_random)

  list(data = data, include_random = include_random, re.form = re.form, include_smooth = include_smooth, ci_type = ci_type)
}





.get_predicted_out <- function(out, ...) {
  attributes(out) <- c(attributes(out), as.list(...))
  class(out) <- c("get_predicted", class(out))
  out
}

# Bootstrap ---------------------------------------------------------------



#' @importFrom stats predict update
.get_predicted_boot <- function(x, data = NULL, predict_function = NULL, iterations = 500, ...) {
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
}
