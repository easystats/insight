# Mixed Models (lme4, glmmTMB) ------------------------------------------
# =======================================================================

#' @export
get_predicted.lmerMod <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = 0.95,
                                  include_random = TRUE,
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {

  # Sanitize input
  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # we have now a validated "predict"...
  predict <- args$predict

  # Make prediction only using random if only random
  if (all(names(args$data) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Prediction function
  predict_function <- function(x, ...) {
    stats::predict(
      x,
      newdata = args$data,
      type = args$type,
      re.form = args$re.form,
      random.only = random.only,
      allow.new.levels = args$allow_new_levels,
      ...
    )
  }

  if (is.null(iterations)) {
    predictions <- predict_function(x)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  ci_data <- get_predicted_ci(x, predictions, data = args$data, ci = ci, ci_type = args$ci_type, ...)
  out <- .get_predicted_transform(x, predictions, args, ci_data)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.merMod <- get_predicted.lmerMod



#' @export
get_predicted.glmmTMB <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = 0.95,
                                  include_random = TRUE,
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {
  dots <- list(...)

  # Sanity checks
  if (!is.null(predict) && predict %in% c("prediction", "predicted", "classification")) {
    predict <- "expectation"
    if (verbose) {
      warning(
        format_message(
          '"prediction" and "classification" are currently not supported by the',
          '`predict` argument for glmmTMB models. Changing to `predict="expectation"`.'
        ),
        call. = FALSE
      )
    }
  }

  # TODO: prediction intervals
  # https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

  # Sanitize input
  args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # we have now a validated "predict"...
  predict <- args$predict

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type = args$type,
      re.form = args$re.form,
      unconditional = FALSE,
      ...
    )
  }

  # Get prediction
  rez <- predict_function(x, data = args$data, se.fit = TRUE)

  if (is.null(iterations) || predict %in% c("expectation", "response")) {
    predictions <- as.numeric(rez$fit)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # "expectation" for zero-inflated? we need a special handling
  # for predictions and CIs here.

  if (predict %in% c("expectation", "response") && args$info$is_zero_inflated) {
    zi_predictions <- stats::predict(
      x,
      newdata = data,
      type = "zprob",
      re.form = args$re.form,
      unconditional = FALSE,
      ...
    )
    predictions <- link_inverse(x)(predictions) * (1 - as.vector(zi_predictions))
    ci_data <- .simulate_zi_predictions(model = x, newdata = data, predictions = predictions, nsim = iterations, ci = ci)
    out <- list(predictions = predictions, ci_data = ci_data)
  } else {
    # Get CI
    ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
    out <- .get_predicted_transform(x, predictions, args, ci_data)
  }

  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}
