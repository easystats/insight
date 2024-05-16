# pscl: hurdle zeroinfl -------------------------------------------------
# =======================================================================

#' @export
get_predicted.hurdle <- function(x,
                                 data = NULL,
                                 predict = "expectation",
                                 ci = 0.95,
                                 iterations = NULL,
                                 verbose = TRUE,
                                 ...) {
  # pscl models return the fitted values immediately and ignores the `type`
  # argument when `data` is NULL
  if (is.null(data)) {
    data <- get_data(x, verbose = FALSE)
  }

  dots <- list(...)

  # Sanitize input
  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    verbose = verbose,
    ...
  )

  # we have now a validated "predict"...
  predict <- my_args$predict

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type = my_args$type,
      ...
    )
  }

  # 1. step: predictions
  predictions <- as.vector(predict_function(x, data = my_args$data))

  # on the response scale, we simulate predictions for CIs...
  if (my_args$scale == "response") {
    # intermediate step: predictions for ZI model
    zi_predictions <- stats::predict(
      x,
      newdata = my_args$data,
      type = "zero",
      ...
    )

    if (inherits(x, "hurdle")) {
      predictions <- predictions * as.vector(zi_predictions)
    } else {
      predictions <- predictions * (1 - as.vector(zi_predictions))
    }

    # 2. and 3. step: confidence intervals and backtransform
    ci_data <- .simulate_zi_predictions(
      model = x,
      newdata = data,
      predictions = predictions,
      nsim = iterations,
      ci = ci
    )
    out <- list(predictions = predictions, ci_data = ci_data)
  } else {
    if (inherits(x, "hurdle") && my_args$scale == "zero") {
      # nothing...
      linv <- function(x) x
    } else if (my_args$scale == "zero") {
      linv <- stats::plogis
    } else {
      linv <- exp
    }

    # 2. step: confidence intervals
    ci_data <- get_predicted_ci(
      x,
      predictions = predictions,
      data = my_args$data,
      ci = ci,
      ci_type = my_args$ci_type,
      predict_arg = predict
    )

    # 3. step: back-transform
    out <- .get_predicted_transform(x, predictions, my_args, ci_data, link_inv = linv, verbose = verbose)
  }

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}

#' @export
get_predicted.zeroinfl <- get_predicted.hurdle
