# Mixed Models (lme4, glmmTMB, MixMod, ...) -----------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.lmerMod <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = NULL,
                                  ci_method = NULL,
                                  include_random = "default",
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {
  # Sanitize input
  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    ci_method = ci_method,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # Make prediction only using random if only random
  if (all(names(my_args$data) %in% find_random(x, flatten = TRUE))) {
    random.only <- TRUE
  } else {
    random.only <- FALSE
  }

  # Prediction function
  predict_function <- function(x, ...) {
    stats::predict(
      x,
      newdata = my_args$data,
      type = my_args$type,
      re.form = my_args$re.form,
      random.only = random.only,
      allow.new.levels = my_args$allow_new_levels,
      ...
    )
  }

  # 1. step: predictions
  if (is.null(iterations)) {
    predictions <- predict_function(x)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # 2. step: confidence intervals
  ci_data <- get_predicted_ci(x, predictions,
    data = my_args$data, ci = ci,
    ci_method = ci_method, ci_type = my_args$ci_type,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(x, predictions, my_args, ci_data, verbose = verbose, ...)

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}

#' @export
get_predicted.merMod <- get_predicted.lmerMod


# glmmTMB ---------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.glmmTMB <- function(x,
                                  data = NULL,
                                  predict = "expectation",
                                  ci = NULL,
                                  include_random = "default",
                                  iterations = NULL,
                                  verbose = TRUE,
                                  ...) {
  # validation checks
  if (!is.null(predict) && predict %in% c("prediction", "predicted", "classification")) {
    predict <- "expectation"
    if (verbose) {
      format_warning(
        "\"prediction\" and \"classification\" are currently not supported by the `predict` argument for `glmmTMB` models.",
        "Changing to `predict=\"expectation\"`."
      )
    }
  }

  # TODO: prediction intervals
  # https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

  # Sanitize input
  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type = my_args$type,
      re.form = my_args$re.form,
      allow.new.levels = my_args$allow_new_levels,
      ...
    )
  }

  # 1. step: predictions
  rez <- predict_function(x, data = my_args$data, se.fit = TRUE)

  if (is.null(iterations)) {
    predictions <- as.numeric(rez$fit)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # "expectation" for zero-inflated? we need a special handling
  # for predictions and CIs here.

  if (my_args$scale == "response" && my_args$info$is_zero_inflated) {
    # intermediate step: prediction from ZI model, for non-truncated families!
    # for truncated family, behaviour in glmmTMB changed in 1.1.5  to correct
    # conditional and response predictions
    if (!my_args$info$is_hurdle) {
      zi_predictions <- stats::predict(
        x,
        newdata = data,
        type = "zprob",
        re.form = my_args$re.form,
        ...
      )
      predictions <- link_inverse(x)(predictions) * (1 - as.vector(zi_predictions))
    }

    # 2. and 3. step: confidence intervals and back-transform
    ci_data <- .simulate_zi_predictions(
      model = x,
      newdata = data,
      predictions = predictions,
      nsim = iterations,
      ci = ci
    )

    out <- list(predictions = predictions, ci_data = ci_data)
  } else {
    # 2. step: confidence intervals
    ci_data <- .get_predicted_se_to_ci(
      x,
      predictions = predictions,
      se = rez$se.fit,
      ci = ci,
      verbose = verbose,
      ...
    )

    # 3. step: back-transform
    out <- .get_predicted_transform(x, predictions, my_args, ci_data, verbose = verbose, ...)
  }

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}


# GLMMadaptive: mixed_model (class MixMod) ------------------------------
# =======================================================================

#' @export
get_predicted.MixMod <- function(x,
                                 data = NULL,
                                 predict = "expectation",
                                 ci = NULL,
                                 include_random = "default",
                                 iterations = NULL,
                                 verbose = TRUE,
                                 ...) {
  # validation checks
  if (!is.null(predict) && predict %in% c("prediction", "predicted", "classification")) {
    predict <- "expectation"
    if (verbose) {
      format_warning(
        "\"prediction\" and \"classification\" are currently not supported by the `predict` argument for `GLMMadaptive` models.",
        "Changing to `predict=\"expectation\"`."
      )
    }
  }

  # Sanitize input
  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    verbose = verbose,
    ...
  )

  # Prediction function
  predict_function <- function(x, data, ...) {
    stats::predict(
      x,
      newdata = data,
      type_pred = my_args$type,
      type = ifelse(isTRUE(my_args$include_random), "subject_specific", "mean_subject"),
      ...
    )
  }

  # 1. step: predictions
  rez <- predict_function(x, data = my_args$data)

  if (is.null(iterations)) {
    predictions <- as.numeric(rez)
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
      predict_function = predict_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # "expectation" for zero-inflated? we need a special handling
  # for predictions and CIs here.

  if (my_args$scale == "response" && my_args$info$is_zero_inflated) {
    # 2. and 3. step: confidence intervals and back-transform
    ci_data <- .simulate_zi_predictions(model = x, newdata = data, predictions = predictions, nsim = iterations, ci = ci)
    out <- list(predictions = predictions, ci_data = ci_data)
  } else {
    # 2. step: confidence intervals
    ci_data <- get_predicted_ci(
      x,
      predictions,
      data = my_args$data[colnames(my_args$data) != find_response(x)],
      ci = ci,
      ci_type = my_args$ci_type,
      ...
    )

    # 3. step: back-transform
    out <- .get_predicted_transform(x, predictions, my_args, ci_data, verbose = verbose, ...)
  }

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}


# HGLM: mixed_model (class hglm) ------------------------------
# =============================================================

#' @export
get_predicted.hglm <- function(x, verbose = TRUE, ...) {
  # hglm only provide fitted values
  x$fv
}
