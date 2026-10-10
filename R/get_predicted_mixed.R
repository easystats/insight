# Mixed Models (lme4, glmmTMB, MixMod, ...) -----------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.lmerMod <- function(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  ci_method = NULL,
  include_random = "default",
  iterations = NULL,
  verbose = TRUE,
  ...
) {
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
  ci_data <- get_predicted_ci(
    x,
    predictions,
    data = my_args$data,
    ci = ci,
    ci_method = ci_method,
    ci_type = my_args$ci_type,
    ...
  )

  # 3. step: back-transform
  out <- .get_predicted_transform(
    x,
    predictions,
    my_args,
    ci_data,
    verbose = verbose,
    ...
  )

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}

#' @export
get_predicted.merMod <- get_predicted.lmerMod


# nlme ------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.lme <- function(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  ci_type = "confidence",
  ci_method = NULL,
  dispersion_method = "sd",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
) {
  dots <- list(...)
  if (is.null(data) && !is.null(dots$newdata)) {
    data <- dots$newdata
  }
  # without new data, we return the predictions for the model data. Nonlinear
  # models (gnls, nlme) have no terms, so they also use the default method.
  if (is.null(data) || inherits(x, c("gnls", "nlme"))) {
    return(get_predicted.default(
      x,
      data = data,
      predict = predict,
      ci = ci,
      ci_type = ci_type,
      ci_method = ci_method,
      dispersion_method = dispersion_method,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    ))
  }
  data <- as.data.frame(data)

  # predict() for lme and gls stops for rows with missing values or with factor
  # levels that the model data does not have, so we predict the other rows only
  bad_rows <- .lme_unusable_rows(x, data, verbose = verbose)
  if (all(bad_rows)) {
    return(NULL)
  }

  my_args <- .get_predicted_args(
    x,
    data = data[!bad_rows, , drop = FALSE],
    predict = predict,
    verbose = verbose,
    ...
  )

  # 1. step: predictions. If grouping columns are missing or have new levels,
  # .get_predicted_args() sets them to NA, and we return population-level
  # predictions, as for lme4 models
  predict_args <- list(x, newdata = my_args$data)
  if (inherits(x, "lme") && isFALSE(my_args$include_random)) {
    predict_args$level <- 0
  }
  predictions <- .safe(do.call(stats::predict, predict_args))
  if (is.null(predictions)) {
    if (isTRUE(verbose)) {
      format_warning(
        paste0("Could not compute predictions for model of class `", class(x)[1], "`.")
      )
    }
    return(NULL)
  }

  # 2. step: confidence intervals
  ci_data <- .safe(get_predicted_ci(
    x,
    predictions,
    data = my_args$data,
    ci_type = my_args$ci_type,
    ci_method = ci_method,
    vcov = vcov,
    vcov_args = vcov_args,
    ...
  ))

  # 3. step: back-transform, 4. step: final preparation
  out <- .get_predicted_transform(x, predictions, my_args = my_args, ci_data, verbose = verbose, ...)
  out <- .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
  if (!any(bad_rows)) {
    return(out)
  }

  # return one row per row of `data`, with NA for the rows we did not predict
  expand_rows <- function(values) {
    full <- rep(NA, nrow(data))
    full[!bad_rows] <- values
    full
  }
  predictions <- expand_rows(as.vector(out))
  attributes(predictions) <- attributes(out)[setdiff(names(attributes(out)), "names")]
  ci_data <- attr(out, "ci_data")
  if (!is.null(ci_data)) {
    attr(predictions, "ci_data") <- as.data.frame(lapply(ci_data, expand_rows))
  }
  attr(predictions, "data") <- data
  predictions
}

#' @export
get_predicted.gls <- get_predicted.lme


# rows of `data` with missing values or factor levels that the model data does
# not have. We only check the columns of the fixed effects, because
# .get_predicted_args() sets missing grouping columns to NA.
.lme_unusable_rows <- function(x, data, verbose = TRUE) {
  model_data <- get_data(x, verbose = FALSE)
  fixed_vars <- intersect(
    all.vars(stats::delete.response(stats::terms(x))),
    colnames(data)
  )
  bad_rows <- rep(FALSE, nrow(data))
  bad_columns <- NULL
  for (i in fixed_vars) {
    bad <- is.na(data[[i]])
    if (is.factor(model_data[[i]]) || is.character(model_data[[i]])) {
      bad <- bad | !as.character(data[[i]]) %in% levels(factor(model_data[[i]]))
    }
    if (any(bad)) {
      bad_rows <- bad_rows | bad
      bad_columns <- c(bad_columns, i)
    }
  }
  if (any(bad_rows) && isTRUE(verbose)) {
    format_warning(paste0(
      "Could not compute predictions for ",
      sum(bad_rows),
      " row(s) of `data`, because of missing values or factor levels",
      " that are not in the model data, in ",
      toString(paste0("`", bad_columns, "`")),
      ".",
      if (!all(bad_rows)) " The predictions for these rows are `NA`."
    ))
  }
  bad_rows
}


# glmmTMB ---------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.glmmTMB <- function(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  include_random = "default",
  iterations = NULL,
  verbose = TRUE,
  ...
) {
  # ordinal family: "expectation" returns per-category probabilities (like
  # `clm`), not `plogis()` of the linear predictor. As in `.get_predicted_args()`,
  # a `type` argument takes precedence over `predict`
  dots <- list(...)
  if (.is_glmmtmb_ordinal(x)) {
    if (is.null(dots$type)) {
      requested <- predict[1]
    } else {
      requested <- dots$type[1]
    }
    ordinal_types <- c(
      "expectation",
      "expected",
      "response",
      "prediction",
      "predicted",
      "classification",
      "probs"
    )
    if (isTRUE(requested %in% ordinal_types)) {
      if (!is.null(iterations) && verbose) {
        format_warning(
          "Bootstrapped predictions are currently not supported for `glmmTMB` models with `ordinal()` family.",
          "Ignoring the `iterations` argument."
        )
      }
      return(.get_predicted_glmmtmb_ordinal(
        x,
        data = data,
        predict = predict,
        ci = ci,
        include_random = include_random,
        verbose = verbose,
        dots = dots
      ))
    }
  }

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
    out <- .get_predicted_transform(
      x,
      predictions,
      my_args,
      ci_data,
      verbose = verbose,
      ...
    )
  }

  # 4. step: final preparation
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}


# GLMMadaptive: mixed_model (class MixMod) ------------------------------
# =======================================================================

#' @export
get_predicted.MixMod <- function(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  include_random = "default",
  iterations = NULL,
  verbose = TRUE,
  ...
) {
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
    ci_data <- get_predicted_ci(
      x,
      predictions,
      data = my_args$data[colnames(my_args$data) != find_response(x)],
      ci = ci,
      ci_type = my_args$ci_type,
      ...
    )

    # 3. step: back-transform
    out <- .get_predicted_transform(
      x,
      predictions,
      my_args,
      ci_data,
      verbose = verbose,
      ...
    )
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
