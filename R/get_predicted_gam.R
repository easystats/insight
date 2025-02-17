# GAM -------------------------------------------------------------------
# =======================================================================
#' @rdname get_predicted
#' @export
get_predicted.gam <- function(x,
                              data = NULL,
                              predict = "expectation",
                              ci = NULL,
                              include_random = TRUE,
                              include_smooth = TRUE,
                              iterations = NULL,
                              verbose = TRUE,
                              ...) {
  # allow users to set `predict=NULL` and specify `type` directly
  if (!is.null(predict)) {
    predict <- match.arg(
      predict,
      choices = c("expectation", "expected", "link", "prediction", "predicted", "classification")
    )

    # validation checks
    if (predict %in% c("prediction", "predicted")) {
      if (verbose) {
        format_warning(
          "`predict=\"prediction\"` is currently not available for GAM models.",
          "Changing to `predict=\"expectation\"`."
        )
      }
      predict <- "expectation"
    }
    # TODO: check this for prediction intervals:
    # https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
    # https://github.com/gavinsimpson/gratia/blob/master/R/confint-methods.R
    # https://github.com/gavinsimpson/gratia/blob/master/R/posterior-samples.R
  }

  # Sanitize input
  my_args <- .get_predicted_args(
    x,
    data = data,
    predict = predict,
    ci = ci,
    include_random = include_random,
    include_smooth = include_smooth,
    verbose = verbose,
    ...
  )

  if (inherits(x, c("gamm", "list"))) x <- x$gam


  # Prediction function
  predict_function <- function(x, data, se.fit = TRUE, ...) {
    dot_args <- list(...)
    dot_args[["type"]] <- NULL
    predict_args <- list(x,
      newdata = data, type = my_args$type, re.form = my_args$re.form,
      unconditional = FALSE, se.fit = se.fit
    )
    predict_args <- c(predict_args, dot_args)
    do.call(stats::predict, compact_list(predict_args))
  }

  boot_function <- function(x, data, ...) {
    predict_function(x, data, se.fit = FALSE, ...)
  }

  # Get prediction
  if (is.null(ci)) {
    rez <- predict_function(x, data = my_args$data, se.fit = FALSE, ...)
    rez <- list(fit = rez)
  } else {
    rez <- predict_function(x, data = my_args$data, se.fit = TRUE, ...)
  }

  if (is.null(iterations)) {
    predictions <- rez$fit
  } else {
    predictions <- .get_predicted_boot(
      x,
      data = my_args$data,
      predict_function = boot_function,
      iterations = iterations,
      verbose = verbose,
      ...
    )
  }

  # Get CI
  if (!is.null(ci)) {
    ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci, verbose = verbose)
  } else {
    ci_data <- NULL
  }
  out <- .get_predicted_transform(x, predictions, my_args, ci_data, verbose = verbose, ...)
  .get_predicted_out(out$predictions, my_args = my_args, ci_data = out$ci_data)
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.Gam <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4


# GAMLSS -----------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.gamlss <- function(x,
                                 data = NULL,
                                 predict = "expectation",
                                 ci = NULL,
                                 include_smooth = TRUE,
                                 iterations = NULL,
                                 verbose = TRUE,
                                 ...) {
  get_predicted.default(x,
    data = NULL,
    predict = "expectation",
    ci = NULL,
    include_smooth = include_smooth,
    iterations = iterations,
    verbose = FALSE,
    ...
  )
}
