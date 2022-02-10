# GAM -------------------------------------------------------------------
# =======================================================================
#' @export
get_predicted.gam <- function(x,
                              data = NULL,
                              predict = "expectation",
                              ci = 0.95,
                              include_random = TRUE,
                              include_smooth = TRUE,
                              iterations = NULL,
                              verbose = TRUE,
                              ...) {

  # allow users to set `predict=NULL` and specify `type` directly
  if (!is.null(predict)) {
    predict <- match.arg(predict, choices = c("expectation", "expected", "link", "prediction", "predicted", "classification"))

    # Sanity checks
    if (predict %in% c("prediction", "predicted")) {
      if (verbose) {
        warning(
          format_message(
            "`predict='prediction'` is currently not available for GAM models.",
            "Changing to `predict='expectation'`."
          ),
          call. = FALSE
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
  args <- .get_predicted_args(
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
  if (is.null(iterations)) {
    predictions <- rez$fit
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

  # Get CI
  ci_data <- .get_predicted_se_to_ci(x, predictions = predictions, se = rez$se.fit, ci = ci)
  out <- .get_predicted_transform(x, predictions, args, ci_data, verbose = verbose)
  .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
}

#' @export
get_predicted.gamm <- get_predicted.gam

#' @export
get_predicted.list <- get_predicted.gam # gamm4
