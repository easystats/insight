# process predict-specific arguments ------------------------------------------

.get_predicted_args <- function(x,
                                data = NULL,
                                predict = "expectation",
                                include_random = TRUE,
                                include_smooth = TRUE,
                                ci = 0.95,
                                newdata = NULL,
                                verbose = TRUE,
                                ...) {

  # check whether user possibly used the "type" instead of "predict" argument
  dots <- list(...)

  # one of "type" or "predict" must be provided...
  if (is.null(dots$type) && is.null(predict)) {
    stop(format_message("Please supply a value for the `predict` argument."))
  }

  # ...but not both
  if (!is.null(dots$type) && !is.null(predict)) {
    stop(format_message(
      '`predict` and `type` cannot both be given. The preferred argument for `get_predicted()` is `predict`.',
      'To use the `type` argument, set `predict = NULL` explicitly, e.g.,:',
      '`get_predicted(model, predict = NULL, type = "response")`'
    ))
  }

  # copy "type" to "predict"
  if (!is.null(dots$type)) {
    predict <- dots$type
  }

  if (length(predict) > 1) {
    predict <- predict[1]
    if (isTRUE(verbose)) {
      msg <- format_message(sprintf("More than one option provided in `predict`. Using first option '%s' now."), predict[1])
      warning(msg, call. = FALSE)
    }
  }

  # Get info
  info <- model_info(x, verbose = FALSE)

  # Data
  if (!is.null(newdata) && is.null(data)) data <- newdata
  if (is.null(data)) data <- get_data(x, verbose = verbose)

  # CI
  if (is.null(ci)) ci <- 0

  # check `predict` user-input
  predict_method <- lapply(
    class(x), function(i) {
      tryCatch(utils::getS3method("predict", i),
               error = function(e) NULL)
    }
  )
  predict_method <- tryCatch(predict_method[!sapply(predict_method, is.null)][[1]], error = function(e) NULL)

  # check aliases
  if (predict %in% c("expected", "response")) {
    predict <- "expectation"
  }
  if (predict == "predicted") {
    predict <- "prediction"
  }

  ## TODO remove in a later update
  # backward compatibility
  if (identical(predict, "relation")) {
    message(format_message(
      '`predict = "relation" is deprecated.',
      'Please use `predict = "expectation" instead.'
    ))
    predict <- "expectation"
  }

  # Warn if get_predicted() is not called with an easystats- or
  # model-supported predicted type
  easystats_methods <- c("expectation", "link", "prediction", "classification")
  type_methods <- suppressWarnings(eval(formals(predict_method)$type))
  supported <- c(easystats_methods, type_methods)
  if (isTRUE(verbose) && !is.null(predict) && !predict %in% supported) {
    msg <- format_message(
      sprintf('`predict` = "%s"` is not officially supported by `get_predicted()`.', predict),
      '`predict` will be passed directly to the `predict()` method for the model and not validated.',
      'Please check the validity and scale of the results.',
      'Set `verbose = FALSE` to silence this warning, or use one of the supported values for the `predict` argument:',
      paste(" ", paste(sprintf('"%s"', setdiff(easystats_methods, c("expected", "predicted"))), collapse = ", "))
    )
    warning(msg, call. = FALSE)
  }


  transform <- FALSE

  # type_arg  = what we pass own to type
  # scale_arg = the scale of the predictions (link, response, terms, ...)
  #             this may differ from type_arg, because we back-transform
  # transform = whether we back-transform predictions, SE and CI

  # define prediction type (that's for the initial call to stats::predict)

  # terms
  if (predict == "terms") {
    type_arg <- "terms"
    scale_arg <- "terms"
    transform <- FALSE

    # linear models are always on response scale (there is no other)
  } else if (info$is_linear) {
    type_arg <- "response"
    scale_arg <- "response"
    transform <- FALSE

    # type = "response" always on link-scale - for later back-transformation
  } else if (predict %in% c("expectation", "response", "prediction", "classification")) {

    if (inherits(x, c("hurdle", "zeroinfl", "zerotrunc"))) {
      # pscl: hurdle/zeroinfl and countreg
      type_arg <- "count"
    } else {
      # default behaviour for "response"
      type_arg <- "link"
    }
    scale_arg <- "response"
    transform <- TRUE

    # link-scale
  } else if (predict == "link") {
    type_arg <- "link"
    scale_arg <- "link"
    transform <- FALSE

    # user provided a valid "type" value, which is not one of our "predict" values
  } else if (predict %in% type_methods) {

    if (predict == "count") {
      # pscl
      type_arg <- "count"
      scale_arg <- "link"
      transform <- FALSE

    } else if (predict == "zero") {
      # pscl
      type_arg <- "zero"
      scale_arg <- "link"
      transform <- FALSE

    } else {
      # unknown / default
      type_arg <- scale_arg <- predict
      transform <- FALSE
    }

    # unknown
  } else {
    type_arg <- scale_arg <- predict
    transform <- FALSE
  }


  # 2. step: define CI type
  if (predict %in% c("prediction", "classification")) {
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

  # Add (or set) random variables to "NA"
  if (include_random == FALSE) {
    if (inherits(x, c("stanreg", "brmsfit"))) {
      # rstantools predictions doens't allow for NaNs in newdata
      data[find_variables(x, effects = "random", verbose = FALSE)$random] <- NULL
    } else {
      data[find_variables(x, effects = "random", verbose = FALSE)$random] <- NA
    }
  }

  re.form <- .format_reform(include_random)

  # Return all args
  list(
    data = data,
    include_random = include_random,
    re.form = re.form,
    include_smooth = include_smooth,
    ci_type = ci_type,
    ci = ci,
    type = type_arg,
    predict = predict,
    scale = scale_arg,
    transform = transform,
    info = info,
    allow_new_levels = isTRUE(list(...)$allow.new.levels)
  )
}
