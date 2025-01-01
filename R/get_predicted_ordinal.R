# ordinal ---------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.clm <- function(x,
                              data = NULL,
                              predict = "expectation",
                              ci = NULL,
                              verbose = TRUE,
                              ...) {
  # When (a) `newdata` is not null and (b) the response variable does *not*
  # appear in `newdata`, predict.clm() returns matrices with predictions for
  # each levels of the response.  When either of those conditions fail,
  # `predict.clm()` returns vectors with only predictions for the actually
  # observed response level in each row.

  dots <- list(...)

  # prediction types
  if (!is.null(predict)) {
    valid <- c("expectation", "classification")
    predict <- match.arg(predict, choices = valid)
    type_arg <- c("prob", "class")[match(predict, valid)]
  } else if ("type" %in% names(dots)) {
    type_arg <- match.arg(dots$type, choices = c("prob", "class"))
  } else {
    format_error("Please specify the `predict` argument.")
  }

  # hack to get predictions for all response levels
  if (is.null(data)) {
    data <- get_data(x, verbose = FALSE)
  }
  resp <- find_response(x)
  data <- data[, setdiff(colnames(data), resp), drop = FALSE]
  vars <- as.character(attr(x$terms, "variables"))[-1]
  vars[attr(x$terms, "response")] <- resp
  s <- paste0("list(", toString(vars), ")")
  new_call <- parse(text = s, keep.source = FALSE)[[1L]]
  attr(x$terms, "variables") <- new_call

  # sanitize CI
  if (!is.null(ci) && is.na(ci)) {
    ci <- NULL
  }

  # check whether CIs should be returned
  if (!is.null(ci) && type_arg == "class") {
    if (verbose) {
      format_warning("Confidence intervals are not available for classification.")
    }
    ci <- NULL
  }

  # compute predictions
  my_args <- list(
    object = x,
    newdata = data,
    type = type_arg,
    se.fit = (type_arg == "prob"),
    interval = !is.null(ci),
    level = ifelse(is.null(ci), 0.95, ci)
  )
  pred <- do.call("predict", my_args)

  my_args$data <- my_args$newdata
  out <- .get_predicted_out(pred$fit, my_args = my_args)

  # standard error and confidence intervals matrix to long format
  if (type_arg == "prob") {
    se <- pred$se.fit
    se <- as.data.frame(se)
    se$Row <- seq_len(nrow(se))
    se <- stats::reshape(se,
      direction = "long",
      varying = setdiff(colnames(se), "Row"),
      times = setdiff(colnames(se), "Row"),
      v.names = "SE",
      timevar = "Response",
      idvar = "Row"
    )
    row.names(se) <- NULL
    ci_data <- NULL

    # add confidence intervals, reshape to long
    if (!is.null(ci) && "lwr" %in% names(pred)) {
      ci_data <- lapply(c("CI_low", "CI_high"), function(i) {
        if (i == "CI_low") {
          conf <- as.data.frame(pred$lwr)
        } else {
          conf <- as.data.frame(pred$upr)
        }
        vary <- setdiff(colnames(conf), "Row")
        conf$Row <- seq_len(nrow(conf))
        conf <- stats::reshape(conf,
          direction = "long",
          varying = vary,
          times = vary,
          v.names = i,
          timevar = "Response",
          idvar = "Row"
        )
        row.names(conf) <- NULL
        conf
      })
      ci_data <- merge(ci_data[[1]], ci_data[[2]])
    }

    # bind SE and CI
    if (!is.null(ci_data)) {
      ci_data <- merge(se, ci_data)
    } else {
      ci_data <- se
    }

    # match order with predictions
    ci_data <- ci_data[order(ci_data$Response, ci_data$Row), ]
    attr(out, "ci_data") <- ci_data
  }

  return(out)
}


# nnet::multinom --------------------------------------------------------
# =======================================================================

#' @export
get_predicted.multinom <- function(x, predict = "expectation", data = NULL, ci = NULL, verbose = TRUE, ...) {
  dots <- list(...)

  # `type` argument can be: probs | class
  if (!is.null(predict)) {
    type_arg <- match.arg(predict, choices = c("classification", "expectation"))
    type_arg <- c("class", "probs")[c("classification", "expectation") == type_arg]
  } else if ("type" %in% names(dots)) {
    type_arg <- match.arg(dots$type, choices = c("class", "probs"))
  } else {
    format_error("The `predict` argument must be either \"expectation\" or \"classification\".")
  }

  my_args <- c(list(x, data = data, ci = ci, predict = type_arg), list(...))

  # predict.multinom doesn't work when `newdata` is explicitly set to NULL (weird)
  if (is.null(data)) {
    out <- stats::predict(x, type = type_arg)
  } else {
    out <- stats::predict(x, newdata = data, type = type_arg)
  }

  # reshape
  out <- .get_predicted_out(out, my_args = my_args)

  # add CI
  if (!is.null(ci)) {
    attr(out, "ci_data") <- get_predicted_ci(
      x,
      predictions = out,
      data = data,
      ci = ci,
      type = type_arg,
      verbose = verbose
    )
  }

  out
}


# MASS ------------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.rlm <- function(x, predict = "expectation", ...) {
  # only one prediction type supported
  if (!is.null(predict)) {
    predict <- match.arg(predict, choices = "expectation")
    get_predicted.lm(x, predict = predict, ...)
  } else {
    dots <- list(...)
    if (!"type" %in% names(dots)) {
      format_error("Please specify the `predict` argument.")
    }
    dots[["type"]] <- match.arg(dots$type, choices = "response")
    dots[["x"]] <- x
    dots <- c(dots, list(predict = NULL))
    do.call("get_predicted.lm", dots)
  }
}

# MASS::polr accepts only "class" or "probs" types, and "expectation"
# corresponds to "probs". These are the same as nnet::multinom.
# Make sure this is below get_predicted.multinom in the file.

#' @export
get_predicted.polr <- get_predicted.multinom

#' @export
get_predicted.bracl <- get_predicted.multinom
