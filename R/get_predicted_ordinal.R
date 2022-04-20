# ordinal ---------------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.clm <- function(x, predict = "expectation", data = NULL, ...) {
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
  } else {
    if (!"type" %in% names(dots)) {
      stop("Please specify the `predict` argument.")
    } else {
      type_arg <- match.arg(dots$type, choices = c("prob", "class"))
    }
  }

  # hack to get predictions for all response levels
  if (is.null(data)) {
    data <- get_data(x)
  }
  resp <- find_response(x)
  data <- data[, setdiff(colnames(data), resp), drop = FALSE]
  vars <- as.character(attr(x$terms, "variables"))[-1]
  vars[attr(x$terms, "response")] <- resp
  s <- paste0("list(", paste(vars, collapse = ", "), ")")
  new_call <- parse(text = s, keep.source = FALSE)[[1L]]
  attr(x$terms, "variables") <- new_call

  # compute predictions
  args <- list(
    object = x,
    newdata = data,
    type = type_arg,
    se.fit = (type_arg == "prob")
  )
  pred <- do.call("predict", args)

  args$data <- args$newdata
  out <- .get_predicted_out(pred$fit, args = args)

  # standard error matrix to long format
  if (type_arg == "prob") {
    se <- pred$se.fit
    se <- as.data.frame(se)
    se$Row <- 1:nrow(se)
    se <- stats::reshape(se,
      direction = "long",
      varying = setdiff(colnames(se), "Row"),
      times = setdiff(colnames(se), "Row"),
      v.names = "SE",
      timevar = "Response",
      idvar = "Row"
    )
    row.names(se) <- NULL
    attr(out, "ci_data") <- se
  }

  return(out)
}




# nnet::multinom --------------------------------------------------------
# =======================================================================

#' @rdname get_predicted
#' @export
get_predicted.multinom <- function(x, predict = "expectation", data = NULL, ...) {
  dots <- list(...)

  # `type` argument can be: probs | class
  if (!is.null(predict)) {
    type_arg <- match.arg(predict, choices = c("classification", "expectation"))
    type_arg <- c("class", "probs")[c("classification", "expectation") == type_arg]
  } else if ("type" %in% names(dots)) {
    type_arg <- match.arg(dots$type, choices = c("class", "probs"))
  } else {
    stop('The `predict` argument must be either "expectation" or "classification".')
  }

  args <- c(list(x, "data" = data), list(...))

  # predict.multinom doesn't work when `newdata` is explicitly set to NULL (weird)
  if (is.null(data)) {
    out <- stats::predict(x, type = type_arg)
  } else {
    out <- stats::predict(x, newdata = data, type = type_arg)
  }

  .get_predicted_out(out, args = args)
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
      stop("Please specify the `predict` argument.")
    }
    dots[["type"]] <- match.arg(dots$type, choices = "response")
    dots[["x"]] <- x
    dots <- c(dots, list("predict" = NULL))
    do.call("get_predicted.lm", dots)
  }
}

# MASS::polr accepts only "class" or "probs" types, and "expectation"
# corresponds to "probs". These are the same as nnet::multinom.
# Make sure this is below get_predicted.multinom in the file.

#' @rdname get_predicted
#' @export
get_predicted.polr <- get_predicted.multinom
