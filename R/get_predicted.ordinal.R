# ordinal ---------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.clm <- function(x, predict = "expectation", data = NULL, ...) {
  # When (a) `newdata` is not null and (b) the response variable does *not*
  # appear in `newdata`, predict.clm() returns matrices with predictions for
  # each levels of the response.  When either of those conditions fail,
  # `predict.clm()` returns vectors with only predictions for the actually
  # observed reponse level in each row.

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

  out <- .get_predicted_out(pred$fit)

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
