# fixest ----------------------------------------------------------------
# =======================================================================

#' @export
get_predicted.fixest <- function(x, predict = "expectation", data = NULL, ...) {
  # Development is ongoing for standard errors. They are too complicated for us
  # to compute, so we need to wait on the `fixest` developer:
  # https://github.com/lrberge/fixest/issues/22
  dots <- list(...)

  # supported prediction types
  if (!is.null(predict)) {
    predict <- match.arg(predict, choices = c("expectation", "link"))
    type_arg <- ifelse(predict == "expectation", "response", "link")
  } else {
    if (!"type" %in% names(dots)) {
      stop("Please specify the `predict` argument.")
    } else {
      type_arg <- match.arg(dots$type, choices = c("response", "link"))
    }
  }

  # predict.fixest supports: object, newdata, type, na.rm
  args <- list()
  args[["type"]] <- type_arg
  args[["object"]] <- x
  if ("na.rm" %in% names(dots)) {
    args[["na.rm"]] <- dots[["na.rm"]]
  }
  # newdata=NULL raises error
  if (!is.null(data)) {
    args[["newdata"]] <- data
  }

  predictions <- do.call("predict", args)

  # ci_data <- get_predicted_ci(x, predictions, data = args$data, ci = ci, ci_type = "confidence", ...)
  # out <- .get_predicted_transform(x, predictions, args, ci_data)
  # .get_predicted_out(out$predictions, args = args, ci_data = out$ci_data)
  .get_predicted_out(predictions, args = args, ci_data = NULL)
}
