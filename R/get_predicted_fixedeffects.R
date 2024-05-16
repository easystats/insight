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
  } else if ("type" %in% names(dots)) {
    type_arg <- match.arg(dots$type, choices = c("response", "link"))
  } else {
    format_error("Please specify the `predict` argument.")
  }

  # predict.fixest supports: object, newdata, type, na.rm
  my_args <- list()
  my_args[["type"]] <- type_arg
  my_args[["object"]] <- x
  if ("na.rm" %in% names(dots)) {
    my_args[["na.rm"]] <- dots[["na.rm"]]
  }
  # newdata=NULL raises error
  if (!is.null(data)) {
    my_args[["newdata"]] <- data
  }

  predictions <- do.call("predict", my_args)

  .get_predicted_out(predictions, my_args = my_args, ci_data = NULL)
}
