#' Predicted values
#'
#' Returns values predicted by a model (i.e., fitted values).
#'
#' @param ... Not used.
#' @inheritParams get_residuals
#' @inheritParams stats::predict.lm
#'
#' @return The fitted values (i.e. predictions for the response).
#'
#' @note Currently, this function just calls \code{stats::fitted()}, but will
#' be extended to other objects that don't work with \code{stats::fitted()} in
#' future updates.
#'
#' @examples
#' data(mtcars)
#' x <- lm(mpg ~ cyl + hp, data = mtcars)
#' get_predicted(x)
#'
#' @export
get_predicted <- function(x, newdata = NULL, ...) {
  UseMethod("get_predicted")
}

#' @importFrom stats fitted
#' @export
get_predicted.default <- function(x, newdata = NULL, ...) {
  if(is.null(newdata)){
    stats::fitted(x)
  } else{
    stats::predict(x, newdata = newdata, ...)
  }
}

#' @export
get_predicted.data.frame <- function(x, newdata = NULL, ...) {
  # This makes it pipe friendly; data %>% get_predicted(model)
  if (is.null(newdata)) {
    stop("Please provide a model to base the estimations on.")
  } else {
    get_predicted(x = newdata, newdata = x, ...)
  }
}

# See options of:
# predict.lm
# predict.glm
# lme4::predict.merMod
# rstanarm::posterior_epred(), rstanarm::posterior_linpred(), rstanarm::posterior_predict(), rstanarm::posterior_interval