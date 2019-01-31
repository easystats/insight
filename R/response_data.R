#' @title Get the values from a model's response variable
#' @name response_data
#'
#' @description Returns the values the response variable(s) from a model object.
#'    If the model is a multivariate response model, a data frame with values
#'    from all response variables is returned.
#'
#' @param resp Optional names of response variables for which to extract values.
#' @inheritParams model_predictors
#'
#' @return The values of the reponse variable, as vector, or a data frame if
#'   \code{x} has more than one defined response variable.
#'
#' @examples
#' library(lme4)
#' data(cbpp)
#' data(mtcars)
#' cbpp$trials <- cbpp$size - cbpp$incidence
#'
#' m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
#' response_data(m)
#' response_data(m, resp = "incidence")
#'
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' response_data(m)
#'
#' @export
response_data <- function(x, ...) {
  UseMethod("response_data")
}


#' @rdname response_data
#' @export
response_data.default <- function(x, resp = NULL, ...) {
  rn <- model_response(x, combine = FALSE)

  if (length(rn) > 1) {
    rv <- model_data(x)[, clean_names(rn), drop = FALSE]
    colnames(rv) <- rn
    # if user only wants specific response value, return this only
    if (!is.null(resp) && all(resp %in% colnames(rv)))
      rv <- rv[, resp, drop = TRUE]
    rv
  } else {
    as.vector(model_data(x)[[clean_names(model_response(x, combine = TRUE))]])
  }
}


#' @export
response_data.lme <- function(x, ...) {
  if (!requireNamespace("nlme", quietly = TRUE))
    stop("To use this function, please install package 'nlme'.")

  as.vector(nlme::getResponse(x))
}


#' @export
response_data.gls <- function(x, ...) {
  if (!requireNamespace("nlme", quietly = TRUE))
    stop("To use this function, please install package 'nlme'.")

  as.vector(nlme::getResponse(x))
}


#' @export
response_data.stanmvreg <- function(x, resp = NULL, ...) {
  rv <- model_data(x)[, clean_names(model_response(x, combine = TRUE)), drop = FALSE]

  # if user only wants specific response value, return this only
  if (!is.null(resp) && all(resp %in% colnames(rv)))
    rv <- rv[, resp, drop = TRUE]
  else if (ncol(rv) == 1)
    rv <- rv[[1]]

  rv
}


#' @importFrom stats formula
#' @export
response_data.brmsfit <- function(x, resp = NULL, ...) {

  ## TODO maybe replace with brms::get_y()?

  rv <- model_data(x)[, clean_names(model_response(x, combine = TRUE)), drop = FALSE]

  # if user only wants specific response value, return this only
  if (!is.null(resp) && all(resp %in% colnames(rv)))
    rv <- rv[, resp, drop = TRUE]
  else if (ncol(rv) == 1)
    rv <- rv[[1]]

  rv
}
