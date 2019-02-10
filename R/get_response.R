#' @title Get the values from a model's response variable
#' @name get_response
#'
#' @description Returns the values the response variable(s) from a model object.
#'    If the model is a multivariate response model, a data frame with values
#'    from all response variables is returned.
#'
#' @param resp Optional names of response variables for which to extract values.
#' @inheritParams find_predictors
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
#' head(get_response(m))
#' get_response(m, resp = "incidence")
#'
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_response(m)
#' @export
get_response <- function(x, resp = NULL) {
  rn <- find_response(x, combine = FALSE)

  if (length(rn) > 1) {
    rv <- get_data(x)[, rn, drop = FALSE]
    colnames(rv) <- rn
    # if user only wants specific response value, return this only
    if (!is.null(resp) && all(resp %in% colnames(rv))) {
      rv <- rv[, resp, drop = TRUE]
    }
    rv
  } else {
    rv <- get_data(x)[[find_response(x, combine = TRUE)]]
    if (!is.factor(rv) &&
        !is.numeric(rv) &&
        !is.character(rv) && !is.logical(rv) && !is.integer(rv))
      as.vector(rv)
    else
      rv
  }
}
