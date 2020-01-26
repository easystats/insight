#' @title Get the values from the response variable
#' @name get_response
#'
#' @description Returns the values the response variable(s) from a model object.
#'    If the model is a multivariate response model, a data frame with values
#'    from all response variables is returned.
#'
#' @param select Optional name(s) of response variables for which to extract values.
#'   Can be used in case of regression models with multiple response variables.
#' @inheritParams find_predictors
#'
#' @return The values of the response variable, as vector, or a data frame if
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
#' get_response(m, select = "incidence")
#'
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_response(m)
#' @export
get_response <- function(x, select = NULL) {
  rn <- find_response(x, combine = FALSE)

  # exceptions
  if (inherits(x, "DirichletRegModel")) {
    rv <- x$Y
    class(rv) <- "matrix"
    data.frame(rv)
  } else if (length(rn) > 1) {
    rv <- get_data(x)[, rn, drop = FALSE]
    colnames(rv) <- rn
    # if user only wants specific response value, return this only
    if (!is.null(select) && all(select %in% colnames(rv))) {
      rv <- rv[, select, drop = TRUE]
    }
    rv
  } else {
    rv <- get_data(x)[[find_response(x, combine = TRUE)]]
    if (!is.factor(rv) &&
      !is.numeric(rv) &&
      !is.character(rv) && !is.logical(rv) && !is.integer(rv)) {
      as.vector(rv)
    } else {
      rv
    }
  }
}
