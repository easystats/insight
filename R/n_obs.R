#' @title Get number of observations from a model
#' @name n_obs
#'
#' @description This method returns the number of observation that were used
#'   to fit the model, as numeric value.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return The number of observations used to fit the model, or \code{NULL} if
#'   this information is not available.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' n_obs(m)
#' @export
n_obs <- function(x, ...) {
  UseMethod("n_obs")
}


#' @export
n_obs.default <- function(x, ...) {
  tryCatch({
    stats::nobs(x)
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
n_obs.gmnl <- function(x, ...) {
  x$logLik$nobs
}


#' @export
n_obs.coxph <- function(x, ...) {
  x$n
}


#' @export
n_obs.stanmvreg <- function(x, ...) {
  min(x$n_yobs)
}


#' @export
n_obs.mlogit <- function(x, ...) {
  nrow(x$model)
}


#' @export
n_obs.hurdle <- function(x, ...) {
  x$n
}


#' @export
n_obs.zerotrunc <- function(x, ...) {
  x$n
}


#' @export
n_obs.zeroinfl <- function(x, ...) {
  x$n
}
