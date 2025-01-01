#' @title Get the values from model weights
#' @name get_weights
#'
#' @description Returns weighting variable of a model.
#'
#' @param x A fitted model.
#' @param remove_na Logical, if `TRUE`, removes possible missing values.
#' @param null_as_ones Logical, if `TRUE`, will return a vector of `1`
#'   if no weights were specified in the model (as if the weights were all set
#'   to 1).
#' @param ... Currently not used.
#'
#' @return The weighting variable, or `NULL` if no weights were specified.
#' If the weighting variable should also be returned (instead of `NULL`)
#' when all weights are set to 1 (i.e. no weighting),
#' set `null_as_ones = TRUE`.
#'
#' @examples
#' data(mtcars)
#' set.seed(123)
#' mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
#'
#' # LMs
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
#' get_weights(m)
#'
#' get_weights(lm(mpg ~ wt, data = mtcars), null_as_ones = TRUE)
#'
#' # GLMs
#' m <- glm(vs ~ disp + mpg, data = mtcars, weights = weight, family = quasibinomial)
#' get_weights(m)
#' m <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = weight, family = binomial)
#' get_weights(m)
#' @export
get_weights <- function(x, ...) {
  UseMethod("get_weights")
}


## TODO: probably need own method for nlme, see #727

#' @rdname get_weights
#' @export
get_weights.default <- function(x, remove_na = FALSE, null_as_ones = FALSE, ...) {
  weight_vars <- find_weights(x)
  w <- tryCatch(
    stats::weights(x, ...),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(w)) {
    w <- tryCatch(
      stats::model.frame(x)[["(weights)"]],
      error = function(e) NULL,
      warning = function(w) NULL
    )
  }

  if (is.null(w)) {
    w <- tryCatch(
      {
        if (length(weight_vars) > 1L) {
          .recover_data_from_environment(x)[weight_vars]
        } else {
          .recover_data_from_environment(x)[[weight_vars]]
        }
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )
  }

  # validation check - if weights is empty, set to NULL
  if (!length(w)) {
    w <- NULL
  }

  # if all weights are 1, set return value to NULL,
  # unless the weights were explicitly set in the model call
  if (!is.null(w) && isTRUE(all(w[!is.na(w)] == 1L)) && is.null(weight_vars)) {
    w <- NULL
  }

  if (!is.null(w) && anyNA(w) && isTRUE(remove_na)) {
    w <- w[!is.na(w)]
  }

  if (is.null(w) && isTRUE(null_as_ones)) {
    w <- rep.int(1, n_obs(x))
  }

  w
}


#' @export
get_weights.brmsfit <- function(x, remove_na = FALSE, null_as_ones = FALSE, ...) {
  w <- unique(find_weights(x))

  if (!is.null(w)) {
    if (length(w) > 1L) {
      return(get_data(x, verbose = FALSE)[w])
    }
    w <- get_data(x, verbose = FALSE)[[w]]
  }

  if (!is.null(w) && all(w == 1L)) {
    w <- NULL
  }

  if (!is.null(w) && anyNA(w) && isTRUE(remove_na)) {
    w <- stats::na.omit(w)
  }

  if (is.null(w) && null_as_ones) {
    w <- rep.int(1, n_obs(x))
  }

  w
}


#' @export
get_weights.btergm <- function(x, null_as_ones = FALSE, ...) {
  x@weights
}


#' @export
get_weights.list <- function(x, remove_na = FALSE, null_as_ones = FALSE, ...) {
  # For GAMMs
  if ("gam" %in% names(x)) {
    get_weights(x$gam, remove_na = remove_na, null_as_ones = null_as_ones, ...)
  } else {
    format_error("Cannot find weights in this object. Please an open an issue!")
  }
}
