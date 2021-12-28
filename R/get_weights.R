#' @title Get the values from model weights
#' @name get_weights
#'
#' @description Returns weighting variable of a model.
#'
#' @param x A fitted model.
#' @param na_rm Logical, if `TRUE`, removes possible missing values.
#' @param null_as_ones Logical, if `TRUE`, will return a vector of `1`
#'   if no weights were specified in the model (as if the weights were all set
#'   to 1).
#' @param ... Currently not used.
#'
#' @return The weighting variable, or `NULL` if no weights were specified
#' or if weights were 1. If the weighting variable should also be returned
#' (instead of `NULL`), when all weights are set to 1 (i.e. no weighting),
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


#' @rdname get_weights
#' @export
get_weights.default <- function(x, na_rm = FALSE, null_as_ones = FALSE, ...) {
  w <- NULL
  tryCatch(
    {
      w <- stats::weights(x, ...)
    },
    error = function(e) {
      NULL
    },
    warning = function(w) {
      NULL
    }
  )

  if (is.null(w)) {
    tryCatch(
      {
        w <- stats::model.frame(x)[["(weights)"]]
      },
      error = function(e) {
        NULL
      },
      warning = function(w) {
        NULL
      }
    )
  }

  if (is.null(w)) {
    tryCatch(
      {
        w <- .recover_data_from_environment(x)[[find_weights(x)]]
      },
      error = function(e) {
        NULL
      },
      warning = function(w) {
        NULL
      }
    )
  }

  # if all weights are 1, set return value to NULL
  if (!is.null(w) && all(w == 1L)) {
    w <- NULL
  }

  if (!is.null(w) && anyNA(w) && isTRUE(na_rm)) {
    w <- stats::na.omit(w)
  }

  if (is.null(w) && isTRUE(null_as_ones)) {
    w <- rep.int(1, n_obs(x))
  }

  w
}


#' @export
get_weights.brmsfit <- function(x, na_rm = FALSE, null_as_ones = FALSE, ...) {
  w <- unique(find_weights(x))

  if (!is.null(w)) {
    if (length(w) > 1) {
      return(get_data(x)[w])
    } else {
      w <- get_data(x)[[w]]
    }
  }

  if (!is.null(w) && all(w == 1L)) {
    w <- NULL
  }

  if (!is.null(w) && anyNA(w) && isTRUE(na_rm)) {
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
get_weights.list <- function(x, na_rm = FALSE, null_as_ones = FALSE, ...) { # For GAMMs
  if ("gam" %in% names(x)) {
    get_weights(x$gam, na_rm = na_rm, null_as_ones = null_as_ones, ...)
  } else {
    stop("Cannot find weights in this object. Please an open an issue!")
  }
}
