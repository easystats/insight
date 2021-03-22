#' @title Get the model's function call
#' @name get_call
#'
#' @description Returns the model's function call when available.
#'
#' @inheritParams find_random
#'
#' @return A function call.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_call(m)
#'
#' if (require("lme4")) {
#'   m <- lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
#'   get_call(m)
#' }
#' @export
get_call <- function(x) {
  UseMethod("get_call")
}


#' @export
get_call.default <- function(x) {
  cl <- tryCatch(
    {
      x$call
    },
    error = function(x) {
      NULL
    }
  )

  if (is.null(cl)) {
    cl <- tryCatch(
      {
        x@call
      },
      error = function(x) {
        NULL
      }
    )
  }

  # For GAMM4
  if (is.null(cl) && "gam" %in% names(x)) {
    cl <- tryCatch(
      {
        x$gam$formula # Where's the call here?
      },
      error = function(x) {
        NULL
      }
    )
  }
  cl
}




#' @export
get_call.lm <- function(x) {
  x$call
}

#' @export
get_call.glm <- get_call.lm

#' @export
get_call.model_fit <- function(x) {
  get_call(x$fit)
}


#' @export
get_call.lmerMod <- function(x) {
  x@call
}
#' @export
get_call.merMod <- get_call.lmerMod
