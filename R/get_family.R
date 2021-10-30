#' A robust alternative to stats::family
#'
#' A robust and resilient alternative to `stats::family`. To avoid issues
#' with models like `gamm4`.
#'
#' @param x A statistical model.
#' @param ... Further arguments passed to methods.
#'
#' @examples
#' data(mtcars)
#' x <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' get_family(x)
#'
#' if (require("mgcv")) {
#'   x <- mgcv::gamm(
#'     vs ~ am + s(wt),
#'     random = list(cyl = ~1),
#'     data = mtcars,
#'     family = "binomial"
#'   )
#'   get_family(x)
#' }
#' @export
get_family <- function(x, ...) {
  UseMethod("get_family")
}


#' @export
get_family.default <- function(x, ...) {
  fam <- tryCatch(
    {
      stats::family(x, ...)
    },
    error = function(e) {
      NULL
    }
  )
  if (is.null(fam)) {
    fam <- tryCatch(
      {
        .get_family(x, ...)
      },
      error = function(e) {
        NULL
      }
    )
  }
  fam
}

#' @export
get_family.list <- function(x, ...) {
  if ("gam" %in% names(x)) {
    .get_family(x)
  } else {
    stop("Could not retrieve family from this list. Check the input.")
  }
}

#' @export
get_family.model_fit <- function(x, ...) {
  get_family(x$fit, ...)
}


.get_family <- function(x, ...) {
  info <- model_info(x, verbose = FALSE)

  if (info$family == "binomial") {
    fam <- stats::binomial(link = info$link_function)
  } else if (info$is_linear) {
    fam <- stats::gaussian(link = "identity")
  } else if (info$is_poisson) {
    fam <- stats::poisson(link = info$link_function)
  } else {
    stop("Could not retrieve family from this object. Open an issue on the insight's GitHub.")
  }
  fam
}
