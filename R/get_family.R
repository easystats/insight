#' A robust alternative to stats::family
#'
#' A robust and resilient alternative to `stats::family`. To avoid issues
#' with models like `gamm4`.
#'
#' @param x A statistical model.
#' @param ... Further arguments passed to methods.
#'
#' @examplesIf require("mgcv")
#' data(mtcars)
#' x <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' get_family(x)
#'
#' x <- mgcv::gamm(
#'   vs ~ am + s(wt),
#'   random = list(cyl = ~1),
#'   data = mtcars,
#'   family = "binomial"
#' )
#' get_family(x)
#' @export
get_family <- function(x, ...) {
  UseMethod("get_family")
}


#' @export
get_family.default <- function(x, ...) {
  fam <- .safe(stats::family(x, ...))
  if (is.null(fam)) {
    fam <- .safe(.get_family(x, ...))
  }
  fam
}

#' @export
get_family.list <- function(x, ...) {
  if ("gam" %in% names(x)) {
    .get_family(x)
  } else {
    format_error("Could not retrieve family from this list. Check the input.")
  }
}

#' @export
get_family.model_fit <- function(x, ...) {
  get_family(x$fit, ...)
}

#' @export
get_family.sdmTMB <- function(x, ...) {
  check_if_installed("sdmTMB")
  f <- x$family
  if (length(f$family) > 1) {
    f <- compact_list(list(
      family = f$family[2],
      link = f$link[2],
      linkfun = f[[2]]$linkfun,
      linkinv = f[[2]]$linkinv,
      mu.eta = f[[2]]$mu.eta,
      valideta = f[[2]]$valideta,
      name = f[[2]]$name,
      initialize = f[[2]]$initialize
    ))
    class(f) <- "family"
  }
  f
}


.get_family <- function(x, ...) {
  info <- model_info(x, response = 1, verbose = FALSE)

  if (info$family == "binomial") {
    fam <- stats::binomial(link = info$link_function)
  } else if (info$is_linear) {
    fam <- stats::gaussian(link = "identity")
  } else if (info$is_poisson) {
    fam <- stats::poisson(link = info$link_function)
  } else {
    format_error(
      paste0("Could not retrieve family from this object of class `", class(x)[1], "`."),
      "Open an issue at {.url https://github.com/easystats/insight/issues}"
    )
  }
  fam
}
