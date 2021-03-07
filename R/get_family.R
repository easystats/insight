#' A robust alternative to stats::family
#'
#' A robust and resilient alternative to \code{stats::family}. To avoid issues with models like \code{gamm4}.
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
#'   x <- mgcv::gamm(vs ~ am + s(wt), random = list(cyl = ~1), data = mtcars, family = "binomial")
#'   get_family(x)
#' }
#'
#' \dontrun{
#' if (require("rstanarm")) {
#'   x <- stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0)
#'   get_family(x)
#' }
#' }
#'
#' @export
get_family <- function(x, ...) {
  UseMethod("get_family")
}


#' @importFrom stats family
#' @export
get_family.default <- function(x, ...) {
  stats::family(x, ...)
}

#' @export
get_family.list <- function(x, ...) {
  if("gam" %in% names(x)) {
    .get_family(x)
  } else {
    stop("Could not retrieve family from this list. Check the input.")
  }
}


#' @importFrom stats binomial gaussian Gamma inverse.gaussian poisson quasi quasibinomial quasipoisson
.get_family <- function(x, ...) {
  info <- model_info(x)

  if(info$is_logit) {
    fam <- stats::binomial(link = "logit")
  } else if(info$is_linear) {
    fam <- stats::gaussian(link = "identity")
  } else if(info$is_poisson) {
    fam <- stats::poisson(link = "log")
  } else {
    stop("Could not retrieve family from this object. Open an issue on the insight's GitHub.")
  }
  fam
}