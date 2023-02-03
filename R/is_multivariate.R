#' @title Checks if an object stems from a multivariate response model
#' @name is_multivariate
#'
#' @description Small helper that checks if a model is a multivariate response
#'   model, i.e. a model with multiple outcomes.
#'
#' @param x A model object, or an object returned by a function from this
#'   package.
#'
#' @return
#' A logical, `TRUE` if either `x` is a model object and is a multivariate
#' response model, or `TRUE` if a return value from a function of \pkg{insight}
#' is from a multivariate response model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' data("pbcLong")
#' model <- suppressWarnings(stan_mvmer(
#'   formula = list(
#'     logBili ~ year + (1 | id),
#'     albumin ~ sex + year + (year | id)
#'   ),
#'   data = pbcLong,
#'   chains = 1, cores = 1, seed = 12345, iter = 1000,
#'   show_messages = FALSE, refresh = 0
#' ))
#'
#' f <- find_formula(model)
#' is_multivariate(model)
#' is_multivariate(f)
#' }
#' @export
is_multivariate <- function(x) {
  mv_classes <- c("stanmvreg", "mlm", "mvord")
  if (inherits(x, mv_classes)) {
    return(TRUE)
  }

  if (inherits(x, "gam", which = TRUE) == 1) {
    f <- .gam_family(x)
    return(isTRUE(!is.null(f) && f$family == "Multivariate normal"))
  }

  if (inherits(x, "brmsfit")) {
    return(!is.null(stats::formula(x)$response))
  }

  if (!is.null(attr(x, "is_mv", exact = TRUE))) {
    return(TRUE)
  }

  if (inherits(x, "lm_robust")) {
    return(isTRUE(ncol(x$coefficients) > 1))
  }

  return(FALSE)
}
