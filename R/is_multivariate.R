#' @title Checks if an object stems from a multivariate response model
#' @name is_multivariate
#'
#' @description Small helper that checks if a model is a multivariate response
#'   model, i.e. a model with multiple outcomes.
#'
#' @param x A model object, or an object returned by a function from this package.
#'
#' @return A logical, `TRUE` if either `x` is a model object and is
#'    a multivariate response model, or `TRUE` if a return value from a
#'    function of \pkg{insight} is from a multivariate response model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' data("pbcLong")
#' model <- stan_mvmer(
#'   formula = list(
#'     logBili ~ year + (1 | id),
#'     albumin ~ sex + year + (year | id)
#'   ),
#'   data = pbcLong,
#'   chains = 1, cores = 1, seed = 12345, iter = 1000
#' )
#'
#' f <- find_formula(model)
#' is_multivariate(model)
#' is_multivariate(f)
#' }
#' @export
is_multivariate <- function(x) {
  if (inherits(x, "gam", which = TRUE) == 1) {
    f <- .gam_family(x)
    gam_mv <- !is.null(f) && f$family == "Multivariate normal"
  } else {
    gam_mv <- FALSE
  }

  (inherits(x, "brmsfit") && !is.null(stats::formula(x)$response)) |
    inherits(x, "stanmvreg") | inherits(x, "mlm") | gam_mv | !is.null(attr(x, "is_mv", exact = TRUE))
}
