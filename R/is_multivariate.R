#' @title Checks if an object stems from a multivariate response model
#' @name is_multivariate
#'
#' @description to do...
#'
#' @param x A model object, or an object returned by a function from this package.
#'
#' @return A logical, \code{TRUE} if either \code{x} is a model object and is
#'    a multivariate response model, or \code{TRUE} if a return value from a
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
#' is_multivariate(f)}
#' @export
is_multivariate <- function(x) {
  (inherits(x, "brmsfit") && !is.null(stats::formula(x)$response)) |
    inherits(x, "stanmvreg") | !is.null(attr(x, "is_mv", exact = TRUE))
}
