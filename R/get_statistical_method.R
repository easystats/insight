#' @name get_statistical_method
#' @title Guess requested statistical method
#'
#' @description
#'
#' Relevant for packages (e.g., `ggstatsplot`, `statsExpressions`, etc.), where
#' different types of statistical methods are supported. This switch function
#' converts strings entered by users to a common pattern.
#'
#' Possible outputs are:
#' \itemize{
#'  \item{parametric}
#'  \item{non-parametric}
#'  \item{robust}
#'  \item{bayesian}
#' }
#'
#' @param method Character string describing the method/type of statistics.
#'
#' @examples
#' get_statistical_method("p")
#' get_statistical_method("bf")
#' @export

get_statistical_method <- function(method) {
  switch(substring(method, 1, 1),
    "p" = "parametric",
    "n" = "nonparametric",
    "r" = "robust",
    "b" = "bayesian",
    "parametric"
  )
}
