#' @name format_statistical_approach
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
#' format_statistical_approach("p")
#' format_statistical_approach("bf")
#' @export

format_statistical_approach <- function(method) {
  switch(substring(method, 1, 1),
    "p" = "parametric",
    "n" = "nonparametric",
    "r" = "robust",
    "b" = "bayesian",
    "parametric"
  )
}
