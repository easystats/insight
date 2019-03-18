#' @title Get summary of priors used for a model
#' @name get_priors
#'
#' @description Provides a summary of the prior distributions used
#'   for the parameters in a given model.
#'
#' @param x A Bayesian model.
#' @param ... Currently not used.
#'
#' @return A data frame with a summary of the prior distributions used
#'   for the parameters in a given model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data=iris)
#' get_priors(model)}
#'
#' @export
get_priors <- function(x, ...) {
  UseMethod("get_priors")
}


#' @export
get_priors.stanreg <- function(x, ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("To use this function, please install package 'rstanarm'.")
  }

  ps <- rstanarm::prior_summary(x)

  l <- lapply(ps[c("prior_intercept", "prior")], function(x) {
    do.call(cbind, x)
  })

  prior_info <- as.data.frame(do.call(rbind, l), stringsAsFactors = FALSE)
  prior_info$parameter <- find_parameters(x)$conditional

  prior_info <- prior_info[, c("parameter", "dist", "location", "scale", "adjusted_scale")]

  names(prior_info) <- gsub("dist", "distribution", names(prior_info))
  names(prior_info) <- gsub("df", "DoF", names(prior_info))

  as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x))
      as.numeric(x)
    else
      x
  }))
}


#' @export
get_priors.brmsfit <- function(x, ...) {
  ## TODO needs testing for edge cases - check if "coef"-row is
  # always empty for intercept-class
  x$prior$coef[x$prior$class == "Intercept"] <- "(Intercept)"

  prior_info <- x$prior[x$prior$coef != "" & x$prior$class %in% c("b", "(Intercept)"), ]

  prior_info$distribution <- gsub("(.*)\\(.*", "\\1", prior_info$prior)
  prior_info$scale <- gsub("(.*)\\((.*)\\,(.*)", "\\2", prior_info$prior)
  prior_info$location <- gsub("(.*)\\,(.*)\\)(.*)", "\\2", prior_info$prior)
  prior_info$parameter <- prior_info$coef

  prior_info <- prior_info[, c("parameter", "distribution", "location", "scale")]

  as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x))
      as.numeric(x)
    else
      x
  }))
}


.is_numeric_character <- function(x) {
  is.character(x) && !anyNA(suppressWarnings(as.numeric(na.omit(x))))
}
