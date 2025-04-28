#' @title Checks if a model is a Bayesian model
#' @name is_bayesian_model
#'
#' @description Small helper that checks if a model is a Bayesian model.
#'
#' @param x A model object.
#' @param exclude Optional character vector, indicating classes that should not
#' be included in the check. E.g., `exclude = "stanreg"` would return `FALSE`
#' for models from package **rstanarm**.
#'
#' @return A logical, `TRUE` if `x` is a Bayesian model.
#'
#' @examplesIf require("rstanarm", quietly = TRUE)
#' \donttest{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' is_bayesian_model(model)
#' }
#' @export
is_bayesian_model <- function(x, exclude = NULL) {
  # check emmeans and marginaleffects
  if (inherits(x, c("emmGrid", "emm_list"))) {
    return(.is_bayesian_emmeans(x))
  }
  if (inherits(x, c("marginaleffects", "slopes", "predictions", "comparisons"))) {
    return(.is_bayesian_marginaleffects(x))
  }
  bayes_classes <- c(
    "brmsfit", "stanfit", "MCMCglmm", "stanreg", "stanmvreg", "bmerMod",
    "BFBayesFactor", "bamlss", "bayesx", "mcmc", "bcplm", "bayesQR", "BGGM",
    "meta_random", "meta_fixed", "meta_bma", "blavaan", "blrm", "blmerMod",
    "bglmerMod"
  )
  # if exclude is not NULL, remove elements in exclude from bayes_class
  if (!is.null(exclude)) {
    bayes_classes <- bayes_classes[!bayes_classes %in% exclude]
  }
  inherits(x, bayes_classes)
}
