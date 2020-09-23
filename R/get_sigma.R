#' @title Get residual standard deviation from models
#'
#' @description Returns the residual standard deviation from classical
#'   and mixed models.
#'
#' @name get_sigma
#'
#' @param x A model.
#'
#' @return The residual standard deviation (sigma), or \code{NULL} if this information could not be accessed.
#'
#' @details The residual standard deviation, \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}},
#'   indicates that the predicted outcome will be within +/- \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}}
#'   units of the linear predictor for approximately 68\% of the data points
#'   (\cite{Gelman, Hill & Vehtari 2020, p.84}). In other words, the residual
#'   standard deviation indicates the accuracy for a model to predict scores,
#'   thus it can be thought of as \dQuote{a measure of the average distance
#'   each observation falls from its prediction from the model}
#'   (\cite{Gelman, Hill & Vehtari 2020, p.168}). Thus, \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}}
#'   can be thought of as a measure of the unexplained variation in the data,
#'   or of the precision of inferences about regression coefficients.
#'
#' @references Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other Stories. Cambridge University Press.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_sigma(m)
#' @importFrom stats deviance sigma
#' @export
get_sigma <- function(x) {
  s <- tryCatch(
    {
      stats::sigma(x)
    },
    error = function(e) { NULL }
  )

  if (.is_empty_object(s)) {
    s <- tryCatch(
      {
        estimates <- get_parameters(x)$Estimate
        sqrt(stats::deviance(x) / (n_obs(x) - sum(!is.na(estimates))))
      },
      error = function(e) { NULL }
    )
  }

  if (.is_empty_object(s)) {
    info <- model_info(x)
    if (info$is_mixed) {
      s <- tryCatch(
        {
          sqrt(get_variance_residual(x, verbose = FALSE))
        },
        error = function(e) { NULL }
      )
    }
  }

  if (.is_empty_object(s) && inherits(x, "brmsfit")) {
    s <- tryCatch(
      {
        mean(get_parameters(x, component = "sigma")[["sigma"]])
      },
      error = function(e) { NULL }
    )
  }

  if (.is_empty_object(s)) {
    return(NULL)
  }
  s
}
