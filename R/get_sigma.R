#' @title Get residual standard deviation from models
#'
#' @description Returns the residual standard deviation from classical
#'   and mixed models.
#'
#' @name get_sigma
#'
#' @param x A model.
#'
#' @return The residual standard deviation (sigma)
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_sigma(m)
#' @importFrom stats deviance
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
      s <- sqrt(get_variance_residual(x, verbose = FALSE))
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

  s
}
