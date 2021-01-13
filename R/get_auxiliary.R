#' @title Get auxiliary parameters from models
#'
#' @description Returns the requested auxiliary parameters from models, like
#' dispersion, sigma, or beta...
#'
#' @name get_auxiliary
#'
#' @param x A model.
#'
#' @return The requested auxiliary parameter, or \code{NULL} if this information
#' could not be accessed.
#'
#' @details Currently, only sigma and the dispersion parameter are returned, and
#' only for a limited set of models.
#'
#' @examples
#' # from ?glm
#' clotting <- data.frame(
#'   u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
#'   lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
#'   lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
#' )
#' model <- glm(lot1 ~ log(u), data = clotting, family = Gamma())
#' get_auxiliary(model) # same as summary(model)$dispersion
#' @importFrom stats sigma
#' @export
get_auxiliary <- function(x, type = c("sigma", "dispersion"), verbose = TRUE, ...) {
  type <- match.arg(type)

  if (type == "sigma") {
    return(as.numeric(get_sigma(x)))
  } else if (type == "dispersion") {
    return(get_dispersion(x))
  }
}





# dispersion parameter -----------------------


get_dispersion <- function(x, ...) {
  UseMethod("get_dispersion")
}


get_dispersion.glm <- function(x, ...) {
  info <- model_info(x)
  disp <- NULL

  if (info$is_poisson || info$is_binomial || info$is_negbin) {
    disp <- 1
  } else {
    working_weights <- get_weights(x, type = "working")
    working_res <- sum(as.vector(get_residuals(x, type = "working"))^2 * working_weights)
    disp <- working_res[working_weights > 0] / get_df(x, type = "residual")
  }
  disp
}
