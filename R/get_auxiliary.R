#' @title Get auxiliary parameters from models
#'
#' @description Returns the requested auxiliary parameters from models, like
#' dispersion, sigma, or beta...
#'
#' @name get_auxiliary
#'
#' @param x A model.
#' @param type The name of the auxiliary parameter that should be retrieved.
#' \code{"sigma"} is available for most models, \code{"dispersion"} for models
#' of class \code{glm}, \code{glmerMod} or \code{glmmTMB} as well as \code{brmsfit}.
#' \code{"beta"} is currently only returned for \code{brmsfit} models.
#'
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return The requested auxiliary parameter, or \code{NULL} if this information
#' could not be accessed.
#'
#' @details Currently, only sigma and the dispersion parameter are returned, and
#' only for a limited set of models.
#' \subsection{Sigma Parameter}{
#' See \code{\link{get_sigma}}.
#' }
#' \subsection{Dispersion Parameter}{
#' There are many different definitions of "dispersion", depending on the context.
#' \code{get_auxiliary()} returns the dispersion parameters that usually can
#' be considered as variance-to-mean ratio for generalized (linear) mixed
#' models. Exceptions are models of class \code{glmmTMB} and \code{brmsfit},
#' where the dispersion equals \ifelse{html}{\out{&sigma;<sup>2</sup>}}{\eqn{\sigma^2}}.
#' In detail, the computation of the dispersion parameter for generalized linear
#' models is the ratio of the sum of the squared working-residuals and the
#' residual degrees of freedom. For mixed models of class \code{glmer}, the
#' dispersion parameter is also called \ifelse{html}{\out{&phi;}}{\eqn{\phi}}
#' and is the ratio of the sum of the squared Pearson-residuals and the residual
#' degrees of freedom. For models of class \code{glmmTMB}, dispersion is
#' \ifelse{html}{\out{&sigma;<sup>2</sup>}}{\eqn{\sigma^2}}.
#' }
#'
#' @examples
#' # from ?glm
#' clotting <- data.frame(
#'   u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
#'   lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
#'   lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
#' )
#' model <- glm(lot1 ~ log(u), data = clotting, family = Gamma())
#' get_auxiliary(model, type = "dispersion") # same as summary(model)$dispersion
#' @importFrom stats sigma
#' @export
get_auxiliary <- function(x, type = c("sigma", "dispersion", "beta", "mix", "shiftprop"), verbose = TRUE, ...) {
  type <- match.arg(type)

  if (type == "sigma") {
    return(as.numeric(get_sigma(x)))
  } else if (type == "dispersion") {
    return(get_dispersion(x))
  } else {
    return(.get_generic_aux(x, type))
  }
}





# dispersion parameter -----------------------


get_dispersion <- function(x, ...) {
  UseMethod("get_dispersion")
}


get_dispersion.model_fit <- function(x, ...) {
  get_dispersion(x$fit, ...)
}


get_dispersion.glm <- function(x, ...) {
  info <- model_info(x)
  disp <- NULL

  if (info$is_poisson || info$is_binomial || info$is_negbin) {
    disp <- 1
  } else {
    working_weights <- get_weights(x, type = "working")
    working_res <- as.vector(get_residuals(x, type = "working"))^2 * working_weights
    disp <- sum(working_res[working_weights > 0]) / get_df(x, type = "residual")
  }
  disp
}


get_dispersion.glmerMod <- function(x, ...) {
  info <- model_info(x)
  disp <- NULL

  if (info$is_poisson || info$is_binomial || info$is_negbin) {
    disp <- 1
  } else {
    # see http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#fitting-models-with-overdispersion
    # phi is the dispersion factor, and phi is usually "sigma^2"
    # (https://stat.ethz.ch/pipermail/r-sig-mixed-models/2017q4/026168.html)
    # or the following ratio:
    res_df <- get_df(x, type = "residual")
    p_res <- get_residuals(x, type = "pearson")
    disp <- sum(p_res^2) / res_df
  }
  disp
}


get_dispersion.glmmTMB <- function(x, ...) {
  info <- model_info(x)
  disp <- NULL

  if (info$is_poisson || info$is_binomial || info$is_negbin) {
    disp <- 1
  } else {
    disp <- as.numeric(get_sigma(x))^2
  }
  disp
}


get_dispersion.brmsfit <- get_dispersion.glmmTMB



# special ------------------


.get_generic_aux <- function(x, param, ...) {
  aux <- NULL
  if (inherits(x, "brmsfit")) {
    aux <- mean(as.data.frame(x)[[param]], na.rm = TRUE)
  }
  aux
}
