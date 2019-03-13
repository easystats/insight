#' @title Find sampling algorithm and optimizers
#' @name find_algorithm
#'
#' @description Returns information on the sampling or estimation algorithm
#'   as well as optimization functions, or for Bayesian model information on
#'   chains, iterations and warmup-samples.
#'
#' @inheritParams find_predictors
#'
#' @return A list with following elements:
#'   \cr For frequentist models:
#'    \itemize{
#'      \item \code{algorithm}, either \code{"OLS"} or \code{"ML"}
#'    }
#'   \cr For frequentist mixed models:
#'    \itemize{
#'      \item \code{algorithm}, either \code{"REML"} or \code{"ML"}
#'      \item \code{optimizer}, name of optimizing function
#'    }
#'   \cr For Bayesian models:
#'    \itemize{
#'      \item \code{algorithm}, the algirithm
#'      \item \code{chains}, number of chains
#'      \item \code{iterations}, number of iterations per chain
#'      \item \code{warmup}, number of warmups per chain
#'    }
#'
#'
#' @examples
#' library(lme4)
#' data(sleepstudy)
#' m <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' find_algorithm(m)
#'
#' \dontrun{
#' library(rstanarm)
#' m <- stan_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' find_algorithm(m)}
#'
#' @export
find_algorithm <- function(x, ...) {
  UseMethod("find_algorithm")
}


#' @export
find_algorithm.default <- function(x, ...) {
  warning(sprintf("Objects of class `%s` are not supported.", class(x)[1]))
  NULL
}


#' @export
find_algorithm.lm <- function(x, ...) {
  list(
    "algorithm" = "OLS",
  )
}


#' @export
find_algorithm.glm <- function(x, ...) {
  list(
    "algorithm" = "ML",
  )
}


#' @export
find_algorithm.merMod <- function(x, ...) {
  algorithm <- ifelse(as.logical(x@devcomp$dims[["REML"]]), "REML", "ML")

  list(
    "algorithm" = algorithm,
    "optimizer" = as.character(x@optinfo$optimizer)
  )
}


#' @export
find_algorithm.lme <- function(x, ...) {
  optimizer <- "nlminb"
  if (!is.null(x$call$control) && "optim" %in% as.character(x$call$control)) {
    optimizer <- "optim"
  }

  list(
    "algorithm" = x$method,
    "optimizer" = optimizer
  )
}


#' @export
find_algorithm.MixMod <- function(x, ...) {
  list(
    ## TODO fix me
    "algorithm" = "quasi-Newton",
    "optimizer" = x$control$optimizer
  )
}


#' @export
find_algorithm.glmmTMB <- function(x, ...) {
  algorithm <- ifelse(x$modelInfo$REML, "REML", "ML")

  list(
    "algorithm" = algorithm,
    "optimizer" = "nlminb"
  )
}


#' @export
find_algorithm.stanreg <- function(x, ...) {
  info <- x$stanfit@sim

  list(
    "algorithm" = x$algorithm,
    "chains" = info$chains,
    "iterations" = info$iter,
    "warmup" = info$warmup
  )
}


#' @export
find_algorithm.brmsfit <- function(x, ...) {
  info <- x$fit@sim

  list(
    "algorithm" = x$algorithm,
    "chains" = info$chains,
    "iterations" = info$iter,
    "warmup" = info$warmup
  )
}