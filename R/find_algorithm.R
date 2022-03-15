#' @title Find sampling algorithm and optimizers
#' @name find_algorithm
#'
#' @description Returns information on the sampling or estimation algorithm
#'   as well as optimization functions, or for Bayesian model information on
#'   chains, iterations and warmup-samples.
#'
#' @inheritParams find_parameters
#'
#' @return A list with elements depending on the model.
#'   \cr
#'   For frequentist models:
#'    \itemize{
#'      \item `algorithm`, for instance `"OLS"` or `"ML"`
#'      \item `optimizer`, name of optimizing function, only applies to
#'      specific models (like `gam`)
#'    }
#'   For frequentist mixed models:
#'    \itemize{
#'      \item `algorithm`, for instance `"REML"` or `"ML"`
#'      \item `optimizer`, name of optimizing function
#'    }
#'   For Bayesian models:
#'    \itemize{
#'      \item `algorithm`, the algorithm
#'      \item `chains`, number of chains
#'      \item `iterations`, number of iterations per chain
#'      \item `warmup`, number of warmups per chain
#'    }
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   m <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#'   find_algorithm(m)
#' }
#' \dontrun{
#' library(rstanarm)
#' m <- stan_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' find_algorithm(m)
#' }
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
find_algorithm.Gam <- function(x, ...) {
  list("algorithm" = "IWLS")
}


#' @export
find_algorithm.lmRob <- function(x, ...) {
  list("algorithm" = x$robust.control$final.alg)
}


#' @export
find_algorithm.lmrob <- function(x, ...) {
  list("algorithm" = x$control$method)
}


#' @export
find_algorithm.glmrob <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.logistf <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.bigglm <- function(x, ...) {
  list("algorithm" = "ML")
}


#' @export
find_algorithm.BBreg <- function(x, ...) {
  list("algorithm" = "ML")
}


#' @export
find_algorithm.Arima <- function(x, ...) {
  list("algorithm" = "ML")
}


#' @export
find_algorithm.glimML <- function(x, ...) {
  list("algorithm" = "ML")
}


#' @export
find_algorithm.BBmm <- function(x, ...) {
  method <- parse(text = safe_deparse(x$call))[[1]]$method
  if (is.null(method)) method <- "BB-NR"
  list(algorithm = "extended likelihood", optimizer = method)
}


#' @export
find_algorithm.biglm <- function(x, ...) {
  list("algorithm" = "OLS")
}


#' @export
find_algorithm.gamlss <- function(x, ...) {
  list("algorithm" = as.character(x$method)[1])
}


#' @export
find_algorithm.gam <- function(x, ...) {
  list(
    "algorithm" = x$method,
    "optimizer" = x$optimizer
  )
}

#' @export
find_algorithm.scam <- find_algorithm.gam


#' @export
find_algorithm.lm <- function(x, ...) {
  list("algorithm" = "OLS")
}


#' @export
find_algorithm.systemfit <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.afex_aov <- function(x, ...) {
  list("algorithm" = "OLS")
}


#' @export
find_algorithm.speedlm <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.blavaan <- function(x, ...) {
  # installed?
  check_if_installed("blavaan")

  list(
    "chains" = blavaan::blavInspect(x, "n.chains"),
    "sample" = x@external$sample,
    "warmup" = x@external$burnin
  )
}


#' @export
find_algorithm.speedglm <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.rq <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.bayesx <- function(x, ...) {
  list(
    "algorithm" = x$method,
    "iterations" = x$iterations,
    "warmup" = x$burnin
  )
}


#' @export
find_algorithm.crq <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.rqss <- function(x, ...) {
  list("algorithm" = x$method)
}


#' @export
find_algorithm.glm <- function(x, ...) {
  list("algorithm" = "ML")
}


#' @export
find_algorithm.LORgee <- function(x, ...) {
  list("algorithm" = "Fisher's scoring ML")
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
find_algorithm.rlmerMod <- find_algorithm.merMod

#' @export
find_algorithm.merModList <- function(x, ...) {
  find_algorithm(x[[1]], ...)
}


#' @export
find_algorithm.mixed <- function(x, ...) {
  x <- x$full_model
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
find_algorithm.stanfit <- function(x, ...) {
  info <- x@sim
  algorithm <- unlist(x@stan_args)

  list(
    "algorithm" = as.vector(algorithm["algorithm"]),
    "chains" = info$chains,
    "iterations" = info$iter,
    "warmup" = info$warmup
  )
}


#' @export
find_algorithm.bayesQR <- function(x, ...) {
  list(
    "algorithm" = x[[1]]$method,
    "iterations" = nrow(x[[1]]$betadraw)
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
