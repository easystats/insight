#' Obtained finite-population-adjusted standard errors
#'
#' This function return the variance-covariance matrix as returned by `vcov()`,
#' but with finite population correction (FPC) applied.
#'
#' @param object a model inheriting from `lm` or `merMod` (2-level only).
#' @param popsize,popsize1,popsize2 The finite population size (for `merMod`
#'   specified seperetly for level-1 and level-2). If `NULL`, an infinite
#'   population is assumed.
#' @param verbose Whether to print messages about whether FPC is applied or not.
#'
#' @details
#' The FPC is defined as:
#' \cr\cr
#' \deqn{FPC = \frac{N - n}{N - 1}}{FPC = (N - n) / (N - 1)}
#' \cr\cr
#' FPC for multilevel models is based on the method described by Lai et al. (2018).
#'
#' @return The variance-covariance matrix of the fixed effect estimates, as
#'   returned by `vcov()`, but with FPC applied.
#'
#' @references Lai, M. H., Kwok, O. M., Hsiao, Y. Y., & Cao, Q. (2018). Finite population correction for two-level hierarchical linear models. _Psychological methods, 23_(1), 94.
#'
#' @export
vcovFPC <- function(object, ..., verbose = TRUE) {
  UseMethod("vcovFPC")
}

#' @export
#' @rdname vcovFPC
vcovFPC.lm <- function(object, popsize = NULL, ..., verbose = TRUE) {
  N <- insight::n_obs(object)
  V <- stats::vcov(object)
  if (popsize <= N) {
    if (verbose) {
      insight::format_alert("No FPC needed.")
    }
    return(V)
  }

  fpc <- (popsize - N) / (popsize - 1)
  return(V / fpc)
}

#' @export
#' @rdname vcovFPC
vcovFPC.merMod <- function(
  object,
  popsize2 = NULL,
  popsize1 = NULL,
  ...,
  verbose = TRUE
) {
  # TODO: KR support from original paper is broken and not implemented
  if (length(object@flist) != 1) {
    stop("Wrong input: Only models with two levels are supported")
  }
  if (is.null(popsize1) & is.null(popsize2)) {
    message("No FPC specified; return results from lme4::vcov.merMod()")
    return(vcov(object))
  }
  PR <- object@pp
  N <- unname(object@devcomp$dims["n"])
  nclus <- unname(ngrps(object))
  if (isTRUE(popsize2 > nclus)) {
    fpc2 <- 1 - nclus / popsize2
  } else {
    fpc2 <- 1
    if (verbose) {
      insight::format_alert("No FPC needed at level-2")
    }
  }
  if (isTRUE(popsize1 > N)) {
    fpc1 <- 1 - N / popsize1
  } else {
    fpc1 <- 1
    if (verbose) {
      insight::format_alert("No FPC needed at level-1")
    }
  }
  if (fpc1 == 1 & fpc2 == 1) {
    return(vcov(object))
  }
  A <- PR$Lambdat %*% PR$Zt
  Astar <- A * sqrt(fpc2)
  X <- PR$X
  Astar_X <- Astar %*% X
  D <- Matrix::Diagonal(nrow(Astar), fpc1) + tcrossprod(Astar)
  Fisher_I <- (crossprod(X) - crossprod(solve(t(chol(D)), Astar_X))) / fpc1
  Phi <- solve(Fisher_I) * sigma(object)^2
  Phi <- as(Phi, "dpoMatrix")
  nmsX <- colnames(X)
  dimnames(Phi) <- list(nmsX, nmsX)
  return(Phi)
}
