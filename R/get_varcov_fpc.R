# Code adapted from Lai et al. 2018
# Lai, M. H. C., Kwok, O.-m., Hsiao, Y.-Y., & Cao, Q. (2018). Finite population
# correction for two-level hierarchical linear models. Psychological Methods,
# 23(1), 94–112. \doi{10.1037/met0000137}
#
# Code was modified using stricter sanity checks and extended to work with
# the glmmTMB package
#' @export
vcovFPC <- function(model, ...) {
  UseMethod("vcovFPC")
}


#' @export
vcovFPC.default <- function(model, ...) {
  format_error(paste0("Models of class `", class(model)[1], "` are not yet supported."))
}


#' @export
vcovFPC.merMod <- function(model, ...) {
  # sanity checks -------------------------------

  check_if_installed(c("lme4", "Matrix"))

  # only works for mixed models
  if (!is_mixed_model(model)) {
    format_error(
      "Finite population correction is only applicable to mixed models from packages 'lme4' and 'glmmTMB'."
    )
  }

  # only works for two-level models
  n_level <- length(find_random(model)$random)
  if (n_level != 1) {
    format_error(
      "Finite population correction is currently only supported for two-level models (one grouping factor)."
    )
  }

  vcov_args <- list(...)

  # user must specify at least one population size
  fpc1 <- vcov_args$population_size
  fpc2 <- vcov_args$cluster_size
  if (is.null(fpc1) && is.null(fpc2)) {
    format_error(
      "You must provide either `population_size` or `cluster_size` for finite population correction in the `vcov_args` argument.",
      "`population_size` refers to the population size on level 1, and `cluster_size` refers to the population size (number of clusters or groups) on level 2."
    )
  }

  # extract number of group levels
  n_grplevel <- n_grouplevels(model)
  if (!is.null(n_grplevel)) {
    n_grplevel <- n_grplevel$N_levels[1]
  }

  n <- n_obs(model)

  # check if correction needed at all?
  if (!is.null(fpc1) && fpc1 < n) {
    format_error("`population_size` must be larger than the sample size.")
  }
  if (!is.null(fpc2) && fpc2 < n_grplevel) {
    format_error(
      "`cluster_size` must be larger than the number of groups of random effects."
    )
  }

  # make sure fpc's are not NULL
  if (is.null(fpc1)) {
    fpc1 <- 1
  }
  if (is.null(fpc2)) {
    fpc2 <- 1
  }

  # adjust population size factor when it's larger than actual sample or group size
  if (fpc1 > n) {
    fpc1 <- 1 - n / fpc1
  }
  if (fpc2 > n_grplevel) {
    fpc2 <- 1 - n_grplevel / fpc2
  }

  # user wants KR adjustment?
  kr <- isTRUE(vcov_args$kenward_roger) || isTRUE(vcov_args$kr)

  # 1. Extract and scale the random effects structure -------------------------

  # Extract the transposed Cholesky factor of the random effects covariance
  # matrix (Lambda^T) and multiply it by the transposed random effects design
  # matrix (Z^T). Mathematically, this captures the unadjusted level-2 variance
  # structure.
  unscaled_re_structure <- as.matrix(
    .get_transposed_cholesky(model) %*% t(as.matrix(lme4::getME(model, "Z")))
  )

  # Apply the level-2 Finite Population Correction (FPC).
  # We scale the random effects structure by the square root of the level-2 FPC factor.
  scaled_re_structure <- unscaled_re_structure * sqrt(fpc2)

  # 2. Extract the fixed effects structure ------------------------------------

  # Extract the fixed effects design matrix (X)
  X <- lme4::getME(model, "X")

  # Project the scaled random effects structure onto the fixed effects design matrix
  scaled_re_fe <- scaled_re_structure %*% X

  # 3. Compute the Fisher Information Matrix ----------------------------------

  # Construct the modified diagonal matrix 'D'. This incorporates the level-1
  # FPC (fpc1) on the diagonal and the cross-product of the adjusted random
  # effects structure (scaled_re_structure %*% t(scaled_re_structure)).
  D <- as.matrix(
    Matrix::Diagonal(nrow(scaled_re_structure), fpc1) + tcrossprod(scaled_re_structure)
  )

  # Calculate the Fisher Information matrix for the fixed effects. Instead of a
  # computationally heavy matrix inversion of D, this uses solve(t(chol(D)),
  # scaled_re_fe) which is a highly optimized way to compute D^-1 * scaled_re_fe
  # by utilizing the Cholesky decomposition.
  fisher_info <- (crossprod(X) - crossprod(solve(t(chol(D)), scaled_re_fe))) / fpc1

  # 4. Compute the adjusted Variance-Covariance Matrix (Phi, vcov_fixed_effects) ------------------

  # The variance-covariance matrix of the fixed effects is the inverse of the
  # Fisher Information matrix, scaled by the estimated residual variance (sigma^2).
  vcov_fixed_effects <- solve(fisher_info) * (stats::sigma(model)^2)

  # Cast the resulting matrix to a positive-definite symmetric matrix ("dpoMatrix")
  # to ensure compatibility with downstream methods like vcov()
  vcov_fixed_effects <- as(vcov_fixed_effects, "dpoMatrix")

  # Assign the original fixed effect parameter names to the rows and columns
  fe_names <- colnames(X)
  dimnames(vcov_fixed_effects) <- list(fe_names, fe_names)

  if (kr) {
    vcov_fixed_effects <- .vcovAdj16_internal(vcov_fixed_effects, .get_SigmaG(model), X)
  }

  vcov_fixed_effects
}


#' @export
vcovFPC.glmmTMB <- vcovFPC.merMod


#' @export
vcovFPC.lm <- function(model, ...) {
  vcov_args <- list(...)
  if (is.null(vcov_args$population_size)) {
    format_error(
      "You must provide `population_size` for finite population correction in the `vcov_args` argument."
    )
  }

  N <- n_obs(model)
  V <- stats::vcov(model)
  popsize <- vcov_args$population_size

  if (popsize <= N) {
    format_error("`population_size` must be larger than the sample size.")
  }

  fpc <- (popsize - N) / (popsize - 1)
  return(V / fpc)
}


#' @export
vcovFPC.glm <- vcovFPC.lm


# helper -----------------------------------------------

.get_transposed_cholesky <- function(model) {
  if (inherits(model, "merMod")) {
    check_if_installed("lme4")
    return(lme4::getME(model, "Lambdat"))
  }

  if (inherits(model, "glmmTMB")) {
    check_if_installed(c("glmmTMB", "Matrix"))
    vc <- glmmTMB::VarCorr(model)$cond

    # 3. Get the number of levels for each grouping factor
    flist <- model$modelInfo$reTrms$cond$flist
    n_levels <- sapply(flist, nlevels)

    # 4. Construct the sparse, block-diagonal Cholesky factor matrix (L)
    L_list <- list()

    for (i in seq_along(vc)) {
      # Get the covariance matrix for this specific random effect term
      Sigma <- vc[[i]]

      # Calculate the lower triangular Cholesky factor
      # (R's chol() returns upper triangular, so we transpose it)
      L_block <- t(chol(Sigma))

      # Repeat this block for each level of the grouping factor
      blocks <- replicate(n_levels[i], L_block, simplify = FALSE)
      L_list[[i]] <- Matrix::bdiag(blocks)
    }

    # Combine all random effect terms into the final global Cholesky matrix
    L_matrix <- as.matrix(Matrix::bdiag(L_list))

    as.matrix(t(L_matrix / stats::sigma(model)))
  }
}
