# Get Variance-covariance Matrix ---------------------------------------------------

.get_varcov_sandwich <- function(
  x,
  vcov_fun = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
) {
  dots <- list(...)

  # make sure we have a "matrix" class
  if (inherits(vcov_fun, "Matrix") || inherits(vcov_fun, "dpoMatrix")) {
    check_if_installed("Matrix")
    vcov_fun <- as.matrix(vcov_fun)
  }

  if (is.null(vcov_args)) {
    vcov_args <- list()
  }

  # vcov_fun is a matrix
  if (is.matrix(vcov_fun)) {
    return(vcov_fun)
  }

  # vcov_fun is a function
  if (is.function(vcov_fun)) {
    if (is.null(vcov_args) || !is.list(vcov_args)) {
      my_args <- list(x)
    } else {
      my_args <- c(list(x), vcov_args)
    }
    .vcov <- do.call("vcov_fun", my_args)
    return(.vcov)
  }

  # fmt: skip
  vcov_type_shortcuts <- c(
    "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "CR0", "CR1",
    "CR1p", "CR1S", "CR2", "CR3", "xy", "residual", "wild", "mammen",
    "webb", "fractional", "jackknife", "norm"
  )

  # type shortcuts: overwrite only if not supplied explicitly by the user
  if (!"type" %in% names(vcov_args)) {
    if (isTRUE(vcov_fun %in% vcov_type_shortcuts)) {
      vcov_args[["type"]] <- vcov_fun
    } else if (is.null(vcov_fun)) {
      # set defaults
      vcov_args[["type"]] <- switch(vcov_fun, CR = "CR3", NULL)
    }
  }

  # default vcov matrix
  if (is.null(vcov_fun)) {
    .vcov <- get_varcov(x, ...)
    return(.vcov)
  }

  ## TODO: what about Sattherthwaite? @vincentarelbundock

  if (grepl("^(vcov|kernHAC|NeweyWest)", vcov_fun)) {
    vcov_fun_clean <- vcov_fun
  } else {
    vcov_fun_clean <- switch(
      vcov_fun,
      HC0 = ,
      HC1 = ,
      HC2 = ,
      HC3 = ,
      HC4 = ,
      HC4m = ,
      HC5 = ,
      HC = "vcovHC",
      CR0 = ,
      CR1 = ,
      CR1p = ,
      CR1S = ,
      CR2 = ,
      CR3 = ,
      CR = "vcovCR",
      xy = ,
      residual = ,
      norm = ,
      jackknife = ,
      fractional = ,
      wild = ,
      mammen = ,
      webb = ,
      BS = "vcovBS",
      OPG = "vcovOPG",
      HAC = "vcovHAC",
      PC = "vcovPC",
      CL = "vcovCL",
      PL = "vcovPL",
      `kenward-roger` = "vcovAdj",
      fpc = "vcovFPC"
    )
  }

  # check if required package is available
  if (is.character(vcov_fun) && is.null(vcov_fun_clean)) {
    format_error(sprintf(
      "`%s` is not a recognized value for the `vcov` argument.",
      vcov_fun[1]
    ))
  } else if (isTRUE(vcov_fun_clean == "vcovAdj")) {
    if (inherits(x, "glmmTMB")) {
      # for glmmTMB, we have the KR-adjusted vcov as attributed saved
      # in the degrees of freedom
      dof <- .degrees_of_freedom_kr(x, verbose = verbose)
      return(attributes(dof)$vcov)
    } else {
      # for other packages, we need pbkrtest
      check_if_installed("pbkrtest")
      fun <- try(get(vcov_fun_clean, asNamespace("pbkrtest")), silent = TRUE)
    }
  } else if (isTRUE(vcov_fun_clean == "vcovCR")) {
    check_if_installed("clubSandwich", reason = "to get cluster-robust standard errors")
    fun <- try(get(vcov_fun_clean, asNamespace("clubSandwich")), silent = TRUE)
  } else if (isTRUE(vcov_fun_clean == "vcovFPC")) {
    return(.vcov_fpc(x, vcov_args))
  } else {
    check_if_installed("sandwich", reason = "to get robust standard errors")
    fun <- try(get(vcov_fun_clean, asNamespace("sandwich")), silent = TRUE)
    if (!is.function(fun) && is.character(vcov_fun)) {
      format_error(sprintf(
        "`%s` is not a function exported by the `sandwich` package.",
        vcov_fun[1]
      ))
    }
  }

  vcov_fun <- vcov_fun_clean

  # for glmmTMB models, we allow "full = TRUE" to get the full vcov matrix
  if (isTRUE(dots$full) && inherits(x, "glmmTMB")) {
    vcov_args[["full"]] <- TRUE
  }

  # try with arguments
  .vcov <- try(do.call(fun, c(list(x), vcov_args)), silent = TRUE)
  if (!inherits(.vcov, "try-error")) {
    .vcov <- as.matrix(.vcov) # weird matrix classes in clubSandwich and vcovAdj
  }

  # extract variance-covariance matrix
  if (!inherits(.vcov, "matrix")) {
    msg <- sprintf(
      "Unable to extract a variance-covariance matrix for model object of class `%s`. Different values of the `vcov` argument trigger calls to the `sandwich` or `clubSandwich` packages in order to extract the matrix (see `?insight::get_varcov`). Your model or the requested estimation type may not be supported by one or both of those packages, or you were missing one or more required arguments in `vcov_args` (like `cluster`).",
      class(x)[1]
    ) # nolint
    if (inherits(.vcov, "try-error")) {
      msg <- c(msg, "", "This error was raised:", attr(.vcov, "condition")$message)
    }
    format_error(msg)
  }

  .vcov
}


# Code adapted from Lai et al. 2018
# Lai, M. H. C., Kwok, O.-m., Hsiao, Y.-Y., & Cao, Q. (2018). Finite population
# correction for two-level hierarchical linear models. Psychological Methods,
# 23(1), 94–112. \doi{10.1037/met0000137}
#
# Code was modified using stricter sanity checks and extended to work with
# the glmmTMB package
.vcov_fpc <- function(model, vcov_args = NULL) {
  # sanity checks -------------------------------

  check_if_installed(c("lme4", "Matrix"))

  # only works for mixed models
  if (!is_mixed_model(model)) {
    format_error(
      "Finite population correction is only applicable to mixed models from packages 'lme4' and 'glmmTMB'."
    )
  }

  # only works for merMod and glmmTMB
  if (!inherits(model, c("merMod", "glmmTMB"))) {
    format_error(
      "Finite population correction is currently only supported for model from packages 'lme4' and 'glmmTMB'."
    )
  }

  # only works for two-level models
  n_level <- length(find_random(model)$random)
  if (n_level != 1) {
    format_error(
      "Finite population correction is currently only supported for two-level models (one grouping factor)."
    )
  }

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
    SigmaG <- .get_SigmaG(model)
    vcov_fixed_effects <- as(
      .vcovAdj16_internal(vcov_fixed_effects, SigmaG, X),
      "dpoMatrix"
    )
  }

  vcov_fixed_effects
}


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
    L_matrix <- Matrix::bdiag(L_list)

    as.matrix(t(L_matrix / stats::sigma(model)))
  }
}
