#' @title Get variance-covariance matrix from models
#' @name get_varcov
#'
#' @description
#' Returns the variance-covariance, as retrieved by `stats::vcov()`, but works
#' for more model objects that probably don't provide a `vcov()`-method.
#'
#'
#' @param x A model.
#' @param component Should the complete variance-covariance matrix of the model
#' be returned, or only for specific model components only (like count or
#' zero-inflated model parts)? Applies to models with zero-inflated component,
#' or models with precision (e.g. `betareg`) component. `component` may be one
#' of `"conditional"`, `"zi"`, `"zero-inflated"`, `"dispersion"`, `"precision"`,
#' or `"all"`. May be abbreviated. Note that the *conditional* component also
#' refers to the *count* or *mean* component - names may differ, depending on
#' the modeling package. See section _Model components_ for details.
#' @param effects Should the complete variance-covariance matrix of the model
#' be returned, or only for specific model parameters only? Currently only
#' applies to models of class `mixor` and `MixMod`.
#' @param complete Logical, if `TRUE`, for `aov`, returns the full
#' variance-covariance matrix.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates
#' (e.g., for robust standard errors). This argument accepts a covariance
#' matrix, a function which returns a covariance matrix, or a string which
#' identifies the function to be used to compute the covariance matrix.
#'  * A covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
#'      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`, `"CR2"`,
#'      `"CR3"`. See `?clubSandwich::vcovCR`
#'    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
#'      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
#'      `?sandwich::vcovBS`
#'    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`, `"OPG"`,
#'      `"PL"`.
#'    - Kenward-Roger approximation: `kenward-roger`. See `?pbkrtest::vcovAdj`.
#'
#' One exception are models of class `glmgee`, which have pre-defined options
#' for the variance-covariance matrix calculation. These are `"robust"`,
#' `"df-adjusted"`, `"model"`, `"bias-corrected"`, and `"jackknife"`. See
#' `?glmtoolbox::vcov.glmgee` for details.
#' @param vcov_args List of arguments to be passed to the function identified by
#'   the `vcov` argument. This function is typically supplied by the
#'   **sandwich** or **clubSandwich** packages. Please refer to their
#'   documentation (e.g., `?sandwich::vcovHAC`) to see the list of available
#'   arguments. If no estimation type (argument `type`) is given, the default
#'   type for `"HC"` equals the default from the **sandwich** package; for type
#'   `"CR"`, the default is set to `"CR3"`.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @note `get_varcov()` tries to return the nearest positive definite matrix
#'   in case of negative eigenvalues of the variance-covariance matrix. This
#'   ensures that it is still possible, for instance, to calculate standard
#'   errors of model parameters. A message is shown when the matrix is negative
#'   definite and a corrected matrix is returned.
#'
#' @inheritSection find_predictors Model components
#'
#' @return The variance-covariance matrix, as `matrix`-object.
#'
#' @examplesIf require("pscl") && require("sandwich")
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_varcov(m)
#'
#' # vcov of zero-inflation component from hurdle-model
#' data("bioChemists", package = "pscl")
#' mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
#' get_varcov(mod, component = "zero_inflated")
#'
#' # robust vcov of, count component from hurdle-model
#' data("bioChemists", package = "pscl")
#' mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
#' get_varcov(
#'   mod,
#'   component = "conditional",
#'   vcov = "BS",
#'   vcov_args = list(R = 50)
#' )
#' @export
get_varcov <- function(x, ...) {
  UseMethod("get_varcov")
}


# Default models ----------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.default <- function(x,
                               verbose = TRUE,
                               vcov = NULL,
                               vcov_args = NULL,
                               ...) {
  .check_get_varcov_dots(x, ...)
  # process vcov-argument
  vcov <- .check_vcov_args(x, vcov = vcov, verbose = verbose, ...)

  if (is.null(vcov)) {
    vc <- .safe_vcov(x)
  } else {
    vc <- .get_varcov_sandwich(x,
      vcov_fun = vcov,
      vcov_args = vcov_args,
      verbose = FALSE,
      ...
    )
  }
  .process_vcov(vc, verbose, ...)
}

#' @export
get_varcov.maxLik <- get_varcov.default

#' @export
get_varcov.HLfit <- get_varcov.default

#' @export
get_varcov.geeglm <- get_varcov.default


# fixest ---------------------------------------------

#' @export
get_varcov.fixest <- function(x,
                              vcov = NULL,
                              vcov_args = NULL,
                              ...) {
  # fixest supplies its own mechanism. Vincent thinks it might not be wise to
  # try `sandwich`, because there may be inconsistencies.
  check_if_installed("fixest")
  my_args <- c(list(x, vcov = vcov), vcov_args)
  FUN <- stats::vcov
  do.call("FUN", my_args)
}


#' @export
get_varcov.asym <- function(x, ...) {
  out <- get_varcov.default(x, ...)
  colnames(out) <- gsub("^plus__", "+", colnames(out))
  rownames(out) <- gsub("^plus__", "+", rownames(out))
  colnames(out) <- gsub("^minus__", "-", colnames(out))
  rownames(out) <- gsub("^minus__", "-", rownames(out))
  out
}


# mlm ---------------------------------------------

#' @export
get_varcov.mlm <- function(x,
                           vcov = NULL,
                           vcov_args = NULL,
                           ...) {
  .check_get_varcov_dots(x, ...)
  if (is.null(x$weights)) {
    get_varcov.default(x, vcov = vcov, vcov_args = vcov_args, ...)
  } else {
    if (!is.null(vcov)) {
      format_error("The `vcov` argument is not supported with weights in a `mlm` model.")
    }
    s <- summary(x)[[1L]]
    .get_weighted_varcov(x, s$cov.unscaled)
  }
}


# models with special components ---------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.glmgee <- function(x,
                              verbose = TRUE,
                              vcov = "robust",
                              ...) {
  vcov <- validate_argument(
    vcov,
    c("robust", "df-adjusted", "model", "bias-corrected", "jackknife")
  )
  vc <- suppressWarnings(stats::vcov(x, type = vcov))
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.nestedLogit <- function(x,
                                   component = "all",
                                   verbose = TRUE,
                                   vcov = NULL,
                                   vcov_args = NULL,
                                   ...) {
  vcovs <- lapply(
    x$models,
    get_varcov,
    verbose = verbose,
    vcov = vcov,
    vcov_args = vcov_args,
    ...
  )

  if (identical(component, "all") || is.null(component)) {
    return(vcovs)
  }

  comp <- intersect(names(vcovs), component)
  if (!length(comp)) {
    if (verbose) {
      format_alert(
        paste0(
          "No matching model found. Possible values for `component` are ",
          toString(paste0("\"", names(vcovs), "\"")),
          "."
        )
      )
    }
    return(NULL)
  }

  vcovs[comp]
}


#' @export
get_varcov.betareg <- function(x,
                               component = "conditional",
                               verbose = TRUE,
                               ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "precision", "all"))

  vc <- switch(component,
    conditional = stats::vcov(object = x, model = "mean"),
    precision = stats::vcov(object = x, model = "precision"),
    stats::vcov(object = x)
  )
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.DirichletRegModel <- function(x,
                                         component = "conditional",
                                         verbose = TRUE,
                                         ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "precision", "all"))
  if (x$parametrization == "common") {
    vc <- .safe_vcov(x)
  } else if (component == "conditional") {
    vc <- .safe_vcov(x)
    keep <- grepl("^(?!\\(phi\\))", rownames(vc), perl = TRUE)
    vc <- vc[keep, keep, drop = FALSE]
  } else if (component == "precision") {
    vc <- .safe_vcov(x)
    keep <- startsWith(rownames(vc), "(phi)")
    vc <- vc[keep, keep, drop = FALSE]
  } else {
    vc <- .safe_vcov(x)
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.clm2 <- function(x, component = "all", ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("all", "conditional", "scale"))

  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  vc <- .safe_vcov(x)

  if (.is_negativ_matrix(vc, ...)) {
    vc <- .fix_negative_matrix(vc)
  }

  col_range <- switch(component,
    all = 1:(n_scale + n_intercepts + n_location),
    conditional = 1:(n_intercepts + n_location),
    scale = (1 + n_intercepts + n_location):(n_scale + n_intercepts + n_location)
  )

  vc <- vc[col_range, col_range, drop = FALSE]

  # fix possible missings due to rank deficient model matrix
  vc <- .fix_rank_deficiency(vc)

  text_remove_backticks(as.matrix(vc))
}

#' @export
get_varcov.clmm2 <- get_varcov.clm2


#' @export
get_varcov.glmx <- function(x,
                            component = "all",
                            verbose = TRUE,
                            ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("all", "conditional", "extra"))
  vc <- stats::vcov(object = x)

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.pgmm <- function(x,
                            component = "conditional",
                            vcov = NULL,
                            vcov_args = NULL,
                            verbose = TRUE,
                            ...) {
  .check_get_varcov_dots(x, ...)
  # process vcov-argument
  vcov <- .check_vcov_args(x, vcov = vcov, verbose = verbose, ...)
  component <- validate_argument(component, c("conditional", "all"))

  if (is.null(vcov)) {
    vc <- .safe_vcov(x)
  } else {
    vc <- .get_varcov_sandwich(x,
      vcov_fun = vcov,
      vcov_args = vcov_args,
      verbose = FALSE,
      ...
    )
  }

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.selection <- function(x, component = "all", ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("all", "selection", "outcome", "auxiliary"))
  vc <- stats::vcov(object = x)

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }

  # we can't check for rank-deficiency here...
  if (.is_negativ_matrix(vc, ...)) {
    vc <- .fix_negative_matrix(vc)
  }
  text_remove_backticks(as.matrix(vc))
}


#' @export
get_varcov.mvord <- function(x,
                             component = "all",
                             verbose = TRUE,
                             ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(
    component,
    c("all", "conditional", "thresholds", "correlation")
  )
  vc <- .safe_vcov(x)

  if (component != "all") {
    fp <- find_parameters(x)[[component]]
    if (component == "thresholds") {
      fp <- gsub("\\s", "\\.", fp)
    }
    keep <- match(fp, rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.mjoint <- function(x, component = "all", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("all", "conditional", "survival"))
  vc <- .safe_vcov(x)

  keep <- match(find_parameters(x, flatten = TRUE, component = component), rownames(vc))
  vc <- vc[keep, keep, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.mhurdle <- function(x, component = "all", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(
    component,
    c(
      "all", "conditional", "zi", "zero_inflated", "infrequent_purchase",
      "ip", "auxiliary"
    )
  )
  vc <- .safe_vcov(x)

  # rownames(vc) <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", rownames(vc))
  # colnames(vc) <- rownames(vc)

  keep <- match(find_parameters(x, flatten = TRUE, component = component), rownames(vc))
  vc <- vc[keep, keep, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.truncreg <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "all"))
  vc <- .safe_vcov(x)

  if (component == "conditional") {
    vc <- vc[1:(nrow(vc) - 1), 1:(ncol(vc) - 1), drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.gamlss <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "all"))
  vc <- .safe_vcov(x)

  if (component == "conditional") {
    cond_pars <- length(find_parameters(x)$conditional)
    vc <- as.matrix(vc)[1:cond_pars, 1:cond_pars, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


# Zero-Inflated models ----------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.hurdle <- function(x,
                              component = "conditional",
                              vcov = NULL,
                              vcov_args = NULL,
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)
  # process vcov-argument
  vcov <- .check_vcov_args(x, vcov = vcov, verbose = verbose, ...)

  component <- validate_argument(component, c("conditional", "zero_inflated", "zi", "all"))

  if (is.null(vcov)) {
    vc <- switch(component,
      conditional = stats::vcov(object = x, model = "count"),
      zi = ,
      zero_inflated = stats::vcov(object = x, model = "zero"),
      stats::vcov(object = x)
    )
  } else {
    vc <- .get_varcov_sandwich(x,
      vcov_fun = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )
    keep <- switch(component,
      conditional = startsWith(colnames(vc), "count_"),
      zi = ,
      zero_inflated = startsWith(colnames(vc), "zero_"),
      seq_len(ncol(vc))
    )
    vc <- vc[keep, keep, drop = FALSE]
  }

  .process_vcov(vc, verbose, ...)
}

#' @export
get_varcov.zeroinfl <- get_varcov.hurdle

#' @export
get_varcov.zerocount <- get_varcov.hurdle

#' @export
get_varcov.zcpglm <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "zero_inflated", "zi", "all"))

  check_if_installed("cplm")

  vc <- cplm::vcov(x)
  tweedie <- which(startsWith(rownames(vc), "tw_"))
  zero <- which(startsWith(rownames(vc), "zero_"))

  vc <- switch(component,
    conditional = vc[tweedie, tweedie, drop = FALSE],
    zi = ,
    zero_inflated = vc[zero, zero, drop = FALSE],
    vc[c(tweedie, zero), c(tweedie, zero), drop = FALSE]
  )
  .process_vcov(vc, verbose, ...)
}


# Zero-Inflated mixed models ------------------------------------------------


#' @export
get_varcov.glmmTMB <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(
    component,
    c("conditional", "zero_inflated", "zi", "dispersion", "all")
  )

  vc <- switch(component,
    conditional = .safe_vcov(x)[["cond"]],
    zi = ,
    zero_inflated = .safe_vcov(x)[["zi"]],
    dispersion = .safe_vcov(x)[["disp"]],
    stats::vcov(x, full = TRUE)
  )
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.MixMod <- function(x,
                              effects = "fixed",
                              component = "conditional",
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)

  # backward compatibility. there used to be a `robust` argument in this
  # method, but we have now moved to `vcov` and `vcov_args`. Also, `robust`
  # does not seem to be a documented argument for the `MixMod` class.
  robust <- isTRUE(list(...)[["robust"]])

  component <- validate_argument(
    component,
    c("conditional", "zero_inflated", "zi", "dispersion", "auxiliary", "all")
  )
  effects <- validate_argument(effects, c("fixed", "random"))

  random_vc <- stats::vcov(x, parm = "var-cov", sandwich = robust)

  if (effects == "random") {
    vc <- random_vc
  } else {
    vc <- switch(component,
      conditional = stats::vcov(x, parm = "fixed-effects", sandwich = robust),
      zero_inflated = ,
      zi = stats::vcov(x, parm = "all", sandwich = robust),
      auxiliary = ,
      dispersion = stats::vcov(x, parm = "extra", sandwich = robust),
      stats::vcov(x, parm = "all", sandwich = robust)
    )

    # drop random parameters
    m <- match(colnames(random_vc), colnames(vc))
    random_parms <- m[!is.na(m)]
    if (length(random_parms)) {
      vc <- vc[-random_parms, -random_parms, drop = FALSE]
    }

    # filter ZI
    if (component %in% c("zi", "zero_inflated")) {
      zi_parms <- startsWith(colnames(vc), "zi_")
      vc <- vc[zi_parms, zi_parms, drop = FALSE]
    }
  }

  .process_vcov(vc, verbose, ...)
}


# Bayesian models ------------------------------------------------


#' @export
get_varcov.brmsfit <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("all", .all_elements()))
  params <- find_parameters(x, effects = "fixed", component = component, flatten = TRUE)
  params <- gsub("^b_", "", params)

  # get full varcov
  vc <- .safe_vcov(x)

  # check if we can filter - for models with monotonic effects, we have
  # different parameter names, this filtering will fail here
  if (all(params %in% colnames(vc))) {
    vc <- vc[params, params, drop = FALSE]
  }

  .process_vcov(vc, verbose, ...)
}


# mfx models -------------------------------------------------------


#' @export
get_varcov.betamfx <- function(x,
                               component = "conditional",
                               verbose = TRUE,
                               ...) {
  .check_get_varcov_dots(x, ...)
  component <- validate_argument(component, c("conditional", "precision", "all"))
  get_varcov.betareg(x$fit, component = component, verbose = verbose, ...)
}

#' @export
get_varcov.betaor <- get_varcov.betamfx

#' @export
get_varcov.logitmfx <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  get_varcov(x$fit, verbose = verbose, ...)
}

#' @export
get_varcov.poissonmfx <- get_varcov.logitmfx

#' @export
get_varcov.negbinmfx <- get_varcov.logitmfx

#' @export
get_varcov.probitmfx <- get_varcov.logitmfx

#' @export
get_varcov.logitor <- get_varcov.logitmfx

#' @export
get_varcov.poissonirr <- get_varcov.logitmfx

#' @export
get_varcov.negbinirr <- get_varcov.logitmfx

#' @export
get_varcov.model_fit <- get_varcov.logitmfx


# Other models with special handling -----------------------------------------


#' @export
get_varcov.flic <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  .process_vcov(x$var, verbose, ...)
}

#' @export
get_varcov.flac <- get_varcov.flic


#' @export
get_varcov.merModList <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  format_warning("Can't access variance-covariance matrix for 'merModList' objects.")
  NULL
}


#' @export
get_varcov.mediate <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  format_warning("Can't access variance-covariance matrix for 'mediate' objects.")
  NULL
}


#' @rdname get_varcov
#' @export
get_varcov.aov <- function(x, complete = FALSE, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- suppressWarnings(stats::vcov(x, complete = complete))
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.ivFixed <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  .process_vcov(x$vcov, verbose = verbose, ...)
}


#' @export
get_varcov.averaging <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  if (is.null(attributes(x)$modelList)) {
    format_warning("Can't calculate covariance matrix. Please use 'fit = TRUE' in 'model.avg()'.")
  } else {
    get_varcov.default(x, verbose = verbose, ...)
  }
}


#' @export
get_varcov.robmixglm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  params <- find_parameters(x, flatten = TRUE)
  np <- length(params)
  vc <- x$fit@vcov[1:np, 1:np, drop = FALSE]

  dimnames(vc) <- list(params, params)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.bife <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  params <- find_parameters(x, flatten = TRUE)
  np <- length(params)
  vc <- .safe_vcov(x)[1:np, 1:np, drop = FALSE]

  dimnames(vc) <- list(params, params)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.Rchoice <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- .safe_vcov(x)
  params <- find_parameters(x, flatten = TRUE)
  dimnames(vc) <- list(params, params)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.rq <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  s <- summary(x, covariance = TRUE)
  vc <- as.matrix(s$cov)

  # add row/column names
  preds <- find_parameters(x, flatten = TRUE)
  rownames(vc) <- preds
  colnames(vc) <- preds

  if (.is_negativ_matrix(vc, ...)) {
    vc <- .fix_negative_matrix(vc)
  }

  text_remove_backticks(as.matrix(vc))
}


#' @export
get_varcov.crr <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- as.matrix(x$var)
  params <- find_parameters(x, flatten = TRUE)
  dimnames(vc) <- list(params, params)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.crq <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  sc <- summary(x, covariance = TRUE)
  preds <- find_parameters(x, flatten = TRUE)

  if (all(unlist(lapply(sc, is.list)))) {
    vc <- lapply(sc, function(i) {
      .x <- as.matrix(i$cov)

      # add row/column names
      rownames(.x) <- preds
      colnames(.x) <- preds

      if (.is_negativ_matrix(.x, ...)) {
        .x <- .fix_negative_matrix(.x)
      }
      .x
    })
    names(vc) <- sprintf("tau (%g)", unlist(lapply(sc, function(i) i$tau)))
  } else {
    vc <- as.matrix(sc$cov)
    if (.is_negativ_matrix(vc, ...)) {
      vc <- .fix_negative_matrix(vc)
    }
    vc <- text_remove_backticks(as.matrix(vc))
  }

  vc
}

#' @export
get_varcov.nlrq <- get_varcov.crq

#' @export
get_varcov.rqs <- get_varcov.crq


#' @export
get_varcov.flexsurvreg <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  pars <- find_parameters(x, flatten = TRUE)
  vc <- as.matrix(.safe_vcov(x))[pars, pars, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.afex_aov <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  get_varcov(x$lm, verbose = verbose, ...)
}


#' @export
get_varcov.mixed <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- as.matrix(stats::vcov(x$full_model))
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.cpglmm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- as.matrix(x@vcov)
  .process_vcov(vc, verbose, ...)
}

#' @export
get_varcov.cpglm <- get_varcov.cpglmm


#' @export
get_varcov.cglm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- as.matrix(x$var)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.mle2 <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- as.matrix(x@vcov)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.mle <- get_varcov.mle2


#' @rdname get_varcov
#' @export
get_varcov.mixor <- function(x, effects = "all", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  params <- find_parameters(x, effects = effects, flatten = TRUE)
  vc <- as.matrix(.safe_vcov(x))[params, params, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.glmm <- get_varcov.mixor


#' @export
get_varcov.gamm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- stats::vcov(x$gam)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.lqmm <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  sc <- summary(x, covariance = TRUE)
  np <- length(find_parameters(x, flatten = TRUE))

  if (length(dim(sc$Cov)) == 3) {
    vc <- lapply(seq_along(x$tau), function(i) {
      .x <- sc$Cov[, , i][1:np, 1:np]
      if (.is_negativ_matrix(.x, ...)) {
        .x <- .fix_negative_matrix(.x)
      }
      .x
    })
    names(vc) <- sprintf("tau (%g)", x$tau)
  } else {
    vc <- as.matrix(sc$Cov)[1:np, 1:np]
    if (.is_negativ_matrix(vc, ...)) {
      vc <- .fix_negative_matrix(vc)
    }
    vc <- text_remove_backticks(as.matrix(vc))
  }

  vc
}

#' @export
get_varcov.lqm <- get_varcov.lqmm


#' @export
get_varcov.list <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  if ("gam" %in% names(x)) {
    vc <- stats::vcov(x$gam)
    .process_vcov(vc, verbose, ...)
  }
}


#' @export
get_varcov.BBmm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$fixed.vcov
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.BBreg <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$vcov
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.feis <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$vcov
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.glimML <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)

  check_if_installed("aod")

  vc <- aod::vcov(x)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.vglm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)

  check_if_installed("VGAM")
  vc <- VGAM::vcov(x)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.vgam <- get_varcov.vglm


#' @export
get_varcov.tobit <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  coef_names <- find_parameters(x, flatten = TRUE)
  vc <- .safe_vcov(x)[coef_names, coef_names, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.lmRob <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$cov
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.glmRob <- get_varcov.lmRob


#' @export
get_varcov.coxr <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$var
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.hglm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$vcov

  if (is.null(vc)) {
    if (verbose) {
      format_warning("Can't extract variance-covariance matrix.")
    }
    return(NULL)
  }

  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.gee <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$naive.variance
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.LORgee <- get_varcov.gee


# helper-functions -----------------------------------------------------


.safe_vcov <- function(x) {
  vc <- tryCatch(
    suppressWarnings(stats::vcov(x)),
    error = function(e) e
  )
  if (inherits(vc, "error")) {
    # check for dates or times, which can cause the error
    my_data <- get_data(x, verbose = FALSE)
    if (!is.null(my_data) && any(vapply(my_data, inherits, FUN.VALUE = logical(1), what = c("Date", "POSIXt", "difftime")))) { # nolint
      msg <- "A reason might be that your model includes dates or times. Please convert them to numeric values before fitting the model." # nolint
    } else {
      msg <- NULL
    }
    format_error(paste(
      "Can't extract variance-covariance matrix. `get_varcov()` returned following error:",
      vc$message
    ), msg)
  }
  vc
}

.process_vcov <- function(vc, verbose = TRUE, ...) {
  if (.is_negativ_matrix(vc, ...)) {
    vc <- .fix_negative_matrix(vc)
  }
  # fix possible missings due to rank deficient model matrix
  vc <- .fix_rank_deficiency(vc, verbose)
  text_remove_backticks(as.matrix(vc))
}


.is_negativ_matrix <- function(x, pd_tolerance = 1e-08, ...) {
  # allow "NULL" to skip this test
  if (is.null(pd_tolerance)) {
    return(FALSE)
  }
  if (is.matrix(x) && (nrow(x) == ncol(x))) {
    rv <- .safe(
      {
        eigenvalues <- eigen(x, only.values = TRUE)$values
        eigenvalues[abs(eigenvalues) < pd_tolerance] <- 0
        any(eigenvalues < 0)
      },
      FALSE
    )
  } else {
    rv <- FALSE
  }

  rv
}


.fix_negative_matrix <- function(m) {
  if (requireNamespace("Matrix", quietly = TRUE)) {
    format_alert(
      "The variance-covariance matrix is not positive definite. Returning the nearest positive definite matrix now.",
      "This ensures that eigenvalues are all positive real numbers, and thereby, for instance, it is possible to calculate standard errors for all relevant parameters." # nolint
    )
    as.matrix(Matrix::nearPD(m)$mat)
  } else {
    m
  }
}


.fix_rank_deficiency <- function(m, verbose = TRUE) {
  if (anyNA(m)) {
    if (isTRUE(verbose)) {
      format_warning("Model matrix is rank deficient. Some variance-covariance parameters are missing.")
    }
    mm <- m[!is.na(m)]
    if (!is.matrix(mm)) {
      mm <- matrix(mm, nrow = sqrt(length(mm)))
      na_cols <- apply(m, 2, function(i) all(is.na(i)))
      rownames(mm) <- rownames(m)[!na_cols]
      colnames(mm) <- rownames(m)[!na_cols]
      attr(mm, "na_columns_name") <- na_cols[na_cols]
      attr(mm, "na_columns_index") <- which(na_cols)
      attr(mm, "rank_deficient") <- TRUE
    }
    m <- mm
  }
  m
}


.get_weighted_varcov <- function(x, cov_unscaled) {
  ssd <- .weighted_crossprod(stats::residuals(x), w = x$weights)
  weight_df <- sum(x$weights)
  out <- structure(list(SSD = ssd, call = x$call, df = weight_df), class = "SSD")
  kronecker(stats::estVar(out), cov_unscaled, make.dimnames = TRUE)
}


.weighted_crossprod <- function(x, w) {
  if (is.vector(x)) {
    x <- as.matrix(x)
  }

  if (missing(w)) {
    return(crossprod(x))
  }

  if (length(w) == 1 || (is.vector(w) && stats::sd(w) < sqrt(.Machine$double.eps))) {
    w[1] * crossprod(x)
  } else if (is.vector(w)) {
    if (length(w) != nrow(x)) {
      format_error("`w` is the wrong length.")
    }
    crossprod(x, w * x)
  } else {
    if (nrow(w) != ncol(w) || nrow(w) != nrow(x)) {
      format_error("`w` is the wrong dimension.")
    }
    crossprod(x, w %*% x)
  }
}

.check_get_varcov_dots <- function(x, ...) {
  dots <- list(...)
  # if `vcov` is in ..., it means that the argument is not
  # explicitly supported by a method, and thus that it will
  # not be passed here through ... and will not trigger a
  # warning here.
  if ("vcov" %in% names(dots) && !is.null(dots[["vcov"]])) {
    format_warning(
      sprintf("The `vcov` argument of the `insight::get_varcov()` function is not yet supported for models of class `%s`.", paste(class(x), collapse = "/")) # nolint
    )
  }
  if ("robust" %in% names(dots) && !is.null(dots[["robust"]])) {
    format_warning("The `robust` argument is no longer supported. Please use the `vcov` and `vcov_args` instead.") # nolint
  }
}


.check_vcov_args <- function(x, vcov, verbose = TRUE, ...) {
  dots <- list(...)

  # backward compatibility for `get_predicted_se()`
  if (is.null(vcov) && "vcov_estimation" %in% names(dots)) {
    vcov <- dots[["vcov_estimation"]]
  }
  vcov
}
