#' @title Get variance-covariance matrix from models
#' @name get_varcov
#'
#' @description
#'
#' Returns the variance-covariance, as retrieved by `stats::vcov()`, but works
#' for more model objects that probably don't provide a `vcov()`-method.
#'
#'
#' @param x A model.
#' @param component Should the complete variance-covariance matrix of the model
#'   be returned, or only for specific model components only (like count or
#'   zero-inflated model parts)? Applies to models with zero-inflated component,
#'   or models with precision (e.g. `betareg`) component. `component` may be one
#'   of `"conditional"`, `"zi"`, `"zero-inflated"`, `"dispersion"`,
#'   `"precision"`, or `"all"`. May be abbreviated. Note that the *conditional*
#'   component is also called *count* or *mean* component, depending on the
#'   model.
#' @param effects Should the complete variance-covariance matrix of the model
#'   be returned, or only for specific model parameters only? Currently only
#'   applies to models of class `mixor`.
#' @param complete Logical, if `TRUE`, for `aov`, returns the full
#'   variance-covariance matrix.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates (e.g., for robust standard errors). This argument accepts a covariance matrix, a function which returns a covariance matrix, or a string which identifies the function to be used to compute the covariance matrix.
#'  * A covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"vcovHC"`, `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`, `"CR2"`, `"CR3"`. See `?clubSandwich::vcovCR()`
#'    - Bootstrap: `"vcovBS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`, `"webb"`. See `?sandwich::vcovBS`
#'    - Other `sandwich` package functions: `"vcovHAC"`, `"vcovPC"`, `"vcovCL"`, `"vcovPL"`.
#' @param vcov_args List of arguments to be passed to the function identified by
#'   the `vcov` argument. This function is typically supplied by the **sandwich**
#'   or **clubSandwich** packages. Please refer to their documentation (e.g.,
#'   `?sandwich::vcovHAC`) to see the list of available arguments.

#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @note `get_varcov()` tries to return the nearest positive definite matrix
#'   in case of negative eigenvalues of the variance-covariance matrix. This
#'   ensures that it is still possible, for instance, to calculate standard
#'   errors of model parameters. A message is shown when the matrix is negative
#'   definite and a corrected matrix is returned.
#'
#' @return The variance-covariance matrix, as `matrix`-object.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_varcov(m)
#'
#' # vcov of zero-inflation component from hurdle-model
#' if (require("pscl")) {
#'   data("bioChemists", package = "pscl")
#'   mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
#'   get_varcov(mod, component = "zero_inflated")
#' }
#'
#' # robust vcov of, count component from hurdle-model
#' if (require("pscl") && require("sandwich")) {
#'   data("bioChemists", package = "pscl")
#'   mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
#'   get_varcov(
#'     mod,
#'     component = "conditional",
#'     vcov = "BS",
#'     vcov_args = list(R = 50)
#'   )
#' }
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
    vc <- suppressWarnings(stats::vcov(x))
  } else {
    vc <- .get_varcov_sandwich(x,
                               vcov_fun = vcov,
                               vcov_args = vcov_args,
                               verbose = FALSE,
                               ...)
  }
  .process_vcov(vc, verbose, ...)
}

#' @export
get_varcov.maxLik <- get_varcov.default

#' @export
get_varcov.HLfit <- get_varcov.default

#' @export
get_varcov.geeglm <- get_varcov.default



# mlm ---------------------------------------------

#' @export
get_varcov.mlm <- function(x,
                           vcov = NULL,
                           vcov_args = NULL,
                           ...) {
  .check_get_varcov_dots(x, ...)
  if (!is.null(x$weights)) {
    if (!is.null(vcov)) {
      stop(format_message("The `vcov` argument is not supported with",
                          "weights in a `mlm` model."), call. = FALSE)
    }
    s <- summary(x)[[1L]]
    .get_weighted_varcov(x, s$cov.unscaled)
  } else {
    get_varcov.default(x, vcov = vcov, vcov_args = vcov_args, ...)
  }
}



# models with special components ---------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.betareg <- function(x,
                               component = c("conditional", "precision", "all"),
                               verbose = TRUE,
                               ...) {

  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)

  vc <- switch(component,
    "conditional" = stats::vcov(object = x, model = "mean"),
    "precision" = stats::vcov(object = x, model = "precision"),
    stats::vcov(object = x)
  )
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.DirichletRegModel <- function(x,
                                         component = c("conditional", "precision", "all"),
                                         verbose = TRUE,
                                         ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  if (x$parametrization == "common") {
    vc <- stats::vcov(x)
  } else {
    if (component == "conditional") {
      vc <- stats::vcov(x)
      keep <- grepl("^(?!\\(phi\\))", rownames(vc), perl = TRUE)
      vc <- vc[keep, keep, drop = FALSE]
    } else if (component == "precision") {
      vc <- stats::vcov(x)
      keep <- grepl("^\\(phi\\)", rownames(vc), perl = TRUE)
      vc <- vc[keep, keep, drop = FALSE]
    } else {
      vc <- stats::vcov(x)
    }
  }
  .process_vcov(vc, verbose, ...)
}


#' @rdname get_varcov
#' @export
get_varcov.clm2 <- function(x,
                            component = c("all", "conditional", "scale"),
                            ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)

  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  vc <- stats::vcov(x)

  if (.is_negativ_matrix(vc, ...)) {
    vc <- .fix_negative_matrix(vc)
  }

  range <- switch(component,
    "all" = 1:(n_scale + n_intercepts + n_location),
    "conditional" = 1:(n_intercepts + n_location),
    "scale" = (1 + n_intercepts + n_location):(n_scale + n_intercepts + n_location)
  )

  vc <- vc[range, range, drop = FALSE]

  # fix possible missings due to rank deficient model matrix
  vc <- .fix_rank_deficiency(vc)

  text_remove_backticks(as.matrix(vc))
}

#' @export
get_varcov.clmm2 <- get_varcov.clm2


#' @export
get_varcov.glmx <- function(x,
                            component = c("all", "conditional", "extra"),
                            verbose = TRUE,
                            ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(object = x)

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.pgmm <- function(x, component = c("conditional", "all"), verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(x)

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.selection <- function(x,
                                 component = c("all", "selection", "outcome", "auxiliary"),
                                 ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
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
                             component = c("all", "conditional", "thresholds", "correlation"),
                             verbose = TRUE,
                             ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(x)

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
get_varcov.mjoint <- function(x,
                              component = c("all", "conditional", "survival"),
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(x)

  keep <- match(find_parameters(x, flatten = TRUE, component = component), rownames(vc))
  vc <- vc[keep, keep, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.mhurdle <- function(x,
                               component = c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary"),
                               verbose = TRUE,
                               ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(x)

  # rownames(vc) <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", rownames(vc))
  # colnames(vc) <- rownames(vc)

  keep <- match(find_parameters(x, flatten = TRUE, component = component), rownames(vc))
  vc <- vc[keep, keep, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


#' @rdname get_varcov
#' @export
get_varcov.truncreg <- function(x, component = c("conditional", "all"), verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- stats::vcov(x)

  if (component == "conditional") {
    vc <- vc[1:(nrow(vc) - 1), 1:(ncol(vc) - 1), drop = FALSE]
  }
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.gamlss <- function(x, component = c("conditional", "all"), verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
  vc <- suppressWarnings(stats::vcov(x))

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
                              component = c("conditional", "zero_inflated", "zi", "all"),
                              vcov = NULL,
                              vcov_args = NULL,
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)
  # process vcov-argument
  vcov <- .check_vcov_args(x, vcov = vcov, verbose = verbose, ...)

  component <- match.arg(component)

  if (is.null(vcov)) {
    vc <- switch(component,
      "conditional" = stats::vcov(object = x, model = "count"),
      "zi" = ,
      "zero_inflated" = stats::vcov(object = x, model = "zero"),
      stats::vcov(object = x)
    )
  } else {
    vc <- .get_varcov_sandwich(x,
                               vcov_fun = vcov,
                               vcov_args = vcov_args,
                               verbose = verbose,
                               ...)
    keep <- switch(component,
      "conditional" = grepl("^count_", colnames(vc)),
      "zi" = ,
      "zero_inflated" = grepl("^zero_", colnames(vc)),
      1:ncol(vc)
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
get_varcov.zcpglm <- function(x,
                              component = c("conditional", "zero_inflated", "zi", "all"),
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)

  # installed?
  check_if_installed("cplm")

  vc <- cplm::vcov(x)
  tweedie <- which(grepl("^tw_", rownames(vc)))
  zero <- which(grepl("^zero_", rownames(vc)))

  vc <- switch(component,
    "conditional" = vc[tweedie, tweedie, drop = FALSE],
    "zi" = ,
    "zero_inflated" = vc[zero, zero, drop = FALSE],
    vc[c(tweedie, zero), c(tweedie, zero), drop = FALSE]
  )
  .process_vcov(vc, verbose, ...)
}



# Zero-Inflated mixed models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.glmmTMB <- function(x,
                               component = c("conditional", "zero_inflated", "zi", "dispersion", "all"),
                               verbose = TRUE,
                               ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)

  vc <- switch(component,
    "conditional" = stats::vcov(x)[["cond"]],
    "zi" = ,
    "zero_inflated" = stats::vcov(x)[["zi"]],
    "dispersion" = stats::vcov(x)[["disp"]],
    stats::vcov(x, full = TRUE)
  )
  .process_vcov(vc, verbose, ...)
}


#' @rdname get_varcov
#' @export
get_varcov.MixMod <- function(x,
                              effects = c("fixed", "random"),
                              component = c("conditional", "zero_inflated", "zi", "dispersion", "auxiliary", "all"),
                              verbose = TRUE,
                              ...) {
  .check_get_varcov_dots(x, ...)

  # backward compatibility. there used to be a `robust` argument in this
  # method, but we have now moved to `vcov` and `vcov_args`. Also, `robust`
  # does not seem to be a documented argument for the `MixMod` class.
  robust <- isTRUE(list(...)[["robust"]])

  component <- match.arg(component)
  effects <- match.arg(effects)

  random_vc <- stats::vcov(x, parm = "var-cov", sandwich = robust)

  if (effects == "random") {
    vc <- random_vc
  } else {
    vc <- switch(component,
      "conditional" = stats::vcov(x, parm = "fixed-effects", sandwich = robust),
      "zero_inflated" = ,
      "zi" = stats::vcov(x, parm = "all", sandwich = robust),
      "auxiliary" = ,
      "dispersion" = stats::vcov(x, parm = "extra", sandwich = robust),
      stats::vcov(x, parm = "all", sandwich = robust)
    )

    # drop random parameters
    random_parms <- stats::na.omit(match(colnames(random_vc), colnames(vc)))
    if (length(random_parms)) {
      vc <- vc[-random_parms, -random_parms, drop = FALSE]
    }

    # filter ZI
    if (component %in% c("zi", "zero_inflated")) {
      zi_parms <- grepl("^zi_", colnames(vc))
      vc <- vc[zi_parms, zi_parms, drop = FALSE]
    }
  }

  .process_vcov(vc, verbose, ...)
}




# Bayesian models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.brmsfit <- function(x, component = "conditional", verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component, choices = c("all", .all_elements()))
  params <- find_parameters(x, effects = "fixed", component = component, flatten = TRUE)
  params <- gsub("^b_", "", params)

  vc <- stats::vcov(x)[params, params, drop = FALSE]
  .process_vcov(vc, verbose, ...)
}


# mfx models -------------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.betamfx <- function(x,
                               component = c("conditional", "precision", "all"),
                               verbose = TRUE,
                               ...) {
  .check_get_varcov_dots(x, ...)
  component <- match.arg(component)
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
get_varcov.merModList <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  warning("Can't access variance-covariance matrix for 'merModList' objects.", call. = FALSE)
  return(NULL)
}


#' @export
get_varcov.mediate <- function(x, ...) {
  .check_get_varcov_dots(x, ...)
  warning("Can't access variance-covariance matrix for 'mediate' objects.", call. = FALSE)
  return(NULL)
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
    warning("Can't calculate covariance matrix. Please use 'fit = TRUE' in 'model.avg()'.", call. = FALSE)
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
  vc <- stats::vcov(x)[1:np, 1:np, drop = FALSE]

  dimnames(vc) <- list(params, params)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.Rchoice <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- stats::vcov(x)
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
  vc <- as.matrix(stats::vcov(x))[pars, pars, drop = FALSE]
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
get_varcov.mixor <- function(x, effects = c("all", "fixed", "random"), verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  effects <- match.arg(effects)
  params <- find_parameters(x, effects = effects, flatten = TRUE)
  vc <- as.matrix(stats::vcov(x))[params, params, drop = FALSE]
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
    vc <- lapply(1:length(x$tau), function(i) {
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
  # installed?
  check_if_installed("aod")

  vc <- aod::vcov(x)
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.vglm <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  # installed?
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
  vc <- stats::vcov(x)[coef_names, coef_names, drop = FALSE]
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
get_varcov.gee <- function(x, verbose = TRUE, ...) {
  .check_get_varcov_dots(x, ...)
  vc <- x$naive.variance
  .process_vcov(vc, verbose, ...)
}


#' @export
get_varcov.LORgee <- get_varcov.gee



# helper-functions -----------------------------------------------------


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
    rv <- tryCatch(
      {
        eigenvalues <- eigen(x, only.values = TRUE)$values
        eigenvalues[abs(eigenvalues) < pd_tolerance] <- 0
        any(eigenvalues < 0)
      },
      error = function(e) {
        FALSE
      }
    )
  } else {
    rv <- FALSE
  }

  rv
}


.fix_negative_matrix <- function(m) {
  if (requireNamespace("Matrix", quietly = TRUE)) {
    message(format_message("The variance-covariance matrix is not positive definite. Returning the nearest positive definite matrix now.",
                           "This ensures that eigenvalues are all positive real numbers, and thereby, for instance, it is possible to calculate standard errors for all relevant parameters."))
    as.matrix(Matrix::nearPD(m)$mat)
  } else {
    m
  }
}


.fix_rank_deficiency <- function(m, verbose = TRUE) {
  if (anyNA(m)) {
    if (isTRUE(verbose)) {
      warning(format_message("Model matrix is rank deficient. Some variance-covariance parameters are missing."), call. = FALSE)
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
  df <- sum(x$weights)
  out <- structure(list(SSD = ssd, call = x$call, df = df), class = "SSD")
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
    return(w[1] * crossprod(x))
  } else {
    if (is.vector(w)) {
      if (length(w) != nrow(x)) {
        stop("w is the wrong length")
      }
      return(crossprod(x, w * x))
    } else {
      if (nrow(w) != ncol(w) || nrow(w) != nrow(x)) {
        stop("w is the wrong dimension")
      }
      return(crossprod(x, w %*% x))
    }
  }
}

.check_get_varcov_dots <- function(x, ...) {
  dots <- list(...)
  # if `vcov` is in ..., it means that the argument is not
  # explicitly supported by a method, and thus that it will
  # not be passed here through ... and will not trigger a
  # warning here.
  if ("vcov" %in% names(dots) && !is.null(dots[["vcov"]])) {
    msg <- sprintf("The `vcov` argument of the `insight::get_varcov()` function is not yet supported for models of class `%s`.", paste(class(x), collapse = "/"))
    warning(format_message(msg), call. = FALSE)
  }
}


.check_vcov_args <- function(x, vcov, verbose = TRUE, ...) {
  dots <- list(...)

  # backward compatibility for `get_predicted_se()`
  if (is.null(vcov) && "vcov_estimation" %in% names(dots)) {
    vcov <- dots[["vcov_estimation"]]
  }

  if ("robust" %in% names(dots)) {
    # default robust covariance
    if (is.null(vcov)) {
      vcov <- "HC3"
    }
    if (isTRUE(verbose)) {
      warning("The `robust` argument is deprecated. Please use `vcov` instead.",
              call. = FALSE)
    }
  }

  vcov
}
