#' @title Get variance-covariance matrix from models
#'
#' @description Returns the variance-covariance, as retrieved by
#'   \code{stats::vcov()}, but works for more model objects that probably
#'   don't provide a \code{vcov()}-method.
#' @name get_varcov
#'
#' @param x A model.
#' @param component Should the complete variance-covariance matrix of the model
#'   be returned, or only for specific model components only (like count or
#'   zero-inflated model parts)? Applies to models with zero-inflated component,
#'   or models with precision (e.g. \code{betareg}) component. \code{component}
#'   may be one of \code{"conditional"}, \code{"zi"}, \code{"zero-inflated"},
#'   \code{"dispersion"}, \code{"precision"}, or \code{"all"}. May be abbreviated.
#'   Note that the \emph{conditional} component is also called \emph{count} or
#'   \emph{mean} component, depending on the model.
#' @param effects Should the complete variance-covariance matrix of the model
#'   be returned, or only for specific model parameters only? Currently only
#'   applies to models of class \code{mixor}.
#' @param complete Logical, if \code{TRUE}, for \code{aov}, returns the full
#'   variance-covariance matrix.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @note \code{get_varcov()} tries to return the nearest positive definite matrix
#'   in case of a negative variance-covariance matrix.
#'
#' @return The variance-covariance matrix, as \code{matrix}-object.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_varcov(m)
#' @importFrom stats vcov
#' @export
get_varcov <- function(x, ...) {
  UseMethod("get_varcov")
}


# Default models ----------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.default <- function(x, verbose = TRUE, ...) {
  vc <- suppressWarnings(stats::vcov(x))
  .process_vcov(vc, verbose)
}


#' @export
get_varcov.maxLik <- get_varcov.default

#' @export
get_varcov.HLfit <- get_varcov.default




# mlm ---------------------------------------------

#' @export
get_varcov.mlm <- function(x, ...) {
  if (!is.null(x$weights)) {
    s <- summary(x)[[1L]]
    .get_weighted_varcov(x, s$cov.unscaled)
  } else {
    get_varcov.default(x)
  }
}



# models with special components ---------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.betareg <- function(x, component = c("conditional", "precision", "all"), verbose = TRUE, ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(object = x, model = "mean"),
    "precision" = stats::vcov(object = x, model = "precision"),
    stats::vcov(object = x)
  )
  .process_vcov(vc, verbose)
}


#' @rdname get_varcov
#' @export
get_varcov.DirichletRegModel <- function(x, component = c("conditional", "precision", "all"), verbose = TRUE, ...) {
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
  .process_vcov(vc, verbose)
}


#' @rdname get_varcov
#' @export
get_varcov.clm2 <- function(x, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  vc <- stats::vcov(x)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  range <- switch(
    component,
    "all" = 1:(n_scale + n_intercepts + n_location),
    "conditional" = 1:(n_intercepts + n_location),
    "scale" = (1 + n_intercepts + n_location):(n_scale + n_intercepts + n_location)
  )

  vc <- vc[range, range, drop = FALSE]

  # fix possible missings due to rank deficient model matrix
  vc <- .fix_rank_deficiency(vc)

  .remove_backticks_from_matrix_names(as.matrix(vc))
}

#' @export
get_varcov.clmm2 <- get_varcov.clm2


#' @export
get_varcov.glmx <- function(x, component = c("all", "conditional", "extra"), ...) {
  component <- match.arg(component)
  vc <- stats::vcov(object = x)

  if (component != "all") {
    keep <- match(find_parameters(x)[[component]], rownames(vc))
    vc <- vc[keep, keep, drop = FALSE]
  }
  .process_vcov(vc)
}


#' @rdname get_varcov
#' @export
get_varcov.truncreg <- function(x, component = c("conditional", "all"), ...) {
  component <- match.arg(component)
  vc <- stats::vcov(x)

  if (component == "conditional") {
    vc <- vc[1:(nrow(vc) - 1), 1:(ncol(vc) - 1), drop = FALSE]
  }
  .process_vcov(vc)
}


#' @rdname get_varcov
#' @export
get_varcov.gamlss <- function(x, component = c("conditional", "all"), ...) {
  component <- match.arg(component)
  vc <- suppressWarnings(stats::vcov(x))

  if (component == "conditional") {
    cond_pars <- length(find_parameters(x)$conditional)
    vc <- as.matrix(vc)[1:cond_pars, 1:cond_pars, drop = FALSE]
  }
  .process_vcov(vc)
}






# Zero-Inflated models ----------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.hurdle <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(object = x, model = "count"),
    "zi" = ,
    "zero_inflated" = stats::vcov(object = x, model = "zero"),
    stats::vcov(object = x)
  )
  .process_vcov(vc)
}

#' @export
get_varcov.zeroinfl <- get_varcov.hurdle

#' @export
get_varcov.zerocount <- get_varcov.hurdle

#' @rdname get_varcov
#' @export
get_varcov.zcpglm <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  vc <- cplm::vcov(x)
  tweedie <- which(grepl("^tw_", rownames(vc)))
  zero <- which(grepl("^zero_", rownames(vc)))

  vc <- switch(
    component,
    "conditional" = vc[tweedie, tweedie, drop = FALSE],
    "zi" = ,
    "zero_inflated" = vc[zero, zero, drop = FALSE],
    vc[c(tweedie, zero), c(tweedie, zero), drop = FALSE]
  )
  .process_vcov(vc)
}







# Zero-Inflated mixed models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.MixMod <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(x, parm = "fixed-effects"),
    "zi" = ,
    "zero_inflated" = stats::vcov(x, parm = "zero_part"),
    stats::vcov(x)
  )
  .process_vcov(vc)
}


#' @rdname get_varcov
#' @export
get_varcov.glmmTMB <- function(x, component = c("conditional", "zero_inflated", "zi", "dispersion", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(x)[["cond"]],
    "zi" = ,
    "zero_inflated" = stats::vcov(x)[["zi"]],
    "dispersion" = stats::vcov(x)[["disp"]],
    stats::vcov(x, full = TRUE)
  )
  .process_vcov(vc)
}






# Bayesian models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.brmsfit <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)
  params <- find_parameters(x, effects = "fixed", component = component, flatten = TRUE)
  params <- gsub("^b_", "", params)

  vc <- stats::vcov(x)[params, params, drop = FALSE]
  .process_vcov(vc)
}






# mfx models -------------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.betamfx <- function(x, component = c("conditional", "precision", "all"), ...) {
  component <- match.arg(component)
  get_varcov.betareg(x$fit, component = component, ...)
}

#' @export
get_varcov.betaor <- get_varcov.betamfx

#' @export
get_varcov.logitmfx <- function(x, ...) {
  get_varcov(x$fit, ...)
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






# Other models with special handling -----------------------------------------


#' @export
get_varcov.merModList <- function(x, ...) {
  warning("Can't access variance-covariance matrix for 'merModList' objects.", call. = FALSE)
  return(NULL)
}


#' @export
get_varcov.mediate <- function(x, ...) {
  warning("Can't access variance-covariance matrix for 'mediate' objects.", call. = FALSE)
  return(NULL)
}


#' @rdname get_varcov
#' @export
get_varcov.aov <- function(x, complete = FALSE, ...) {
  vc <- suppressWarnings(stats::vcov(x, complete = complete))
  .process_vcov(vc)
}


#' @export
get_varcov.ivFixed <- function(x, ...) {
  .process_vcov(x$vcov)
}


#' @export
get_varcov.averaging <- function(x, ...) {
  if (is.null(attributes(x)$modelList)) {
    warning("Can't calculate covariance matrix. Please use 'fit = TRUE' in 'model.avg()'.", call. = FALSE)
  } else {
    get_varcov.default(x)
  }
}


#' @export
get_varcov.robmixglm <- function(x, ...) {
  params <- find_parameters(x, flatten = TRUE)
  np <- length(params)
  vc <- x$fit@vcov[1:np, 1:np, drop = FALSE]

  dimnames(vc) <- list(params, params)
  .process_vcov(vc)
}


#' @export
get_varcov.rq <- function(x, ...) {
  s <- summary(x, covariance = TRUE)
  vc <- as.matrix(s$cov)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  .remove_backticks_from_matrix_names(as.matrix(vc))
}


#' @export
get_varcov.crr <- function(x, ...) {
  vc <- as.matrix(x$var)
  params <- find_parameters(x, flatten = TRUE)
  dimnames(vc) <- list(params, params)
  .process_vcov(vc)
}


#' @export
get_varcov.crq <- function(x, ...) {
  sc <- summary(x, covariance = TRUE)

  if (all(unlist(lapply(sc, is.list)))) {
    vc <- lapply(sc, function(i) {
      .x <- as.matrix(i$cov)
      if (.is_negativ_matrix(.x)) {
        .x <- .fix_negative_matrix(.x)
      }
      .x
    })
    names(vc) <- sprintf("tau (%g)", unlist(lapply(sc, function(i) i$tau)))
  } else {
    vc <- as.matrix(sc$cov)
    if (.is_negativ_matrix(vc)) {
      vc <- .fix_negative_matrix(vc)
    }
    vc <- .remove_backticks_from_matrix_names(as.matrix(vc))
  }

  vc
}

#' @export
get_varcov.nlrq <- get_varcov.rq

#' @export
get_varcov.rqs <- get_varcov.rq


#' @export
get_varcov.flexsurvreg <- function(x, ...) {
  pars <- find_parameters(x, flatten = TRUE)
  vc <- as.matrix(stats::vcov(x))[pars, pars, drop = FALSE]
  .process_vcov(vc)
}


#' @export
get_varcov.afex_aov <- function(x, ...) {
  if ("lm" %in% names(x)) {
    get_varcov(x$lm)
  } else {
    NULL
  }
}


#' @export
get_varcov.mixed <- function(x, ...) {
  vc <- as.matrix(stats::vcov(x$full_model))
  .process_vcov(vc)
}


#' @export
get_varcov.cpglmm <- function(x, ...) {
  vc <- as.matrix(x@vcov)
  .process_vcov(vc)
}

#' @export
get_varcov.cpglm <- get_varcov.cpglmm



#' @export
get_varcov.cglm <- function(x, ...) {
  vc <- as.matrix(x$var)
  .process_vcov(vc)
}



#' @export
get_varcov.mle2 <- function(x, ...) {
  vc <- as.matrix(x@vcov)
  .process_vcov(vc)
}


#' @export
get_varcov.mle <- get_varcov.mle2



#' @rdname get_varcov
#' @export
get_varcov.mixor <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  params <- find_parameters(x, effects = effects, flatten = TRUE)
  vc <- as.matrix(stats::vcov(x))[params, params, drop = FALSE]
  .process_vcov(vc)
}


#' @export
get_varcov.glmm <- get_varcov.mixor


#' @export
get_varcov.gamm <- function(x, ...) {
  vc <- stats::vcov(x$gam)
  .process_vcov(vc)
}


#' @importFrom stats cov
#' @export
get_varcov.lqmm <- function(x, ...) {
  sc <- summary(x, covariance = TRUE)
  np <- length(find_parameters(x, flatten = TRUE))

  if (length(dim(sc$Cov)) == 3) {
    vc <- lapply(1:length(x$tau), function(i) {
      .x <- sc$Cov[, , i][1:np, 1:np]
      if (.is_negativ_matrix(.x)) {
        .x <- .fix_negative_matrix(.x)
      }
      .x
    })
    names(vc) <- sprintf("tau (%g)", x$tau)
  } else {
    vc <- as.matrix(sc$Cov)[1:np, 1:np]
    if (.is_negativ_matrix(vc)) {
      vc <- .fix_negative_matrix(vc)
    }
    vc <- .remove_backticks_from_matrix_names(as.matrix(vc))
  }

  vc
}

#' @export
get_varcov.lqm <- get_varcov.lqmm


#' @export
get_varcov.list <- function(x, ...) {
  if ("gam" %in% names(x)) {
    vc <- stats::vcov(x$gam)
    .process_vcov(vc)
  }
}


#' @export
get_varcov.BBmm <- function(x, ...) {
  vc <- x$fixed.vcov
  .process_vcov(vc)
}


#' @export
get_varcov.BBreg <- function(x, ...) {
  vc <- x$vcov
  .process_vcov(vc)
}


#' @export
get_varcov.feis <- function(x, ...) {
  vc <- x$vcov
  .process_vcov(vc)
}


#' @export
get_varcov.glimML <- function(x, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }
  vc <- aod::vcov(x)
  .process_vcov(vc)
}


#' @export
get_varcov.vglm <- function(x, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' required for this function to work. Please install it.")
  }
  vc <- VGAM::vcov(x)
  .process_vcov(vc)
}


#' @export
get_varcov.vgam <- get_varcov.vglm


#' @export
get_varcov.geeglm <- function(x, ...) {
  vc <- summary(x)$cov.unscaled
  .process_vcov(vc)
}


#' @export
get_varcov.tobit <- function(x, ...) {
  coef_names <- find_parameters(x, flatten = TRUE)
  vc <- stats::vcov(x)[coef_names, coef_names, drop = FALSE]
  .process_vcov(vc)
}


#' @export
get_varcov.lmRob <- function(x, ...) {
  vc <- x$cov
  .process_vcov(vc)
}


#' @export
get_varcov.glmRob <- get_varcov.lmRob



#' @export
get_varcov.coxr <- function(x, ...) {
  vc <- x$var
  .process_vcov(vc)
}


#' @export
get_varcov.gee <- function(x, ...) {
  vc <- x$naive.variance
  .process_vcov(vc)
}


#' @export
get_varcov.LORgee <- get_varcov.gee







# helper-functions -----------------------------------------------------


.process_vcov <- function(vc, verbose = TRUE) {
  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }
  # fix possible missings due to rank deficient model matrix
  vc <- .fix_rank_deficiency(vc, verbose)
  .remove_backticks_from_matrix_names(as.matrix(vc))
}


.is_negativ_matrix <- function(x) {
  if (is.matrix(x) && (nrow(x) == ncol(x))) {
    rv <- tryCatch(
      {
        eigenvalues <- eigen(x, only.values = TRUE)$values
        eigenvalues[abs(eigenvalues) < 1e-07] <- 0
        any(eigenvalues <= 0)
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
    as.matrix(Matrix::nearPD(m)$mat)
  } else {
    m
  }
}


.fix_rank_deficiency <- function(m, verbose = TRUE) {
  if (anyNA(m)) {
    if (isTRUE(verbose)) {
      warning("Model matrix is rank deficient. Some variance-covariance parameters are missing.", call. = FALSE)
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



#' @importFrom stats residuals estVar sd
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
