#' @importFrom stats nobs
#' @keywords internal
.compute_variances <- function(x, component, name_fun = NULL, name_full = NULL, verbose = TRUE, null_model = NULL) {

  ## Code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an
  ## cleaned-up/adapted version of Jon Lefcheck's code from SEMfit

  faminfo <- model_info(x)

  if (!faminfo$is_mixed) {
    stop("Model is not a mixed model.", call. = FALSE)
  }

  if (faminfo$family %in% c("truncated_nbinom1", "truncated_nbinom2", "tweedie")) {
    if (verbose) {
      warning(sprintf("Truncated negative binomial and tweedie families are currently not supported by `%s`.", name_fun), call. = F)
    }
    return(NA)
  }

  # get necessary model information, like fixed and random effects,
  # variance-covariance matrix etc.
  vals <- .get_variance_information(x, faminfo = faminfo, name_fun = name_fun, verbose = verbose)

  # Test for non-zero random effects ((near) singularity)
  if (.is_singular(x, vals) && !(component %in% c("slope", "intercept"))) {
    if (verbose) {
      warning(sprintf("Can't compute %s. Some variance components equal zero.\n  Solution: Respecify random structure!", name_full), call. = F)
    }
    return(NA)
  }

  # initialize return values, if not all components are requested
  var.fixed <- NULL
  var.random <- NULL
  var.residual <- NULL
  var.distribution <- NULL
  var.dispersion <- NULL
  var.intercept <- NULL
  var.slope <- NULL
  cor.slope_intercept <- NULL

  # Get variance of fixed effects: multiply coefs by design matrix
  if (component %in% c("fixed", "all")) {
    var.fixed <- .compute_variance_fixed(vals)
  }

  # Are random slopes present as fixed effects? Warn.
  random.slopes <- .random_slopes(random.effects = vals$re, model = x)

  if (!all(random.slopes %in% names(vals$beta))) {
    if (verbose) {
      warning(sprintf("Random slopes not present as fixed effects. This artificially inflates the conditional %s.\n  Solution: Respecify fixed structure!", name_full), call. = FALSE)
    }
  }

  # Separate observation variance from variance of random effects
  nr <- sapply(vals$re, nrow)
  not.obs.terms <- names(nr[nr != stats::nobs(x)])
  obs.terms <- names(nr[nr == stats::nobs(x)])

  # Variance of random effects
  if (component %in% c("random", "all")) {
    var.random <- .compute_variance_random(not.obs.terms, x = x, vals = vals)
  }

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  if (component %in% c("residual", "distribution", "all")) {
    var.distribution <- .compute_variance_distribution(x, var.cor = vals$vc, faminfo, name = name_full, null_model = null_model, verbose = verbose)
  }

  if (component %in% c("residual", "dispersion", "all")) {
    var.dispersion <- .compute_variance_dispersion(x = x, vals = vals, faminfo = faminfo, obs.terms = obs.terms)
  }

  if (component %in% c("residual", "all")) {
    var.residual <- var.distribution + var.dispersion
  }

  if (component %in% c("intercept", "all")) {
    var.intercept <- .between_subject_variance(vals, x)
  }

  if (component %in% c("slope", "all")) {
    var.slope <- .random_slope_variance(vals, x)
  }

  if (component %in% c("rho01", "all")) {
    cor.slope_intercept <- .random_slope_intercept_corr(vals, x)
  }

  # if we only need residual variance, we can delete those
  # values again...
  if (component == "residual") {
    var.distribution <- NULL
    var.dispersion <- NULL
  }


  compact_list(list(
    "var.fixed" = var.fixed,
    "var.random" = var.random,
    "var.residual" = var.residual,
    "var.distribution" = var.distribution,
    "var.dispersion" = var.dispersion,
    "var.intercept" = var.intercept,
    "var.slope" = var.slope,
    "cor.slope_intercept" = cor.slope_intercept
  ))
}


#' @importFrom stats model.matrix
#' @keywords internal
.get_variance_information <- function(x, faminfo, name_fun = "get_variances", verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "lme") && !requireNamespace("nlme", quietly = TRUE)) {
    stop("Package `nlme` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "rstanarm") && !requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package `rstanarm` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (inherits(x, "stanreg")) {
    vals <- list(
      beta = lme4::fixef(x),
      X = rstanarm::get_x(x),
      vc = lme4::VarCorr(x),
      re = lme4::ranef(x)
    )
  } else if (inherits(x, "MixMod")) {
    vals <- list(
      beta = lme4::fixef(x),
      X = stats::model.matrix(x),
      vc = x$D,
      re = list(lme4::ranef(x))
    )
    names(vals$re) <- x$id_name
  } else if (inherits(x, "lme")) {
    re_names <- find_random(x, split_nested = TRUE, flatten = TRUE)
    comp_x <- as.matrix(cbind(`(Intercept)` = 1, get_predictors(x)))
    rownames(comp_x) <- 1:nrow(comp_x)

    if (.is_nested_lme(x)) {
      vals_vc <- .get_nested_lme_varcorr(x)
      vals_re <- lme4::ranef(x)
    } else {
      vals_vc <- list(nlme::getVarCov(x))
      vals_re <- list(lme4::ranef(x))
    }

    vals <- list(
      beta = lme4::fixef(x),
      X = comp_x,
      vc = vals_vc,
      re = vals_re
    )

    names(vals$re) <- re_names
    names(vals$vc) <- re_names

  } else {
    vals <- list(
      beta = lme4::fixef(x),
      X = lme4::getME(x, "X"),
      vc = lme4::VarCorr(x),
      re = lme4::ranef(x)
    )
  }

  # for glmmTMB, use conditional component of model only,
  # and tell user that zero-inflation is ignored

  if (inherits(x, "glmmTMB")) {
    vals <- lapply(vals, .collapse_cond)
  }

  if (faminfo$is_zeroinf && verbose) {
    warning(sprintf("%s ignores effects of zero-inflation.", name_fun), call. = FALSE)
  }

  if (!is.null(find_formula(x)[["dispersion"]]) && verbose) {
    warning(sprintf("%s ignores effects of dispersion model.", name_fun), call. = FALSE)
  }

  vals
}


#' helper-function, telling user if model is supported or not
#' @keywords internal
.badlink <- function(link, family, verbose = TRUE) {
  if (verbose) {
    warning(sprintf("Model link '%s' is not yet supported for the %s distribution.", link, family), call. = FALSE)
  }
  return(NA)
}


#' glmmTMB returns a list of model information, one for conditional and one for zero-inflated part, so here we "unlist" it
#' @keywords internal
.collapse_cond <- function(x) {
  if (is.list(x) && "cond" %in% names(x)) {
    x[["cond"]]
  } else {
    x
  }
}


#' Get fixed effects variance
#' @importFrom stats var
#' @keywords internal
.compute_variance_fixed <- function(vals) {
  with(vals, stats::var(as.vector(beta %*% t(X))))
}


#' Compute variance associated with a random-effects term (Johnson 2014)
#' @importFrom stats nobs
#' @keywords internal
.compute_variance_random <- function(terms, x, vals) {

  sigma_sum <- function(Sigma) {
    rn <- rownames(Sigma)

    if (!is.null(rn)) {
      valid <- rownames(Sigma) %in% colnames(vals$X)
      if (!all(valid)) {
        rn <- rn[valid]
        Sigma <- Sigma[valid, valid]
      }
    }

    Z <- vals$X[, rn, drop = FALSE]
    Z.m <- Z %*% Sigma
    sum(diag(crossprod(Z.m, Z))) / stats::nobs(x)
  }

  if (inherits(x, "MixMod")) {
    sigma_sum(vals$vc)
  } else {
    sum(sapply(vals$vc[terms], sigma_sum))
  }
}


#' @keywords internal
.compute_variance_distribution <- function(x, var.cor, faminfo, name, null_model, verbose = TRUE) {
  if (inherits(x, "lme"))
    sig <- x$sigma
  else
    sig <- attr(var.cor, "sc")

  if (is.null(sig)) sig <- 1

  if (faminfo$is_linear) {
    dist.variance <- sig^2
  } else {
    if (faminfo$is_binomial) {
      dist.variance <- switch(
        faminfo$link_function,
        logit = pi^2 / 3,
        probit = 1,
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$is_count) {
      dist.variance <- switch(
        faminfo$link_function,
        log = .get_variance_dist(
          x,
          .null_model(x, null_model, verbose = verbose),
          faminfo,
          sig,
          name = name,
          verbose = verbose
        ),
        sqrt = 0.25,
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$family == "beta") {
      dist.variance <- switch(
        faminfo$link_function,
        logit = .get_variance_dist(
          x,
          .null_model(x, null_model, verbose = verbose),
          faminfo,
          sig,
          name = name,
          verbose = verbose
        ),
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    }
  }

  dist.variance
}


#' Get dispersion-specific variance
#' @keywords internal
.compute_variance_dispersion <- function(x, vals, faminfo, obs.terms) {
  if (faminfo$is_linear) {
    0
  } else {
    if (length(obs.terms) == 0) {
      0
    } else {
      .compute_variance_random(obs.terms, x = x, vals = vals)
    }
  }
}


#' Get distributional variance for beta-family
#' @keywords internal
.get_variance_beta <- function(x, mu, phi) {
  if (inherits(x, "MixMod"))
    stats::family(x)$variance(mu)
  else
    mu * (1 - mu) / (1 + phi)
}


#' @importFrom stats family
#' @keywords internal
.get_variance_dist <- function(x, null.fixef, faminfo, sig, name, verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  # lognormal-approcimation of distributional variance,
  # see Nakagawa et al. 2017

  # in general want log(1+var(x)/mu^2)

  mu <- exp(null.fixef)

  if (is.na(mu)) {
    if (verbose) {
      warning("Can't calculate model's distribution-specific variance. Results are not reliable.", call. = F)
    }
    return(0)
  }
  else if (mu < 6) {
    if (verbose) {
      warning(sprintf("mu of %0.1f is too close to zero, estimate of %s may be unreliable.\n", mu, name), call. = FALSE)
    }
  }

  cvsquared <- tryCatch({
    vv <- switch(
      faminfo$family,
      poisson = stats::family(x)$variance(mu),
      `hurdle poisson` = ,
      truncated_poisson = stats::family(x)$variance(sig),
      beta = .get_variance_beta(x, mu, sig),
      `negative binomial` = ,
      genpois = ,
      nbinom1 = ,
      nbinom2 = stats::family(x)$variance(mu, sig),

      if (inherits(x, "merMod")) {
        mu * (1 + mu / lme4::getME(x, "glmer.nb.theta"))
      } else if (inherits(x, "MixMod")) {
        stats::family(x)$variance(mu)
      } else {
        mu * (1 + mu / x$theta)
      }
    )

    vv / mu^2
  },
  error = function(x) {
    if (verbose) {
      warning("Can't calculate model's distribution-specific variance. Results are not reliable.", call. = F)
    }
    0
  }
  )

  log1p(cvsquared)
}


#' @importFrom stats formula reformulate update
#' @keywords internal
.null_model <- function(model, null_model = NULL, verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  if (!is.null(null_model)) {
    return(unname(lme4::fixef(null_model)))
  }

  if (inherits(model, "MixMod")) {
    if (verbose) {
      warning("Please fit an additional null-model and pass it to the `null_model`-argument.", call. = FALSE)
    }
    return(NA)
  }

  ## TODO compute null-model for MixMod?

  # yet another brms fix
  f <- stats::formula(model)

  if (is.list(f) && obj_has_name(f, "formula")) f <- f$formula

  ## https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q4/023013.html
  rterms <- paste0("(", sapply(lme4::findbars(f), deparse, width.cutoff = 500), ")")
  # nullform <- paste(".", deparse(find_formula(m3)$random, width.cutoff = 500))
  nullform <- stats::reformulate(rterms, response = ".")
  null.model <- stats::update(model, nullform)

  ## Get the fixed effects of the null model
  unname(.collapse_cond(lme4::fixef(null.model)))
}


#' @keywords internal
.random_slopes <- function(random.effects = NULL, model = NULL) {
  if (inherits(model, "MixMod")) {
    return(unlist(find_random_slopes(model)))
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to return random slopes for mixed models.", call. = FALSE)
  }

  if (is.null(random.effects)) {
    if (is.null(model)) {
      stop("Either `random.effects` or `model` must be supplied to return random slopes for mixed models.", call. = FALSE)
    }
    random.effects <- lme4::ranef(model)
  }

  # for glmmTMB, just get conditional component
  if (isTRUE(all.equal(names(random.effects), c("cond", "zi")))) {
    random.effects <- random.effects[["cond"]]
  }

  if (is.list(random.effects)) {
    random.slopes <- unique(unlist(lapply(random.effects, function(re) {
      colnames(re)[-1]
    })))
  } else {
    random.slopes <- colnames(random.effects)
  }

  random.slopes <- setdiff(random.slopes, "(Intercept)")

  if (!length(random.slopes))
    NULL
  else
    random.slopes
}


.between_subject_variance <- function(vals, x) {
  # retrieve only intercepts
  if (inherits(x, "MixMod")) {
    vars <- lapply(vals$vc, function(i) i)[1]
  } else {
    vars <- lapply(vals$vc, function(i) i[1])
  }

  # random intercept-variances, i.e.
  # between-subject-variance (tau 00)
  sapply(vars, function(i) i)
}


.random_slope_variance <- function(vals, x) {
  # random slope-variances (tau 11)
  if (inherits(x, "MixMod")) {
    diag(vals$vc)[-1]
  } else if (inherits(x, "lme")) {
    unlist(lapply(vals$vc, function(x) diag(x)[-1]))
  } else {
    unlist(lapply(vals$vc, function(x) diag(x)[-1]))
  }
}


.random_slope_intercept_corr <- function(vals, x) {
  # get slope-intercept-correlations (rho 01)
  if (inherits(x, "lme")) {
    rho01 <- unlist(sapply(vals$vc, function(i) attr(i, "cor_slope_intercept")))
    if (is.null(rho01)) {
      vc <- lme4::VarCorr(x)
      if ("Corr" %in% colnames(vc)) {
        rho01 <- as.vector(suppressWarnings(na.omit(as.numeric(vc[, "Corr"]))))
      }
    }
    rho01
  } else {
    corrs <- lapply(vals$vc, attr, "correlation")
    rho01 <- sapply(corrs, function(i) {
      if (!is.null(i))
        i[-1, 1]
      else
        NULL
    })
    unlist(rho01)
  }
}
