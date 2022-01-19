.compute_variances <- function(x,
                               component,
                               name_fun = NULL,
                               name_full = NULL,
                               verbose = TRUE,
                               tolerance = 1e-5,
                               model_component = "conditional") {

  ## Original code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an cleaned-up/adapted
  ## version of Jon Lefcheck's code from SEMfit

  ## Major revisions and adaption to more complex models and other packages
  ## by Daniel Lüdecke

  faminfo <- model_info(x, verbose = FALSE)

  if (faminfo$family %in% c("truncated_nbinom1")) {
    if (verbose) {
      warning(format_message(sprintf("Truncated negative binomial families are currently not supported by `%s`.", name_fun)), call. = FALSE)
    }
    return(NA)
  }

  # get necessary model information, like fixed and random effects,
  # variance-covariance matrix etc.
  vals <- .get_variance_information(
    x,
    faminfo = faminfo,
    name_fun = name_fun,
    verbose = verbose,
    model_component = model_component
  )

  # Test for non-zero random effects ((near) singularity)
  no_random_variance <- FALSE
  if (.is_singular(x, vals, tolerance = tolerance) && !(component %in% c("slope", "intercept"))) {
    if (verbose) {
      warning(format_message(
        sprintf("Can't compute %s. Some variance components equal zero. Your model may suffer from singularity (see '?lme4::isSingular' and '?performance::check_singularity').", name_full),
        "Solution: Respecify random structure! You may also decrease the 'tolerance' level to enforce the calculation of random effect variances."
      ), call. = FALSE)
    }
    no_random_variance <- TRUE
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
  cor.slopes <- NULL

  # Get variance of fixed effects: multiply coefs by design matrix
  if (component %in% c("fixed", "all")) {
    var.fixed <- .compute_variance_fixed(vals)
  }

  # Are random slopes present as fixed effects? Warn.
  if (!.random_slopes_in_fixed(x) && verbose) {
    warning(format_message(
      sprintf("Random slopes not present as fixed effects. This artificially inflates the conditional %s.", name_full),
      "Solution: Respecify fixed structure!"
    ), call. = FALSE)
  }

  # Separate observation variance from variance of random effects
  nr <- sapply(vals$re, nrow)
  not.obs.terms <- names(nr[nr != n_obs(x)])
  obs.terms <- names(nr[nr == n_obs(x)])

  # Variance of random effects
  if (component %in% c("random", "all") && isFALSE(no_random_variance)) {
    var.random <- .compute_variance_random(not.obs.terms, x = x, vals = vals)
  }

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  if (component %in% c("residual", "distribution", "all")) {
    var.distribution <- .compute_variance_distribution(
      x = x,
      var.cor = vals$vc,
      faminfo,
      name = name_full,
      verbose = verbose
    )
  }

  if (component %in% c("residual", "dispersion", "all")) {
    var.dispersion <- .compute_variance_dispersion(
      x = x,
      vals = vals,
      faminfo = faminfo,
      obs.terms = obs.terms
    )
  }

  if (component %in% c("residual", "all")) {
    var.residual <- var.distribution + var.dispersion
  }

  if (isTRUE(faminfo$is_mixed) || inherits(x, c("wblm", "wbgee"))) {
    if (component %in% c("intercept", "all")) {
      var.intercept <- .between_subject_variance(vals, x)
    }

    if (component %in% c("slope", "all")) {
      var.slope <- .random_slope_variance(vals, x)
    }

    if (component %in% c("rho01", "all")) {
      cor.slope_intercept <- .random_slope_intercept_corr(vals, x)
    }

    if (component %in% c("rho00", "all")) {
      cor.slopes <- .random_slopes_corr(vals, x)
    }
  } else {
    var.intercept <- NULL
    var.slope <- NULL
    cor.slope_intercept <- NULL
    cor.slopes <- NULL
  }



  # if we only need residual variance, we can delete those
  # values again...
  if (component == "residual") {
    var.distribution <- NULL
    var.dispersion <- NULL
  }


  .compact_list(list(
    "var.fixed" = var.fixed,
    "var.random" = var.random,
    "var.residual" = var.residual,
    "var.distribution" = var.distribution,
    "var.dispersion" = var.dispersion,
    "var.intercept" = var.intercept,
    "var.slope" = var.slope,
    "cor.slope_intercept" = cor.slope_intercept,
    "cor.slopes" = cor.slopes
  ))
}





# store essential information on coefficients, model matrix and so on
# as list, since we need these information throughout the functions to
# calculate the variance components...
#
# basically, this function should return a list that has the same
# structure for any mixed models like this code for lme4:
# beta = lme4::fixef(x),
# X = lme4::getME(x, "X"),
# vc = lme4::VarCorr(x),
# re = lme4::ranef(x)
#
.get_variance_information <- function(x,
                                      faminfo,
                                      name_fun = "get_variances",
                                      verbose = TRUE,
                                      model_component = "conditional") {

  # installed?
  check_if_installed("lme4", reason = "to compute variances for mixed models")

  if (inherits(x, "lme")) {
    check_if_installed("nlme", reason = "to compute variances for mixed models")
  }

  if (inherits(x, "rstanarm")) {
    check_if_installed("rstanarm", reason = "to compute variances for mixed models")
  }

  # stanreg
  # ---------------------------
  if (inherits(x, "stanreg")) {
    vals <- list(
      beta = lme4::fixef(x),
      X = rstanarm::get_x(x),
      vc = lme4::VarCorr(x),
      re = lme4::ranef(x)
    )

    # GLMMapdative
    # ---------------------------
  } else if (inherits(x, "MixMod")) {
    vc1 <- vc2 <- NULL
    re_names <- find_random(x)

    vc_cond <- !grepl("^zi_", colnames(x$D))
    if (any(vc_cond)) {
      vc1 <- x$D[vc_cond, vc_cond, drop = FALSE]
      attr(vc1, "stddev") <- sqrt(diag(vc1))
      attr(vc1, "correlation") <- stats::cov2cor(x$D[vc_cond, vc_cond, drop = FALSE])
    }

    vc_zi <- grepl("^zi_", colnames(x$D))
    if (any(vc_zi)) {
      colnames(x$D) <- gsub("^zi_(.*)", "\\1", colnames(x$D))
      rownames(x$D) <- colnames(x$D)
      vc2 <- x$D[vc_zi, vc_zi, drop = FALSE]
      attr(vc2, "stddev") <- sqrt(diag(vc2))
      attr(vc2, "correlation") <- stats::cov2cor(x$D[vc_zi, vc_zi, drop = FALSE])
    }

    vc1 <- list(vc1)
    names(vc1) <- re_names[[1]]
    attr(vc1, "sc") <- sqrt(get_deviance(x, verbose = FALSE) / get_df(x, type = "residual", verbose = FALSE))

    if (!is.null(vc2)) {
      vc2 <- list(vc2)
      names(vc2) <- re_names[[2]]
      attr(vc2, "sc") <- sqrt(get_deviance(x, verbose = FALSE) / get_df(x, type = "residual", verbose = FALSE))
    }

    vcorr <- .compact_list(list(vc1, vc2))
    names(vcorr) <- c("cond", "zi")[1:length(vcorr)]

    vals <- list(
      beta = lme4::fixef(x),
      X = get_modelmatrix(x),
      vc = vcorr,
      re = list(lme4::ranef(x))
    )
    names(vals$re) <- x$id_name

    # joineRML
    # ---------------------------
  } else if (inherits(x, "mjoint")) {
    re_names <- find_random(x, flatten = TRUE)
    vcorr <- summary(x)$D
    attr(vcorr, "stddev") <- sqrt(diag(vcorr))
    attr(vcorr, "correlation") <- stats::cov2cor(vcorr)
    vcorr <- list(vcorr)
    names(vcorr) <- re_names[1]
    attr(vcorr, "sc") <- x$coef$sigma2[[1]]

    vals <- list(
      beta = lme4::fixef(x),
      X = matrix(1, nrow = n_obs(x), dimnames = list(NULL, "(Intercept)_1")),
      vc = vcorr,
      re = list(lme4::ranef(x))
    )
    names(vals$re) <- re_names[1:length(vals$re)]

    # nlme
    # ---------------------------
  } else if (inherits(x, "lme")) {
    re_names <- find_random(x, split_nested = TRUE, flatten = TRUE)
    comp_x <- get_modelmatrix(x)
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

    # ordinal
    # ---------------------------
  } else if (inherits(x, "clmm")) {
    if (requireNamespace("ordinal", quietly = TRUE)) {
      mm <- get_modelmatrix(x)
      vals <- list(
        beta = c("(Intercept)" = 1, stats::coef(x)[intersect(names(stats::coef(x)), colnames(mm))]),
        X = mm,
        vc = ordinal::VarCorr(x),
        re = ordinal::ranef(x)
      )
    }

    # glmmadmb
    # ---------------------------
  } else if (inherits(x, "glmmadmb")) {
    vals <- list(
      beta = lme4::fixef(x),
      X = get_modelmatrix(x),
      vc = lme4::VarCorr(x),
      re = lme4::ranef(x)
    )

    # brms
    # ---------------------------
  } else if (inherits(x, "brmsfit")) {
    comp_x <- get_modelmatrix(x)
    rownames(comp_x) <- 1:nrow(comp_x)
    vc <- lapply(names(lme4::VarCorr(x)), function(i) {
      element <- lme4::VarCorr(x)[[i]]
      if (i != "residual__") {
        if (!is.null(element$cov)) {
          out <- as.matrix(drop(element$cov[, 1, ]))
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$cov), fixed = TRUE)
        } else {
          out <- as.matrix(drop(element$sd[, 1])^2)
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$sd), fixed = TRUE)
        }
        attr(out, "sttdev") <- element$sd[, 1]
      } else {
        out <- NULL
      }
      out
    })
    vc <- .compact_list(vc)
    names(vc) <- setdiff(names(lme4::VarCorr(x)), "residual__")
    attr(vc, "sc") <- lme4::VarCorr(x)$residual__$sd[1, 1]
    vals <- list(
      beta = lme4::fixef(x)[, 1],
      X = comp_x,
      vc = vc,
      re = lapply(lme4::ranef(x), function(re) {
        reval <- as.data.frame(drop(re[, 1, ]))
        colnames(reval) <- gsub("Intercept", "(Intercept)", dimnames(re)[[3]], fixed = TRUE)
        reval
      })
    )
    names(vals$beta) <- gsub("Intercept", "(Intercept)", names(vals$beta), fixed = TRUE)

    # cpglmm
    # ---------------------------
  } else if (inherits(x, "cpglmm")) {
    # installed?
    check_if_installed("cplm")

    vals <- list(
      beta = cplm::fixef(x),
      X = cplm::model.matrix(x),
      vc = cplm::VarCorr(x),
      re = cplm::ranef(x)
    )

    # lme4 / glmmTMB
    # ---------------------------
  } else {
    vals <- list(
      beta = lme4::fixef(x),
      X = lme4::getME(x, "X"),
      vc = lme4::VarCorr(x),
      re = lme4::ranef(x)
    )
  }


  # for glmmTMB, tell user that dispersion model is ignored

  if (inherits(x, c("glmmTMB", "MixMod"))) {
    if (is.null(model_component) || model_component == "conditional") {
      vals <- lapply(vals, .collapse_cond)
    } else {
      vals <- lapply(vals, .collapse_zi)
    }
  }

  if (!is.null(find_formula(x)[["dispersion"]]) && verbose) {
    warning(sprintf("%s ignores effects of dispersion model.", name_fun), call. = FALSE)
  }

  vals
}





# helper-function, telling user if family / distribution
# is supported or not
.badlink <- function(link, family, verbose = TRUE) {
  if (verbose) {
    warning(format_message(sprintf("Model link '%s' is not yet supported for the %s distribution.", link, family)), call. = FALSE)
  }
  return(NA)
}





# glmmTMB returns a list of model information, one for conditional
# and one for zero-inflated part, so here we "unlist" it, returning
# only the conditional part.
.collapse_cond <- function(x) {
  if (is.list(x) && "cond" %in% names(x)) {
    x[["cond"]]
  } else {
    x
  }
}


.collapse_zi <- function(x) {
  if (is.list(x) && "zi" %in% names(x)) {
    x[["zi"]]
  } else {
    x
  }
}





# fixed effects variance ----
# ---------------------------
.compute_variance_fixed <- function(vals) {
  with(vals, stats::var(as.vector(beta %*% t(X))))
}





# variance associated with a random-effects term (Johnson 2014) ----
# ------------------------------------------------------------------
.compute_variance_random <- function(terms, x, vals) {
  if (is.null(terms)) {
    return(NULL)
  }
  .sigma_sum <- function(Sigma) {
    rn <- rownames(Sigma)

    # fix for models w/o intercept
    if (!any(grepl("^\\(Intercept\\)", colnames(vals$X)))) {
      vals$X <- cbind("(Intercept)" = 1, vals$X)
    }

    if (!is.null(rn)) {
      valid <- rownames(Sigma) %in% colnames(vals$X)
      if (!all(valid)) {
        rn <- rn[valid]
        Sigma <- Sigma[valid, valid, drop = FALSE]
      }
    }

    Z <- vals$X[, rn, drop = FALSE]
    Z.m <- Z %*% Sigma
    sum(diag(crossprod(Z.m, Z))) / n_obs(x)
  }

  # if (inherits(x, "MixMod")) {
  #   .sigma_sum(vals$vc)
  # } else {
  #   sum(sapply(vals$vc[terms], .sigma_sum))
  # }
  sum(sapply(vals$vc[terms], .sigma_sum))
}





# distribution-specific variance (Nakagawa et al. 2017) ----
# ----------------------------------------------------------
.compute_variance_distribution <- function(x, var.cor, faminfo, name, verbose = TRUE) {
  if (inherits(x, "lme")) {
    sig <- x$sigma
  } else {
    sig <- attr(var.cor, "sc")
  }

  if (is.null(sig)) sig <- 1

  # Distribution-specific variance depends on the model-family
  # and the related link-function

  if (faminfo$is_linear && !faminfo$is_tweedie) {

    # linear / Gaussian ----
    # ----------------------

    dist.variance <- sig^2
  } else {
    if (faminfo$is_betabinomial) {

      # beta-binomial ----
      # ------------------

      dist.variance <- switch(faminfo$link_function,
        logit = ,
        probit = ,
        cloglog = ,
        clogloglink = .variance_distributional(x, faminfo, sig, name = name, verbose = verbose),
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$is_binomial) {

      # binomial / bernoulli  ----
      # --------------------------

      dist.variance <- switch(faminfo$link_function,
        logit = pi^2 / 3,
        probit = 1,
        cloglog = ,
        clogloglink = pi^2 / 6,
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$is_count) {

      # count  ----
      # -----------

      dist.variance <- switch(faminfo$link_function,
        log = .variance_distributional(x, faminfo, sig, name = name, verbose = verbose),
        sqrt = 0.25,
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$family %in% c("Gamma", "gamma")) {

      # Gamma  ----
      # -----------

      ## TODO needs some more checking - should now be in line with other packages
      dist.variance <- switch(faminfo$link_function,
        inverse = ,
        identity = ,
        log = stats::family(x)$variance(sig),
        # log = .variance_distributional(x, faminfo, sig, name = name, verbose = verbose),
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$family == "beta") {

      # Beta  ----
      # ----------

      dist.variance <- switch(faminfo$link_function,
        logit = .variance_distributional(x, faminfo, sig, name = name, verbose = verbose),
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else if (faminfo$is_tweedie) {

      # Tweedie  ----
      # -------------

      dist.variance <- switch(faminfo$link_function,
        log = .variance_distributional(x, faminfo, sig, name = name, verbose = verbose),
        .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
      )
    } else {
      dist.variance <- sig
    }
  }

  dist.variance
}





# dispersion-specific variance ----
# ---------------------------------
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





# This is the core-function to calculate the distribution-specific variance
# Nakagawa et al. 2017 propose three different methods, here we only rely
# on the lognormal-approximation.
#
.variance_distributional <- function(x, faminfo, sig, name, verbose = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  # lognormal-approximation of distributional variance,
  # see Nakagawa et al. 2017

  # in general want log(1+var(x)/mu^2)
  .null_model <- null_model(x, verbose = verbose)

  # check if null-model could be computed
  if (!is.null(.null_model)) {
    if (inherits(.null_model, "cpglmm")) {
      # installed?
      check_if_installed("cplm")
      null_fixef <- unname(cplm::fixef(.null_model))
    } else {
      null_fixef <- unname(.collapse_cond(lme4::fixef(.null_model)))
    }
    mu <- exp(null_fixef)
  } else {
    mu <- NA
  }

  if (is.na(mu)) {
    if (verbose) {
      warning(format_message("Can't calculate model's distribution-specific variance. Results are not reliable."), call. = FALSE)
    }
    return(0)
  } else if (mu < 6) {
    if (verbose) {
      warning(format_message(sprintf("mu of %0.1f is too close to zero, estimate of %s may be unreliable.", mu, name)), call. = FALSE)
    }
  }

  cvsquared <- tryCatch(
    {
      vv <- switch(faminfo$family,

        # (zero-inflated) poisson ----
        # ----------------------------
        `zero-inflated poisson` = ,
        poisson = .variance_family_poisson(x, mu, faminfo),

        # hurdle-poisson ----
        # -------------------
        `hurdle poisson` = ,
        truncated_poisson = stats::family(x)$variance(sig),

        # Gamma, exponential ----
        # -----------------------
        Gamma = stats::family(x)$variance(sig),

        # (zero-inflated) negative binomial ----
        # --------------------------------------
        `zero-inflated negative binomial` = ,
        `negative binomial` = ,
        genpois = ,
        nbinom1 = ,
        nbinom2 = .variance_family_nbinom(x, mu, sig, faminfo),
        truncated_nbinom2 = stats::family(x)$variance(mu, sig),

        # other distributions ----
        # ------------------------
        tweedie = .variance_family_tweedie(x, mu, sig),
        beta = .variance_family_beta(x, mu, sig),
        # betabinomial = stats::family(x)$variance(mu, sig),
        # betabinomial = .variance_family_betabinom(x, mu, sig),

        # default variance for non-captured distributions ----
        # ----------------------------------------------------
        .variance_family_default(x, mu, verbose)
      )

      if (vv < 0 && isTRUE(verbose)) {
        warning(format_message("Model's distribution-specific variance is negative. Results are not reliable."), call. = FALSE)
      }
      vv / mu^2
    },
    error = function(x) {
      if (verbose) {
        warning(format_message("Can't calculate model's distribution-specific variance. Results are not reliable."), call. = FALSE)
      }
      0
    }
  )

  log1p(cvsquared)
}





# Get distributional variance for poisson-family
# ----------------------------------------------
.variance_family_poisson <- function(x, mu, faminfo) {
  if (faminfo$is_zero_inflated) {
    .variance_zip(x, faminfo, family_var = mu)
  } else {
    if (inherits(x, "MixMod")) {
      return(mu)
    } else if (inherits(x, "cpglmm")) {
      .get_cplm_family(x)$variance(mu)
    } else {
      stats::family(x)$variance(mu)
    }
  }
}





# Get distributional variance for beta-family
# ----------------------------------------------
.variance_family_beta <- function(x, mu, phi) {
  if (inherits(x, "MixMod")) {
    stats::family(x)$variance(mu)
  } else {
    mu * (1 - mu) / (1 + phi)
  }
}





# Get distributional variance for beta-family
# ----------------------------------------------
.variance_family_betabinom <- function(x, mu, phi) {
  if (inherits(x, "MixMod")) {
    stats::family(x)$variance(mu)
  } else {
    n <- n_obs(x)
    mu * (1 - mu) * (n * (phi + n) / (1 + phi))
  }
}





# Get distributional variance for tweedie-family
# ----------------------------------------------
.variance_family_tweedie <- function(x, mu, phi) {
  p <- unname(stats::plogis(x$fit$par["thetaf"]) + 1)
  phi * mu^p
}





# Get distributional variance for nbinom-family
# ----------------------------------------------
.variance_family_nbinom <- function(x, mu, sig, faminfo) {
  if (faminfo$is_zero_inflated) {
    if (missing(sig)) sig <- 0
    .variance_zinb(x, sig, faminfo, family_var = mu * (1 + sig))
  } else {
    if (inherits(x, "MixMod")) {
      if (missing(sig)) {
        return(rep(1e-16, length(mu)))
      }
      mu * (1 + sig)
    } else {
      stats::family(x)$variance(mu, sig)
    }
  }
}





# For zero-inflated negative-binomial models,
# the distributional variance is based on Zuur et al. 2012
# ----------------------------------------------
.variance_zinb <- function(model, sig, faminfo, family_var) {
  if (inherits(model, "glmmTMB")) {
    v <- stats::family(model)$variance
    # zi probability
    p <- stats::predict(model, type = "zprob")
    # mean of conditional distribution
    mu <- stats::predict(model, type = "conditional")
    # sigma
    betad <- model$fit$par["betad"]
    k <- switch(faminfo$family,
      gaussian = exp(0.5 * betad),
      Gamma = exp(-0.5 * betad),
      exp(betad)
    )
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
  } else if (inherits(model, "MixMod")) {
    v <- family_var
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- suppressWarnings(stats::predict(model, type_pred = "link", type = "mean_subject"))
    k <- sig
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
  } else {
    pvar <- family_var
  }

  mean(pvar)

  # pearson residuals
  # pred <- predict(model, type = "response") ## (1 - p) * mu
  # pred <- stats::predict(model, type_pred = "response", type = "mean_subject")
  # (get_response(model) - pred) / sqrt(pvar)
}





# For zero-inflated poisson models, the
# distributional variance is based on Zuur et al. 2012
# ----------------------------------------------
.variance_zip <- function(model, faminfo, family_var) {
  if (inherits(model, "glmmTMB")) {
    p <- stats::predict(model, type = "zprob")
    mu <- stats::predict(model, type = "conditional")
    pvar <- (1 - p) * (mu + p * mu^2)
  } else if (inherits(model, "MixMod")) {
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- suppressWarnings(stats::predict(model, type = "mean_subject"))
    pvar <- (1 - p) * (mu + p * mu^2)
  } else {
    pvar <- family_var
  }

  mean(pvar)
}





# Get distribution-specific variance for general and
# undefined families / link-functions
# ----------------------------------------------
.variance_family_default <- function(x, mu, verbose) {
  # installed?
  check_if_installed("lme4")

  tryCatch(
    {
      if (inherits(x, "merMod")) {
        mu * (1 + mu / lme4::getME(x, "glmer.nb.theta"))
      } else if (inherits(x, "MixMod")) {
        stats::family(x)$variance(mu)
      } else if (inherits(x, "glmmTMB")) {
        if (is.null(x$theta)) {
          theta <- lme4::getME(x, "theta")
        } else {
          theta <- x$theta
        }
        mu * (1 + mu / theta)
      } else {
        mu * (1 + mu / x$theta)
      }
    },
    error = function(x) {
      if (verbose) {
        warning("Can't calculate model's distribution-specific variance. Results are not reliable.", call. = FALSE)
      }
      0
    }
  )
}





# return existence of random slopes ----
# ----------------------------------------------
.random_slopes_in_fixed <- function(model) {
  rs <- find_random_slopes(model)
  fe <- find_predictors(model, effects = "fixed", component = "all")

  # if model has no random slopes, there are no random slopes that
  # are *not* present as fixed effects
  if (is.null(rs)) {
    return(TRUE)
  }

  # NULL models have no predictors, so no fixed effect as random slope
  if (is.null(fe)) {
    return(FALSE)
  }

  # make sure we have identical subcomponents between random and
  # fixed effects
  fe <- .compact_list(fe[c("conditional", "zero_inflated")])
  if (length(rs) > length(fe)) rs <- rs[1:length(fe)]
  if (length(fe) > length(rs)) fe <- fe[1:length(rs)]

  all(mapply(function(r, f) all(r %in% f), rs, fe, SIMPLIFY = TRUE))
}





# random intercept-variances, i.e.
# between-subject-variance (tau 00) ----
# ----------------------------------------------
.between_subject_variance <- function(vals, x) {
  vars <- lapply(vals$vc, function(i) i[1])
  # check for uncorrelated random slopes-intercept
  non_intercepts <- which(sapply(vals$vc, function(i) !grepl("^\\(Intercept\\)", dimnames(i)[[1]][1])))
  if (length(non_intercepts)) {
    vars <- vars[-non_intercepts]
  }

  sapply(vars, function(i) i)
}





# random slope-variances (tau 11) ----
# ----------------------------------------------
.random_slope_variance <- function(vals, x) {
  if (inherits(x, "lme")) {
    unlist(lapply(vals$vc, function(x) diag(x)[-1]))
  } else {
    out <- unlist(lapply(vals$vc, function(x) diag(x)[-1]))
    # check for uncorrelated random slopes-intercept
    non_intercepts <- which(sapply(vals$vc, function(i) !grepl("^\\(Intercept\\)", dimnames(i)[[1]][1])))
    if (length(non_intercepts)) {
      dn <- unlist(lapply(vals$vc, function(i) dimnames(i)[1])[non_intercepts])
      rndslopes <- unlist(lapply(vals$vc, function(i) i[1])[non_intercepts])
      names(rndslopes) <- gsub("(.*)\\.\\d+$", "\\1", names(rndslopes))
      out <- c(out, stats::setNames(rndslopes, paste0(names(rndslopes), ".", dn)))
    }
    out
  }
}





# slope-intercept-correlations (rho 01) ----
# ----------------------------------------------
.random_slope_intercept_corr <- function(vals, x) {
  if (inherits(x, "lme")) {
    rho01 <- unlist(sapply(vals$vc, function(i) attr(i, "cor_slope_intercept")))
    if (is.null(rho01)) {
      vc <- lme4::VarCorr(x)
      if ("Corr" %in% colnames(vc)) {
        re_name <- find_random(x, split_nested = FALSE, flatten = TRUE)
        rho01 <- as.vector(suppressWarnings(stats::na.omit(as.numeric(vc[, "Corr"]))))
        if (length(re_name) == length(rho01)) {
          names(rho01) <- re_name
        }
      }
    }
    rho01
  } else {
    corrs <- lapply(vals$vc, attr, "correlation")
    rho01 <- sapply(corrs, function(i) {
      if (!is.null(i)) {
        i[-1, 1]
      } else {
        NULL
      }
    })
    unlist(rho01)
  }
}





# slope-slope-correlations (rho 01) ----
# ----------------------------------------------
.random_slopes_corr <- function(vals, x) {
  corrs <- lapply(vals$vc, attr, "correlation")
  rnd_slopes <- unlist(find_random_slopes(x))

  if (length(rnd_slopes) < 2) {
    return(NULL)
  }

  rho01 <- tryCatch(
    {
      lapply(corrs, function(d) {
        d[upper.tri(d, diag = TRUE)] <- NA
        d <- as.data.frame(d)

        d <- .reshape_longer(d, colnames_to = "Parameter1", rows_to = "Parameter2")
        d <- d[stats::complete.cases(d), ]
        d <- d[!d$Parameter1 %in% c("Intercept", "(Intercept)"), ]

        d$Parameter <- paste0(d$Parameter1, "-", d$Parameter2)
        d$Parameter1 <- d$Parameter2 <- NULL
        stats::setNames(d$Value, d$Parameter)
      })
    },
    error = function(e) {
      NULL
    }
  )

  # rho01 <- tryCatch(
  #   {
  #     sapply(corrs, function(i) {
  #       if (!is.null(i)) {
  #         slope_pairs <- utils::combn(x = rnd_slopes, m = 2, simplify = FALSE)
  #         lapply(slope_pairs, function(j) {
  #           stats::setNames(i[j[1], j[2]], paste0("..", paste0(j, collapse = "-")))
  #         })
  #       } else {
  #         NULL
  #       }
  #     })
  #   },
  #   error = function(e) {
  #     NULL
  #   }
  # )

  unlist(rho01)
}






# helper --------------------------

.reshape_longer <- function(data,
                            colnames_to = "Name",
                            rows_to = NULL) {
  cols <- names(data)
  values_to <- "Value"

  # save attribute of each variable
  variable_attr <- lapply(data, attributes)

  # Create Index column as needed by reshape
  data[["_Row"]] <- .to_numeric(row.names(data))

  # Reshape
  long <- stats::reshape(data,
    varying = cols,
    idvar = "_Row",
    v.names = values_to,
    timevar = colnames_to,
    direction = "long"
  )

  # Sort the dataframe (to match pivot_longer's output)
  long <- long[order(long[["_Row"]], long[[colnames_to]]), ]

  # Remove or rename the row index
  if (is.null(rows_to)) {
    long[["_Row"]] <- NULL
  } else {
    names(long)[names(long) == "_Row"] <- rows_to
  }

  # Re-insert col names as levels
  long[[colnames_to]] <- cols[long[[colnames_to]]]

  # Reset row names
  row.names(long) <- NULL

  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL

  # add back attributes where possible
  for (i in colnames(long)) {
    attributes(long[[i]]) <- variable_attr[[i]]
  }

  long
}


.to_numeric <- function(x) {
  tryCatch(as.numeric(as.character(x)), error = function(e) x, warning = function(w) x)
}
