.compute_variances <- function(model,
                               component,
                               name_fun = NULL,
                               name_full = NULL,
                               verbose = TRUE,
                               tolerance = 1e-8,
                               model_component = "full",
                               model_null = NULL,
                               approximation = "lognormal") {
  ## Original code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an cleaned-up/adapted
  ## version of Jon Lefcheck's code from SEMfit

  ## Revisions and adaption to more complex models and other packages
  ## by Daniel Lüdecke

  # sanity check - only proceed for mixed models
  if (!is_mixed_model(model)) {
    if (verbose) {
      format_warning("This function only works for mixed models, i.e. models with random effects.")
    }
    return(NULL)
  }

  # needed for singularity check
  check_if_installed("performance", reason = "to check for singularity")

  faminfo <- model_info(model, verbose = FALSE)

  # check argument
  approx_method <- match.arg(approximation, c("lognormal", "delta", "trigamma", "observation_level"))

  # check whether R2 should be calculated for the full model, or the
  # conditional model only
  if (is.null(model_component) || model_component %in% c("zi", "zero_inflated")) {
    model_component <- "full"
  }

  # sanity checks - distribution supported?
  if (any(faminfo$family == "truncated_nbinom1")) {
    if (verbose) {
      format_warning(sprintf(
        "Truncated negative binomial families are currently not supported by `%s`.",
        name_fun
      ))
    }
    return(NA)
  }

  # sanity checks - distribution supported with requested component?
  if ((any(startsWith(faminfo$family, "truncated_")) || any(startsWith(faminfo$family, "hurdle"))) && model_component != "full") { # nolint
    if (verbose) {
      format_warning("Truncated or hurdle families are only supported for `model_component = \"full\"`.")
    }
    return(NA)
  }

  # zero-inflated model, but not conditioning on full model?
  if (!identical(model_component, "full") && (faminfo$is_zero_inflated || faminfo$is_hurdle) && verbose) {
    format_alert(
      "Zero-inflation part of the model is not considered for variance decomposition. Use `model_component = \"full\"` to take both the conditional and the zero-inflation model into account." # nolint
    )
  }

  # rename lme4 neg-binom family
  if (startsWith(faminfo$family, "Negative Binomial")) {
    faminfo$family <- "negative binomial"
  }

  # get necessary model information, like fixed and random effects,
  # variance-covariance matrix etc.
  mixed_effects_info <- .get_variance_information(
    model,
    faminfo = faminfo,
    name_fun = name_fun,
    verbose = verbose
  )

  # we also need necessary model information, like fixed and random effects,
  # variance-covariance matrix etc. for the null model
  if (faminfo$is_linear && !faminfo$is_tweedie) {
    # we don't need these for linear models
    me_info_null <- NULL
  } else {
    if (is.null(model_null)) {
      model_null <- .safe(null_model(model, verbose = FALSE))
    }
    me_info_null <- .get_variance_information(
      model_null,
      faminfo = faminfo,
      name_fun = name_fun,
      verbose = verbose
    )
  }

  # Test for non-zero random effects ((near) singularity)
  no_random_variance <- FALSE
  singular_fit <- isTRUE(.safe(performance::check_singularity(model, tolerance = tolerance)))

  if (singular_fit && !(component %in% c("slope", "intercept"))) {
    if (verbose) {
      format_warning(
        sprintf("Can't compute %s. Some variance components equal zero. Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`).", name_full), # nolint
        "Solution: Respecify random structure! You may also decrease the `tolerance` level to enforce the calculation of random effect variances." # nolint
      )
    }
    no_random_variance <- TRUE
  }

  # initialize return values, if not all components are requested
  var.fixed <- NULL
  var.random <- NULL
  var.random_null <- NULL
  var.residual <- NULL
  var.distribution <- NULL
  var.dispersion <- NULL
  var.intercept <- NULL
  var.slope <- NULL
  cor.slope_intercept <- NULL
  cor.slopes <- NULL

  # Get variance of fixed effects: multiply coefs by design matrix
  if (component %in% c("fixed", "all")) {
    var.fixed <- .compute_variance_fixed(mixed_effects_info)
  }

  # Are random slopes present as fixed effects? Warn.
  if (!.random_slopes_in_fixed(model) && verbose) {
    format_warning(
      sprintf("Random slopes not present as fixed effects. This artificially inflates the conditional %s.", name_full),
      "Solution: Respecify fixed structure!"
    )
  }

  # Separate observation variance from variance of random effects, e.g.
  # if we have a random effects term that has one observation per group
  # (i.e. number of random effects "groups" is the same as number of
  # observations in the model)
  nr <- vapply(mixed_effects_info$re, nrow, numeric(1))
  not_obs_terms <- names(nr[nr != n_obs(model)])
  obs_terms <- names(nr[nr == n_obs(model)])

  # Variance of random effects
  if (component %in% c("random", "all") && isFALSE(no_random_variance)) {
    var.random <- .compute_variance_random(model, not_obs_terms, mixed_effects_info)
  }

  # Variance of random effects for NULL model
  if (!singular_fit && !is.null(me_info_null)) {
    # Separate observation variance from variance of random effects
    nr <- vapply(me_info_null$re, nrow, numeric(1))
    not_obs_terms_null <- names(nr[nr != n_obs(model_null)])
    var.random_null <- .compute_variance_random(
      model = model_null,
      not_obs_terms_null,
      mixed_effects_info = me_info_null
    )
  }

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  if (component %in% c("residual", "distribution", "all")) {
    var.distribution <- .compute_variance_distribution(
      model,
      var_cor = mixed_effects_info$vc,
      faminfo,
      model_null = model_null,
      revar_null = var.random_null,
      approx_method = approximation,
      name = name_full,
      model_component = model_component,
      verbose = verbose
    )
  }

  if (component %in% c("residual", "dispersion", "all")) {
    var.dispersion <- .compute_variance_dispersion(
      model,
      mixed_effects_info = mixed_effects_info,
      faminfo = faminfo,
      obs_terms = obs_terms
    )
  }

  if (component %in% c("residual", "all")) {
    var.residual <- var.distribution + var.dispersion
  }

  if (isTRUE(faminfo$is_mixed) || inherits(model, c("wblm", "wbgee"))) {
    if (component %in% c("intercept", "all")) {
      var.intercept <- .between_subject_variance(mixed_effects_info)
    }

    if (component %in% c("slope", "all")) {
      var.slope <- .random_slope_variance(model, mixed_effects_info)
    }

    if (component %in% c("rho01", "all")) {
      cor.slope_intercept <- .random_slope_intercept_corr(model, mixed_effects_info)
    }

    if (component %in% c("rho00", "all")) {
      cor.slopes <- .random_slopes_corr(model, mixed_effects_info)
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


  compact_list(list(
    var.fixed = var.fixed,
    var.random = var.random,
    var.residual = var.residual,
    var.distribution = var.distribution,
    var.dispersion = var.dispersion,
    var.intercept = var.intercept,
    var.slope = var.slope,
    cor.slope_intercept = cor.slope_intercept,
    cor.slopes = cor.slopes
  ))
}


# store essential information on coefficients, model matrix and so on
# as list, since we need these information throughout the functions to
# calculate the variance components...
#
# basically, this function should return a list that has the same
# structure for any mixed models like this code for lme4:
# beta = lme4::fixef(model),
# X = lme4::getME(model, "X"),
# vc = lme4::VarCorr(model),
# re = lme4::ranef(model)
#
.get_variance_information <- function(model,
                                      faminfo,
                                      name_fun = "get_variances",
                                      verbose = TRUE) {
  # sanity check
  if (is.null(model)) {
    return(NULL)
  }


  check_if_installed("lme4", reason = "to compute variances for mixed models")

  if (inherits(model, "lme")) {
    check_if_installed("nlme", reason = "to compute variances for mixed models")
  }

  if (inherits(model, "rstanarm")) {
    check_if_installed("rstanarm", reason = "to compute variances for mixed models")
  }

  # stanreg
  # ---------------------------
  if (inherits(model, "stanreg")) {
    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = rstanarm::get_x(model),
      vc = lme4::VarCorr(model),
      re = lme4::ranef(model)
    )

    # GLMMapdative
    # ---------------------------
  } else if (inherits(model, "MixMod")) {
    vc1 <- vc2 <- NULL
    re_names <- find_random(model)

    vc_cond <- !startsWith(colnames(model$D), "zi_")
    if (any(vc_cond)) {
      vc1 <- model$D[vc_cond, vc_cond, drop = FALSE]
      attr(vc1, "stddev") <- sqrt(diag(vc1))
      attr(vc1, "correlation") <- stats::cov2cor(model$D[vc_cond, vc_cond, drop = FALSE])
    }

    vc_zi <- startsWith(colnames(model$D), "zi_")
    if (any(vc_zi)) {
      colnames(model$D) <- gsub("^zi_(.*)", "\\1", colnames(model$D))
      rownames(model$D) <- colnames(model$D)
      vc2 <- model$D[vc_zi, vc_zi, drop = FALSE]
      attr(vc2, "stddev") <- sqrt(diag(vc2))
      attr(vc2, "correlation") <- stats::cov2cor(model$D[vc_zi, vc_zi, drop = FALSE])
    }

    model_deviance <- get_deviance(model, verbose = FALSE)
    residual_df <- get_df(model, type = "residual", verbose = FALSE)

    vc1 <- list(vc1)
    names(vc1) <- re_names[[1]]
    attr(vc1, "sc") <- sqrt(abs(model_deviance) / residual_df)

    if (!is.null(vc2)) {
      vc2 <- list(vc2)
      names(vc2) <- re_names[[2]]
      attr(vc2, "sc") <- sqrt(abs(model_deviance) / residual_df)
    }

    vcorr <- compact_list(list(vc1, vc2))
    names(vcorr) <- c("cond", "zi")[seq_along(vcorr)]

    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = get_modelmatrix(model),
      vc = vcorr,
      re = list(lme4::ranef(model))
    )
    names(mixed_effects_info$re) <- model$id_name

    # joineRML
    # ---------------------------
  } else if (inherits(model, "mjoint")) {
    re_names <- find_random(model, flatten = TRUE)
    vcorr <- summary(model)$D
    attr(vcorr, "stddev") <- sqrt(diag(vcorr))
    attr(vcorr, "correlation") <- stats::cov2cor(vcorr)
    vcorr <- list(vcorr)
    names(vcorr) <- re_names[1]
    attr(vcorr, "sc") <- model$coef$sigma2[[1]]

    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = matrix(1, nrow = n_obs(model), dimnames = list(NULL, "(Intercept)_1")),
      vc = vcorr,
      re = list(lme4::ranef(model))
    )
    names(mixed_effects_info$re) <- re_names[seq_along(mixed_effects_info$re)]

    # nlme / glmmPQL
    # ---------------------------
  } else if (inherits(model, "lme")) {
    re_names <- find_random(model, split_nested = TRUE, flatten = TRUE)
    comp_x <- get_modelmatrix(model)
    rownames(comp_x) <- seq_len(nrow(comp_x))
    if (.is_nested_lme(model)) {
      vals_vc <- .get_nested_lme_varcorr(model)
      vals_re <- lme4::ranef(model)
    } else {
      vals_vc <- list(nlme::getVarCov(model))
      vals_re <- list(lme4::ranef(model))
    }
    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = comp_x,
      vc = vals_vc,
      re = vals_re
    )
    names(mixed_effects_info$re) <- re_names
    names(mixed_effects_info$vc) <- re_names

    # ordinal
    # ---------------------------
  } else if (inherits(model, "clmm")) {
    if (requireNamespace("ordinal", quietly = TRUE)) {
      mm <- get_modelmatrix(model)
      mixed_effects_info <- list(
        beta = c("(Intercept)" = 1, stats::coef(model)[intersect(names(stats::coef(model)), colnames(mm))]),
        X = mm,
        vc = ordinal::VarCorr(model),
        re = ordinal::ranef(model)
      )
    }

    # glmmadmb
    # ---------------------------
  } else if (inherits(model, "glmmadmb")) {
    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = get_modelmatrix(model),
      vc = lme4::VarCorr(model),
      re = lme4::ranef(model)
    )

    # brms
    # ---------------------------
  } else if (inherits(model, "brmsfit")) {
    comp_x <- get_modelmatrix(model)
    rownames(comp_x) <- seq_len(nrow(comp_x))
    vc <- lapply(names(lme4::VarCorr(model)), function(i) {
      element <- lme4::VarCorr(model)[[i]]
      if (i != "residual__") {
        if (is.null(element$cov)) {
          out <- as.matrix(drop(element$sd[, 1])^2)
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$sd), fixed = TRUE)
        } else {
          out <- as.matrix(drop(element$cov[, 1, ]))
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$cov), fixed = TRUE)
        }
        attr(out, "sttdev") <- element$sd[, 1]
      } else {
        out <- NULL
      }
      out
    })
    vc <- compact_list(vc)
    names(vc) <- setdiff(names(lme4::VarCorr(model)), "residual__")
    attr(vc, "sc") <- lme4::VarCorr(model)$residual__$sd[1, 1]
    fixef_params <- lme4::fixef(model)[, 1]
    # remove sigma parameters
    fixef_params <- fixef_params[!startsWith(names(fixef_params), "sigma_")]
    mixed_effects_info <- list(
      beta = fixef_params,
      X = comp_x,
      vc = vc,
      re = lapply(lme4::ranef(model), function(re) {
        reval <- as.data.frame(drop(re[, 1, ]))
        colnames(reval) <- gsub("Intercept", "(Intercept)", dimnames(re)[[3]], fixed = TRUE)
        reval
      })
    )
    names(mixed_effects_info$beta) <- gsub("Intercept", "(Intercept)", names(mixed_effects_info$beta), fixed = TRUE)

    # cpglmm
    # ---------------------------
  } else if (inherits(model, "cpglmm")) {
    check_if_installed("cplm")

    mixed_effects_info <- list(
      beta = cplm::fixef(model),
      X = cplm::model.matrix(model),
      vc = cplm::VarCorr(model),
      re = cplm::ranef(model)
    )

    # lme4 / glmmTMB
    # ---------------------------
  } else {
    mixed_effects_info <- list(
      beta = lme4::fixef(model),
      X = lme4::getME(model, "X"),
      vc = lme4::VarCorr(model),
      re = lme4::ranef(model)
    )
  }


  # for models with zero-inflation, we only want the conditional part
  if (inherits(model, c("glmmTMB", "MixMod"))) {
    mixed_effects_info <- lapply(mixed_effects_info, .collapse_cond)
  }

  # currently, we don't support calculating all variance components
  # for the zero-inflated part of the model only. This is not fully implemented
  # mixed_effects_info <- lapply(mixed_effects_info, .collapse_zi)

  # for glmmTMB, tell user that dispersion model is ignored
  if (!is.null(find_formula(model, verbose = FALSE)[["dispersion"]]) && verbose) {
    format_warning(sprintf("%s ignores effects of dispersion model.", name_fun))
  }

  # fix rank deficiency
  rankdef <- is.na(mixed_effects_info$beta)
  if (any(rankdef)) {
    rankdef_names <- names(mixed_effects_info$beta)[rankdef]
    mixed_effects_info$beta <- mixed_effects_info$beta[setdiff(names(mixed_effects_info$beta), rankdef_names)]
  }

  mixed_effects_info
}


# helper-function, telling user if family / distribution
# is supported or not
.badlink <- function(link, family, verbose = TRUE) {
  if (verbose) {
    format_warning(sprintf(
      "Model link `%s` is not yet supported for the %s distribution.", link, family
    ))
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


# same as above, but for the zero-inflation component
.collapse_zi <- function(x) {
  if (is.list(x) && "zi" %in% names(x)) {
    x[["zi"]]
  } else {
    x
  }
}


# fixed effects variance ------------------------------------------------------
#
# This is in line with Nakagawa et al. 2017, Suppl. 2
# see https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2017.0213&file=rsif20170213supp2.pdf
#
# However, package MuMIn differs and uses "fitted()" instead, leading to minor
# deviations
# -----------------------------------------------------------------------------
.compute_variance_fixed <- function(mixed_effects_info) {
  with(mixed_effects_info, stats::var(as.vector(beta %*% t(X))))
}


# dispersion-specific variance ----
# ---------------------------------
.compute_variance_dispersion <- function(model, mixed_effects_info, faminfo, obs_terms) {
  if (faminfo$is_linear) {
    0
  } else if (length(obs_terms) == 0) {
    0
  } else {
    .compute_variance_random(model, obs_terms, mixed_effects_info)
  }
}


# variance associated with a random-effects term (Johnson 2014) --------------
#
# This is in line with Nakagawa et al. 2017, Suppl. 2, and package MuMIm
# see https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2017.0213&file=rsif20170213supp2.pdf
# ----------------------------------------------------------------------------
.compute_variance_random <- function(model, terms, mixed_effects_info) {
  if (is.null(terms)) {
    return(NULL)
  }
  .sigma_sum <- function(Sigma) {
    rn <- rownames(Sigma)

    # fix for models w/o intercept
    if (!any(startsWith(colnames(mixed_effects_info$X), "(Intercept)"))) {
      mixed_effects_info$X <- cbind("(Intercept)" = 1, mixed_effects_info$X)
    }

    if (!is.null(rn)) {
      valid <- rownames(Sigma) %in% colnames(mixed_effects_info$X)
      if (!all(valid)) {
        rn <- rn[valid]
        Sigma <- Sigma[valid, valid, drop = FALSE]
      }
    }

    Z <- mixed_effects_info$X[, rn, drop = FALSE]
    Z.m <- Z %*% Sigma
    sum(diag(crossprod(Z.m, Z))) / n_obs(model)
  }

  # if (inherits(x, "MixMod")) {
  #   .sigma_sum(mixed_effects_info$vc)
  # } else {
  #   sum(sapply(mixed_effects_info$vc[terms], .sigma_sum))
  # }
  sum(sapply(mixed_effects_info$vc[terms], .sigma_sum))
}


# distribution-specific (residual) variance (Nakagawa et al. 2017) ------------
# (also call obersvation level variance).
#
# This is in line with Nakagawa et al. 2017, Suppl. 2, and package MuMIm
# see https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2017.0213&file=rsif20170213supp2.pdf
#
# We need:
# - the overdispersion parameter / sigma
# - the null model for the conditional mean (for the observation level variance)
#
# There may be small deviations to Nakagawa et al. for the null-model, which
# despite being correctly re-formulated in "null_model()", returns slightly
# different values for the log/delta/trigamma approximation.
# -----------------------------------------------------------------------------
.compute_variance_distribution <- function(model,
                                           var_cor,
                                           faminfo,
                                           model_null = NULL,
                                           revar_null = NULL,
                                           name,
                                           approx_method = "lognormal",
                                           model_component = NULL,
                                           verbose = TRUE) {
  # get overdispersion parameter / sigma
  sig <- .safe(.get_sigma(model, no_recursion = TRUE))

  if (is.null(sig)) {
    # for brms-models, when sigma is modeled, there is no longer a
    # single sigma parameter. in this case, we can't calculate residual
    # variance and return NULL
    if (inherits(model, "brmsfit")) {
      params <- find_parameters(model)$conditional
      sigma_params <- grepl("b_sigma", params, fixed = TRUE)
      if (sum(sigma_params) > 1) {
        if (verbose) {
          format_alert("`sigma` is modeled directly, and hence there is no longer a single sigma parameter to calculate the residual variance. Returning `NULL` instead.") # nolint
        }
        return(NULL)
      }
    }
    sig <- 1
  }

  # Distribution-specific variance depends on the model-family
  # and the related link-function

  if (faminfo$is_linear && !faminfo$is_tweedie) {
    # linear / Gaussian ----
    # ----------------------

    resid.variance <- sig^2
  } else if (faminfo$is_betabinomial) {
    # beta-binomial ----
    # ------------------

    resid.variance <- switch(faminfo$link_function,
      logit = ,
      probit = ,
      cloglog = ,
      clogloglink = .variance_distributional(
        model,
        faminfo = faminfo,
        sig = sig,
        model_null = model_null,
        revar_null = revar_null,
        approx_method = approx_method,
        name = name,
        model_component = model_component,
        verbose = verbose
      ),
      .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
    )
  } else if (faminfo$is_binomial) {
    # binomial / bernoulli  ----
    # --------------------------

    # we need this to adjust for "cbind()" outcomes
    y_factor <- .binomial_response_weight(model)

    # for observation level approximation, when we don't want the "fixed"
    # residual variance, pi^2/3, but the variance based on the distribution
    # of the response
    pmean <- .obs_level_variance(model_null, revar_null)

    # sanity check - clmm-models are "binomial" but have no pmean
    if (is.null(pmean) && identical(approx_method, "observation_level")) {
      approx_method <- "lognormal" # we don't have lognormal, it's just the default
    }

    resid.variance <- switch(faminfo$link_function,
      logit = switch(approx_method,
        observation_level = 1 / (y_factor * pmean * (1 - pmean)),
        pi^2 / (3 * y_factor)
      ),
      probit = switch(approx_method,
        observation_level = 2 * pi / y_factor * pmean * (1 - pmean) * exp((stats::qnorm(pmean) / sqrt(2))^2)^2, # nolint
        1 / y_factor
      ),
      cloglog = ,
      clogloglink = switch(approx_method,
        observation_level = pmean / y_factor / log(1 - pmean)^2 / (1 - pmean),
        pi^2 / (6 * y_factor)
      ),
      .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
    )
  } else if (faminfo$is_tweedie) {
    # Tweedie  ----
    # -------------

    resid.variance <- .variance_distributional(
      model,
      faminfo = faminfo,
      sig = sig,
      model_null = model_null,
      revar_null = revar_null,
      approx_method = approx_method,
      name = name,
      verbose = verbose
    )
  } else if (faminfo$is_count) {
    # count  ----
    # -----------

    resid.variance <- switch(faminfo$link_function,
      log = .variance_distributional(
        model,
        faminfo = faminfo,
        sig = sig,
        model_null = model_null,
        revar_null = revar_null,
        approx_method = approx_method,
        name = name,
        model_component = model_component,
        verbose = verbose
      ),
      sqrt = 0.25 * sig,
      .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
    )
  } else if (faminfo$family %in% c("Gamma", "gamma")) {
    # Gamma  ----
    # -----------

    resid.variance <- switch(faminfo$link_function,
      inverse = ,
      identity = stats::family(model)$variance(sig),
      log = switch(approx_method,
        delta = 1 / sig^-2,
        trigamma = trigamma(sig^-2),
        log1p(1 / sig^-2)
      ),
      .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
    )
  } else if (faminfo$family == "beta" || faminfo$is_orderedbeta) {
    # (Ordered) Beta  ----
    # --------------------

    resid.variance <- switch(faminfo$link_function,
      logit = .variance_distributional(
        model,
        faminfo = faminfo,
        sig = sig,
        model_null = model_null,
        revar_null = revar_null,
        name = name,
        approx_method = approx_method,
        model_component = model_component,
        verbose = verbose
      ),
      .badlink(faminfo$link_function, faminfo$family, verbose = verbose)
    )
  } else {
    resid.variance <- sig
  }

  resid.variance
}


# helper for .compute_variance_distribution()
#
# calculate weight if we have a "cbind()" response for binomial models
# needed to weight the residual variance
.binomial_response_weight <- function(model) {
  # we need this to adjust for "cbind()" outcomes
  resp_value <- .safe(stats::model.frame(model)[, find_response(model)])
  # sanity check
  if (is.null(resp_value)) {
    resp_value <- .safe(get_data(model, source = "frame")[, find_response(model)])
  }
  if (!is.null(resp_value) && !is.null(ncol(resp_value)) && ncol(resp_value) > 1) {
    mean(rowSums(resp_value, na.rm = TRUE))
  } else {
    1
  }
}


# helper for .compute_variance_distribution()
#
# for observation level approximation, when we don't want the "fixed"
# residual variance, pi^2/3, but the variance based on the distribution
# of the response
.obs_level_variance <- function(model_null, revar_null) {
  fe_null <- .safe(as.numeric(.collapse_cond(lme4::fixef(model_null))))
  .safe(as.numeric(stats::plogis(fe_null - 0.5 * sum(revar_null) * tanh(fe_null * (1 + 2 * exp(-0.5 * sum(revar_null))) / 6)))) # nolint
}


# This is the core-function to calculate the distribution-specific variance
# (also call obersvation level variance). Nakagawa et al. 2017 propose three
# different methods, which are now also implemented here.
#
# This is in line with Nakagawa et al. 2017, Suppl. 2, and package MuMIm
# see https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2017.0213&file=rsif20170213supp2.pdf
#
# There may be small deviations to Nakagawa et al. for the null-model, which
# despite being correctly re-formulated in "null_model()", returns slightly
# different values for the log/delta/trigamma approximation.
#
# what we get here is the following rom the Nakagawa et al. Supplement:
# VarOdF <- 1 / lambda + 1 / thetaF # the delta method
# VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
# VarOtF <- trigamma((1 / lambda + 1 / thetaF)^(-1)) # trigamma function
# -----------------------------------------------------------------------------
.variance_distributional <- function(model,
                                     faminfo,
                                     sig,
                                     model_null = NULL,
                                     revar_null = NULL,
                                     name,
                                     approx_method = "lognormal",
                                     model_component = NULL,
                                     verbose = TRUE) {
  check_if_installed("lme4", "to compute variances for mixed models")

  # ------------------------------------------------------------------
  # approximation of distributional variance, see Nakagawa et al. 2017
  # in general want something like log(1+var(x)/mu^2) for log-approximation,
  # and for the other approximations accordingly. Therefore, we need
  # "mu" (the conditional mean of the null model - that's why we need
  # the null model here) and the overdispersion / sigma parameter
  # ------------------------------------------------------------------

  # check if null-model could be computed
  if (is.null(model_null)) {
    mu <- NA
  } else {
    if (inherits(model_null, "cpglmm")) {
      check_if_installed("cplm")
      null_fixef <- unname(cplm::fixef(model_null))
    } else {
      null_fixef <- unname(.collapse_cond(lme4::fixef(model_null)))
    }
    # brmsfit also returns SE and CI, so we just need the first value
    if (inherits(model_null, "brmsfit")) {
      null_fixef <- as.vector(null_fixef)[1]
    }
    mu <- null_fixef
  }

  if (is.na(mu)) {
    if (verbose) {
      format_warning(
        "Can't calculate model's distribution-specific variance. Results are not reliable.",
        "A reason can be that the null model could not be computed manually. Try to fit the null model manually and pass it to `null_model`." # nolint
      )
    }
    return(0)
  }

  # transform expected mean of the null model
  if (is.null(faminfo$family)) {
    mu <- link_inverse(model)(mu)
  } else {
    # transform mu
    mu <- switch(faminfo$family,
      # beta-alike
      beta = ,
      betabinomial = ,
      ordbeta = stats::plogis(mu),
      # for count models, Nakagawa et al. suggest this transformation
      poisson = ,
      quasipoisson = ,
      genpois = ,
      nbinom = ,
      nbinom1 = ,
      nbinom2 = ,
      nbinom12 = ,
      negbinomial = ,
      tweedie = ,
      `negative binomial` = exp(mu + 0.5 * as.vector(revar_null)),
      # all other models
      link_inverse(model)(mu)
    )
  }

  # ------------------------------------------------------------------
  # we have to make exception and check for tweedie-link_function here.
  # Some models from tweedie family have saved this information in
  # "faminfo$family", but some also in "faminfo$link_function" (with a different
  # family). So we have to check for both.
  # Same applies to zero-inflated models, that's why we better double-check here.
  # If variances for conditional model only are requested, we check the same
  # families below again.
  # ------------------------------------------------------------------

  cvsquared <- tryCatch(
    {
      if (faminfo$link_function == "tweedie") {
        # Tweedie models ------------------------------------------------------
        # ---------------------------------------------------------------------
        dispersion_param <- .variance_family_tweedie(model, mu, sig)
      } else if (identical(model_component, "full") && (faminfo$is_zero_inflated || faminfo$is_hurdle)) {
        # Zero Inflated models ------------------------------------------------
        # ---------------------------------------------------------------------
        dispersion_param <- switch(faminfo$family,

          # (zero-inflated) poisson ----
          # ----------------------------
          poisson = ,
          `zero-inflated poisson` = .variance_family_poisson(model, mu, faminfo),

          # (zero-inflated) negative binomial ----
          # --------------------------------------
          genpois = ,
          nbinom = ,
          nbinom1 = ,
          nbinom2 = ,
          nbinom12 = ,
          negbinomial = ,
          `negative binomial` = ,
          `zero-inflated negative binomial` = .variance_family_nbinom(model, mu, sig, faminfo),

          # hurdle and Gamma ----
          # -------------------------
          gamma = ,
          Gamma = ,
          `hurdle poisson` = ,
          truncated_poisson = stats::family(model)$variance(mu),
          truncated_nbinom2 = stats::family(model)$variance(mu, sig),

          # others ----
          # -----------
          sig
        )
      } else {
        # All other families (or conditional model only) ------------------
        # -----------------------------------------------------------------
        dispersion_param <- switch(faminfo$family,

          # (compoised) poisson ----
          # ------------------------
          poisson = 1,
          `zero-inflated poisson` = 1,

          # Gamma, exponential ----
          # -----------------------
          gamma = ,
          Gamma = stats::family(model)$variance(sig),

          # negative binomial ----
          # --------------------------------------
          nbinom = ,
          nbinom1 = ,
          nbinom2 = ,
          nbinom12 = ,
          quasipoisson = ,
          negbinomial = ,
          `negative binomial` = ,
          genpois = ,
          `zero-inflated negative binomial` = sig,

          # beta-alike ----
          # ---------------
          beta = .variance_family_beta(model, mu, sig),
          ordbeta = .variance_family_orderedbeta(model, mu, sig),
          betabinomial = .variance_family_betabinom(model, mu, sig),

          ## TODO: check alternatives, but probably less accurate
          # betabinomial = .variance_family_beta(model, mu, sig),
          # betabinomial = stats::family(model)$variance(mu, sig),

          # tweed distributions ----
          # ------------------------
          tweedie = .variance_family_tweedie(model, mu, sig),

          # default variance for non-captured distributions ----
          # ----------------------------------------------------
          .variance_family_default(model, mu, verbose)
        )
      }

      if (dispersion_param < 0 && isTRUE(verbose)) {
        format_warning(
          "Model's distribution-specific variance is negative. Results are not reliable."
        )
      }

      # now compute cvsquared -------------------------------------------

      # for cpglmm with tweedie link, the model is not of tweedie family,
      # only the link function is tweedie
      if (faminfo$link_function == "tweedie") {
        dispersion_param
      } else if (identical(approx_method, "trigamma")) {
        switch(faminfo$family,
          nbinom = ,
          nbinom1 = ,
          nbinom2 = ,
          nbinom12 = ,
          negbinomial = ,
          genpois = ,
          `negative binomial` = ((1 / mu) + (1 / dispersion_param))^-1,
          poisson = ,
          quasipoisson = mu / dispersion_param,
          dispersion_param / mu^2
        )
      } else {
        switch(faminfo$family,
          nbinom = ,
          nbinom1 = ,
          nbinom2 = ,
          nbinom12 = ,
          negbinomial = ,
          genpois = ,
          `negative binomial` = (1 / mu) + (1 / dispersion_param),
          poisson = ,
          quasipoisson = dispersion_param / mu,
          dispersion_param / mu^2
        )
      }
    },
    error = function(e) {
      if (verbose) {
        format_warning(
          "Can't calculate model's distribution-specific variance. Results are not reliable.",
          paste("The following error occured: ", e$message)
        )
      }
      0
    }
  )

  switch(approx_method,
    delta = cvsquared,
    trigamma = trigamma(cvsquared),
    log1p(cvsquared)
  )
}


# Get distributional variance for poisson-family
# ----------------------------------------------
.variance_family_poisson <- function(model, mu, faminfo) {
  if (faminfo$is_zero_inflated) {
    .variance_zip(model, faminfo, family_var = mu)
  } else if (inherits(model, "MixMod")) {
    mu
  } else if (inherits(model, "cpglmm")) {
    .get_cplm_family(model)$variance(mu)
  } else {
    stats::family(model)$variance(mu)
  }
}


# Get distributional variance for beta-family
# ----------------------------------------------
.variance_family_beta <- function(model, mu, phi) {
  mu * (1 - mu) / (1 + phi)
  # was:
  # stats::family(model)$variance(mu)
  # But the conditional variance should definitely be divided by (1+phi)
  # see also https://github.com/easystats/performance/issues/742
}


# Get distributional variance for ordered beta-family
# ----------------------------------------------
.variance_family_orderedbeta <- function(model, mu, phi) {
  if (inherits(model, "MixMod")) {
    stats::family(model)$variance(mu)
  } else {
    mu * (1 - mu) / (1 + phi)
  }
}


# Get distributional variance for beta-family
# ----------------------------------------------
.variance_family_betabinom <- function(model, mu, phi) {
  if (inherits(model, "MixMod")) {
    stats::family(model)$variance(mu)
  } else {
    mu * (1 - mu) / (1 + phi)
    # n <- .binomial_response_weight(x)
    # mu * (1 - mu) * (n * (phi + n) / (1 + phi))
  }
}


# Get distributional variance for tweedie-family
# ----------------------------------------------
.variance_family_tweedie <- function(model, mu, phi) {
  if (inherits(model, "cpglmm")) {
    phi <- model@phi
    p <- model@p - 2
  } else {
    check_if_installed("glmmTMB")
    p <- unname(unlist(glmmTMB::family_params(model)))
  }
  phi * mu^p
}


# Get distributional variance for nbinom-family
# ----------------------------------------------
.variance_family_nbinom <- function(model, mu, sig, faminfo) {
  if (faminfo$is_zero_inflated) {
    if (missing(sig)) sig <- 0
    .variance_zinb(model, sig, faminfo, family_var = mu * (1 + sig))
  } else if (inherits(model, "MixMod")) {
    if (missing(sig)) {
      return(rep(1e-16, length(mu)))
    }
    mu * (1 + sig)
  } else if (identical(faminfo$family, "nbinom12")) {
    # nbinom12-family from glmmTMB requires psi-parameter
    if ("psi" %in% names(model$fit$par)) {
      psi <- model$fit$par["psi"]
    } else {
      format_error("Could not extract psi-parameter for the distributional variance for nbinom12-family.")
    }
    stats::family(model)$variance(mu, sig, psi)
  } else {
    stats::family(model)$variance(mu, sig)
  }
}


# For zero-inflated negative-binomial models,
# the distributional variance is based on Zuur et al. 2012
# ----------------------------------------------
.variance_zinb <- function(model, sig, faminfo, family_var) {
  if (inherits(model, "glmmTMB")) {
    if (identical(faminfo$family, "nbinom12")) {
      # nbinom12-family from glmmTMB requires psi-parameter
      if ("psi" %in% names(model$fit$par)) {
        psi <- model$fit$par["psi"]
      } else {
        format_error("Could not extract psi-parameter for the distributional variance for nbinom12-family.")
      }
    }
    v <- stats::family(model)$variance
    # zi probability
    p <- stats::predict(model, type = "zprob")
    # mean of conditional distribution
    mu <- stats::predict(model, type = "conditional")
    # sigma
    betadisp <- model$fit$par["betadisp"]
    k <- switch(faminfo$family,
      gaussian = exp(0.5 * betadisp),
      Gamma = exp(-0.5 * betadisp),
      exp(betadisp)
    )
    if (identical(faminfo$family, "nbinom12")) {
      pvar <- (1 - p) * v(mu, k, psi) + mu^2 * (p^2 + p)
    } else {
      pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
    }
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
.variance_family_default <- function(model, mu, verbose) {
  check_if_installed("lme4")

  tryCatch(
    if (inherits(model, "merMod")) {
      mu * (1 + mu / lme4::getME(model, "glmer.nb.theta"))
    } else if (inherits(model, "MixMod")) {
      stats::family(model)$variance(mu)
    } else if (inherits(model, "glmmTMB")) {
      if (is.null(model$theta)) {
        theta <- lme4::getME(model, "theta")
      } else {
        theta <- model$theta
      }
      mu * (1 + mu / theta)
    } else {
      mu * (1 + mu / model$theta)
    },
    error = function(e) {
      if (verbose) {
        format_warning("Can't calculate model's distribution-specific variance. Results are not reliable.")
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
  fe <- compact_list(fe[c("conditional", "zero_inflated")])
  if (length(rs) > length(fe)) rs <- rs[seq_along(fe)]
  if (length(fe) > length(rs)) fe <- fe[seq_along(rs)]

  all(mapply(function(r, f) all(r %in% f), rs, fe, SIMPLIFY = TRUE))
}


# random intercept-variances, i.e.
# between-subject-variance (tau 00) ----
# ----------------------------------------------
.between_subject_variance <- function(mixed_effects_info) {
  vars <- lapply(mixed_effects_info$vc, function(i) i[1])
  # check for uncorrelated random slopes-intercept
  non_intercepts <- which(sapply(mixed_effects_info$vc, function(i) {
    !startsWith(dimnames(i)[[1]][1], "(Intercept)")
  }))
  if (length(non_intercepts)) {
    vars <- vars[-non_intercepts]
  }

  sapply(vars, function(i) i)
}


# random slope-variances (tau 11) ----
# ----------------------------------------------
.random_slope_variance <- function(model, mixed_effects_info) {
  if (inherits(model, "lme")) {
    unlist(lapply(mixed_effects_info$vc, function(i) diag(i)[-1]))
  } else {
    # random slopes for correlated slope-intercept
    out <- unlist(lapply(mixed_effects_info$vc, function(i) diag(i)[-1]))
    # check for uncorrelated random slopes-intercept
    non_intercepts <- which(sapply(mixed_effects_info$vc, function(i) {
      !startsWith(dimnames(i)[[1]][1], "(Intercept)")
    }))
    if (length(non_intercepts) == length(mixed_effects_info$vc)) {
      out <- unlist(lapply(mixed_effects_info$vc, diag))
    } else if (length(non_intercepts)) {
      dn <- unlist(lapply(mixed_effects_info$vc, function(i) dimnames(i)[1])[non_intercepts])
      rndslopes <- unlist(lapply(mixed_effects_info$vc, function(i) {
        if (is.null(dim(i)) || identical(dim(i), c(1, 1))) {
          as.vector(i)
        } else {
          as.vector(diag(i))
        }
      })[non_intercepts])
      # random slopes for uncorrelated slope-intercept
      names(rndslopes) <- gsub("(.*)\\.\\d+$", "\\1", names(rndslopes))
      rndslopes <- stats::setNames(rndslopes, paste0(names(rndslopes), ".", dn))
      # anything missing? (i.e. correlated slope-intercept slopes)
      missig_rnd_slope <- setdiff(names(out), names(rndslopes))
      if (length(missig_rnd_slope)) {
        # validation check
        to_remove <- NULL
        for (j in seq_along(out)) {
          # identical random slopes might have different names, so
          # we here check if random slopes from correlated and uncorrelated
          # are duplicated (i.e. their difference is 0 - including a tolerance)
          # and then remove duplicated elements
          the_same <- which(abs(outer(out[j], rndslopes, `-`)) < 0.0001)
          if (length(the_same) && grepl(dn[the_same], names(out[j]), fixed = TRUE)) {
            to_remove <- c(to_remove, j)
          }
        }
        if (length(to_remove)) {
          out <- out[-to_remove]
        }
        out <- c(out, rndslopes)
      } else {
        out <- rndslopes
      }
    }
    out
  }
}


# slope-intercept-correlations (rho 01) ----
# ----------------------------------------------
.random_slope_intercept_corr <- function(model, mixed_effects_info) {
  if (inherits(model, "lme")) {
    rho01 <- unlist(sapply(mixed_effects_info$vc, attr, which = "cor_slope_intercept"))
    if (is.null(rho01)) {
      vc <- lme4::VarCorr(model)
      if ("Corr" %in% colnames(vc)) {
        re_name <- find_random(model, split_nested = FALSE, flatten = TRUE)
        rho01 <- as.vector(suppressWarnings(stats::na.omit(as.numeric(vc[, "Corr"]))))
        if (length(re_name) == length(rho01)) {
          names(rho01) <- re_name
        }
      }
    }
    rho01
  } else {
    corrs <- lapply(mixed_effects_info$vc, attr, "correlation")
    rho01 <- sapply(corrs, function(i) {
      if (!is.null(i) && colnames(i)[1] == "(Intercept)") {
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
.random_slopes_corr <- function(model, mixed_effects_info) {
  corrs <- lapply(mixed_effects_info$vc, attr, "correlation")
  rnd_slopes <- unlist(find_random_slopes(model))

  # check if any categorical random slopes. we then have
  # correlation among factor levels
  cat_random_slopes <- tryCatch(
    {
      d <- get_data(model, verbose = FALSE)[rnd_slopes]
      any(vapply(d, is.factor, logical(1)))
    },
    error = function(e) {
      NULL
    }
  )

  # check if any polynomial / I term in random slopes.
  # we then have correlation among levels
  rs_names <- unique(unlist(lapply(corrs, colnames)))
  pattern <- paste0("(I|poly)(.*)(", paste(rnd_slopes, collapse = "|"), ")")
  poly_random_slopes <- any(grepl(pattern, rs_names))

  if (length(rnd_slopes) < 2 && !isTRUE(cat_random_slopes) && !isTRUE(poly_random_slopes)) {
    return(NULL)
  }

  rho00 <- tryCatch(
    compact_list(lapply(corrs, function(d) {
      d[upper.tri(d, diag = TRUE)] <- NA
      d <- as.data.frame(d)

      d <- .reshape_longer(d, colnames_to = "Parameter1", rows_to = "Parameter2")
      d <- d[stats::complete.cases(d), ]
      d <- d[!d$Parameter1 %in% c("Intercept", "(Intercept)"), ]

      if (nrow(d) == 0) {
        return(NULL)
      }

      d$Parameter <- paste0(d$Parameter1, "-", d$Parameter2)
      d$Parameter1 <- d$Parameter2 <- NULL
      stats::setNames(d$Value, d$Parameter)
    })),
    error = function(e) {
      NULL
    }
  )

  # rho01 <- tryCatch(
  #   {
  #     sapply(corrs, function(i) {
  #       if (!is.null(i)) {
  #         slope_pairs <- utils::combn(model = rnd_slopes, m = 2, simplify = FALSE)
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

  unlist(rho00)
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
