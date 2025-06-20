#' @title Access information from model objects
#' @name model_info
#'
#' @description Retrieve information from model objects.
#'
#' @param response If `x` is a multivariate response model, `model_info()`
#' returns a list of information for each response variable. Set `response` to
#' the number of a specific response variable, or provide the name of the
#' response variable in `response`, to return the information for only one
#' response.
#' @param verbose Toggle off warnings.
#' @inheritParams find_predictors
#' @inheritParams link_inverse
#' @inheritParams find_formula
#'
#' @return A list with information about the model, like family, link-function
#'   etc. (see 'Details').
#'
#' @details `model_info()` returns a list with information about the
#' model for many different model objects. Following information
#' is returned, where all values starting with `is_` are logicals.
#'
#' **Common families and distributions:**
#'
#' * `is_bernoulli`: special case of binomial models: family is Bernoulli
#' * `is_beta`: family is beta
#' * `is_betabinomial`: family is beta-binomial
#' * `is_binomial`: family is binomial (but not negative binomial)
#' * `is_categorical`: family is categorical link
#' * `is_censored`: model is a censored model (has a censored response, including survival models)
#' * `is_count`: model is a count model (i.e. family is either poisson or negative binomial)
#' * `is_cumulative`: family is ordinal or cumulative link
#' * `is_dirichlet`: family is dirichlet
#' * `is_exponential`: family is exponential (e.g. Gamma or Weibull)
#' * `is_linear`: family is gaussian
#' * `is_logit`: model has logit link
#' * `is_multinomial`: family is multinomial or categorical link
#' * `is_negbin`: family is negative binomial
#' * `is_orderedbeta`: family is ordered beta
#' * `is_ordinal`: family is ordinal or cumulative link
#' * `is_poisson`: family is poisson
#' * `is_probit`: model has probit link
#' * `is_tweedie`: family is tweedie
#'
#' **Special model types**:
#'
#' * `is_anova`: model is an Anova object
#' * `is_bayesian`: model is a Bayesian model
#' * `is_dispersion`: model has dispersion component (not only dispersion _parameter_)
#' * `is_gam`: model is a generalized additive model
#' * `is_meta`: model is a meta-analysis object
#' * `is_mixed`: model is a mixed effects model (with random effects)
#' * `is_mixture`: model is a finite mixture model (currently only recognized for
#'   package *brms*).
#' * `is_multivariate`: model is a multivariate response model (currently only works for _brmsfit_ and _vglm/vgam_ objects)
#' * `is_hurdle`: model has zero-inflation component and is a hurdle-model (truncated family distribution)
#' * `is_rtchoice`: model is a *brms* decision-making (sequential sampling) model,
#'   which models outcomes that consists of two components (reaction times and
#'   choice).
#' * `is_survival`: model is a survival model
#' * `is_trial`: model response contains additional information about the trials
#' * `is_truncated`: model is a truncated model (has a truncated response)
#' * `is_wiener`: model is a *brms* decision-making (sequential sampling) model
#'   with Wiener process (also called drift diffusion model)
#' * `is_zero_inflated`: model has zero-inflation component
#'
#' **Hypotheses tests:**
#'
#' * `is_binomtest`: model is an an object of class `htest`, returned by `binom.test()`
#' * `is_chi2test`: model is an an object of class `htest`, returned by `chisq.test()`
#' * `is_correlation`: model is an an object of class `htest`, returned by `cor.test()`
#' * `is_ftest`: model is an an object of class `htest`, and test-statistic
#'   is an F-statistic.
#' * `is_levenetest`: model is an an object of class `anova`, returned by `car::leveneTest()`.
#' * `is_onewaytest`: model is an an object of class `htest`, returned by `oneway.test()`
#' * `is_proptest`: model is an an object of class `htest`, returned by `prop.test()`
#' * `is_ranktest`: model is an an object of class `htest`, returned by `cor.test()`
#'   (if Spearman's rank correlation), `wilcox.text()` or `kruskal.test()`.
#' * `is_ttest`: model is an an object of class `htest`, returned by `t.test()`
#' * `is_variancetest`: model is an an object of class `htest`, returned by
#'   `bartlett.test()`, `shapiro.test()` or `car::leveneTest()`.
#' * `is_xtab`: model is an an object of class `htest` or `BFBayesFactor`, and
#'   test-statistic stems from a contingency table (i.e. `chisq.test()` or
#'   `BayesFactor::contingencyTableBF()`).
#'
#' **Other model information:**
#'
#' * `link_function`: the link-function
#' * `family`: name of the distributional family of the model. For some
#'   exceptions (like some `htest` objects), can also be the name of the test.
#' * `n_obs`: number of observations
#' * `n_grouplevels`: for mixed models, returns names and numbers of random effect groups
#'
#' @examples
#' ldose <- rep(0:5, 2)
#' numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
#' sex <- factor(rep(c("M", "F"), c(6, 6)))
#' SF <- cbind(numdead, numalive = 20 - numdead)
#' dat <- data.frame(ldose, sex, SF, stringsAsFactors = FALSE)
#' m <- glm(SF ~ sex * ldose, family = binomial)
#'
#' # logistic regression
#' model_info(m)
#'
#' # t-test
#' m <- t.test(1:10, y = c(7:20))
#' model_info(m)
#' @export
model_info <- function(x, ...) {
  UseMethod("model_info")
}


# Default methods --------------------------------------

#' @export
model_info.data.frame <- function(x, ...) {
  format_error("A data frame is no valid object for this function.")
}


#' @rdname model_info
#' @export
model_info.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  faminfo <- .safe({
    if (inherits(x, "Zelig-relogit")) {
      stats::binomial(link = "logit")
    } else {
      stats::family(x)
    }
  })

  if (is.null(faminfo)) {
    if (isTRUE(verbose)) {
      format_warning("Could not access model information.")
    }
    NULL
  } else {
    .retrieve_model_info(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      link.fun = faminfo$link,
      verbose = verbose,
      ...
    )
  }
}


#' @export
model_info.model_fit <- function(x, verbose = TRUE, ...) {
  model_info(x$fit, verbose = verbose, ...)
}


# Models with general handling, Gaussian ----------------------------------


#' @export
model_info.anova <- function(x, verbose = TRUE, ...) {
  if (!is.null(attributes(x)$heading) && grepl("Levene's Test", attributes(x)$heading, fixed = TRUE)) {
    .retrieve_model_info(x, verbose = verbose, ...)
  } else {
    NULL
  }
}


#' @export
model_info.asym <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(x, verbose = verbose, ...)
}


#' @export
model_info.mclogit <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(
    x,
    fitfam = "categorical",
    logit.link = TRUE,
    link.fun = "logit",
    verbose = verbose,
    ...
  )
}

#' @export
model_info.mblogit <- model_info.mclogit

#' @export
model_info.mmclogit <- model_info.mclogit

#' @export
model_info.logitr <- model_info.mclogit


#' @export
model_info.maxLik <- function(x, verbose = TRUE, ...) {
  fitfam <- .safe(eval(get_call(x)$family))
  if (is.null(fitfam)) {
    .retrieve_model_info(x, verbose = verbose, ...)
  } else {
    .retrieve_model_info(
      x,
      fitfam = fitfam$family,
      logit.link = fitfam$link == "logit",
      link.fun = fitfam$link,
      verbose = verbose,
      ...
    )
  }
}

#' @export
model_info.mjoint <- model_info.maxLik

#' @export
model_info.censReg <- model_info.maxLik

#' @export
model_info.htest <- model_info.maxLik

#' @export
model_info.BFBayesFactor <- model_info.maxLik

#' @export
model_info.lme <- model_info.maxLik

#' @export
model_info.bayesx <- model_info.maxLik

#' @export
model_info.rq <- model_info.maxLik

#' @export
model_info.rqs <- model_info.maxLik

#' @export
model_info.crq <- model_info.maxLik

#' @export
model_info.crqs <- model_info.maxLik

#' @export
model_info.nlrq <- model_info.maxLik

#' @export
model_info.rqss <- model_info.maxLik

#' @export
model_info.mixed <- model_info.maxLik

#' @export
model_info.plm <- model_info.maxLik

#' @export
model_info.mcmc <- model_info.maxLik

#' @export
model_info.bayesQR <- model_info.maxLik

#' @export
model_info.gls <- model_info.maxLik

#' @export
model_info.nls <- model_info.maxLik

#' @export
model_info.MANOVA <- model_info.maxLik

#' @export
model_info.RM <- model_info.maxLik

#' @export
model_info.truncreg <- model_info.maxLik

#' @export
model_info.lmRob <- model_info.maxLik

#' @export
model_info.speedlm <- model_info.maxLik

#' @export
model_info.lmrob <- model_info.maxLik

#' @export
model_info.complmrob <- model_info.maxLik

#' @export
model_info.lm_robust <- model_info.maxLik

#' @export
model_info.iv_robust <- model_info.maxLik

#' @export
model_info.systemfit <- model_info.maxLik

#' @export
model_info.lqmm <- model_info.maxLik

#' @export
model_info.lqm <- model_info.maxLik

#' @export
model_info.felm <- model_info.maxLik

#' @export
model_info.feis <- model_info.maxLik

#' @export
model_info.ivreg <- model_info.maxLik

#' @export
model_info.ivFixed <- model_info.maxLik

#' @export
model_info.aovlist <- model_info.maxLik

#' @export
model_info.rma <- model_info.maxLik

#' @export
model_info.meta_random <- model_info.maxLik

#' @export
model_info.meta_bma <- model_info.maxLik

#' @export
model_info.meta_fixed <- model_info.maxLik

#' @export
model_info.metaplus <- model_info.maxLik


#' @export
model_info.mlm <- function(x, ...) {
  .retrieve_model_info(x, multi.var = TRUE, ...)
}


#' @export
model_info.afex_aov <- function(x, verbose = TRUE, ...) {
  if (is.null(x$aov)) {
    .retrieve_model_info(x$lm, verbose = verbose, ...)
  } else {
    .retrieve_model_info(x$aov, verbose = verbose, ...)
  }
}


# Models with logit-link --------------------------------


#' @export
model_info.logistf <- function(x, verbose = TRUE, ...) {
  faminfo <- stats::binomial(link = "logit")
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.flac <- model_info.logistf

#' @export
model_info.flic <- model_info.logistf

#' @export
model_info.lrm <- model_info.logistf

#' @export
model_info.blrm <- model_info.logistf

#' @export
model_info.multinom <- model_info.logistf

#' @export
model_info.mlogit <- model_info.logistf

#' @export
model_info.gmnl <- model_info.logistf


# Phylo logit and poisson family ------------------------------------

#' @export
model_info.phylolm <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(x, verbose = verbose, ...)
}


#' @export
model_info.phyloglm <- function(x, verbose = TRUE, ...) {
  if (startsWith(x$method, "logistic")) {
    faminfo <- stats::binomial(link = "logit")
  } else {
    faminfo <- stats::poisson()
  }
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}


# Models with ordinal family ------------------------------------


#' @export
model_info.clm <- function(x, verbose = TRUE, ...) {
  faminfo <- stats::binomial(link = .get_ordinal_link(x))
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.clm2 <- model_info.clm

#' @export
model_info.clmm <- model_info.clm

#' @export
model_info.serp <- model_info.clm

#' @export
model_info.mixor <- model_info.clm

#' @export
model_info.ordinal_weightit <- function(x, verbose = TRUE, ...) {
  faminfo <- stats::binomial(link = .get_ordinal_link(x$family))
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.multinom_weightit <- model_info.ordinal_weightit

#' @export
model_info.mvord <- function(x, verbose = verbose, ...) {
  link_name <- x$rho$link$name
  faminfo <- stats::binomial(link = ifelse(link_name == "mvprobit", "probit", "logit"))
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    multi.var = TRUE,
    verbose = verbose,
    ...
  )
}


# Models with family-function  ----------------------------------


#' @export
model_info.bamlss <- function(x, verbose = TRUE, ...) {
  faminfo <- stats::family(x)
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$links[1] == "logit",
    link.fun = faminfo$links[1],
    verbose = verbose,
    ...
  )
}


#' @export
model_info.speedglm <- function(x, verbose = TRUE, ...) {
  faminfo <- stats::family(x)
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.brmultinom <- model_info.speedglm


# Models with tobit family ----------------------------------


#' @export
model_info.flexsurvreg <- function(x, verbose = TRUE, ...) {
  distribution <- parse(text = safe_deparse(x$call))[[1]]$dist
  faminfo <- .make_tobit_family(x, distribution)

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.tobit <- function(x, verbose = TRUE, ...) {
  faminfo <- .make_tobit_family(x)

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.crch <- model_info.tobit

#' @export
model_info.survreg <- model_info.tobit


# Models with family in object ----------------------------------


#' @export
model_info.MixMod <- function(x, verbose = TRUE, ...) {
  faminfo <- x$family
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    zero.inf = !is.null(stats::formula(x, type = "zi_fixed")),
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.glmmPQL <- model_info.MixMod

#' @export
model_info.bife <- model_info.MixMod


#' @export
model_info.sdmTMB <- function(x, verbose = TRUE, ...) {
  faminfo <- x$family

  # check if we have delta component
  if (length(faminfo$family) > 1) {
    faminfo <- list(
      family = faminfo$family[2],
      link = faminfo$link[2],
      linkfun = faminfo[[2]]$linkfun,
      linkinv = faminfo[[2]]$linkinv
    )
  }

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}


#' @export
model_info.glmx <- function(x, verbose = TRUE, ...) {
  faminfo <- x$family$glm
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    verbose = verbose,
    ...
  )
}


#' @export
model_info.fixest <- function(x, verbose = TRUE, ...) {
  faminfo <- x$family

  if (is.null(faminfo)) {
    if (!is.null(x$method) && x$method == "feols") {
      .retrieve_model_info(x, ...)
    }
  } else if (inherits(faminfo, "family")) {
    .retrieve_model_info(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      link.fun = faminfo$link,
      verbose = verbose,
      ...
    )
  } else {
    fitfam <- switch(faminfo,
      negbin = "negative binomial",
      logit = "binomial",
      faminfo
    )

    link <- switch(faminfo,
      poisson = ,
      negbin = "log",
      logit = "logit",
      gaussian = "identity"
    )

    .retrieve_model_info(
      x = x,
      fitfam = fitfam,
      logit.link = link == "logit",
      link.fun = link,
      verbose = verbose,
      ...
    )
  }
}

#' @export
model_info.feglm <- model_info.fixest


# Survival-models ----------------------------------------

#' @export
model_info.coxph <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "survival",
    logit.link = TRUE,
    link.fun = NULL,
    verbose = verbose,
    ...
  )
}

#' @export
model_info.coxr <- model_info.coxph

#' @export
model_info.aareg <- model_info.coxph

#' @export
model_info.survfit <- model_info.coxph

#' @export
model_info.coxme <- model_info.coxph

#' @export
model_info.riskRegression <- model_info.coxph

#' @export
model_info.comprisk <- model_info.coxph


# Zero-Inflated Models ------------------------------

#' @export
model_info.zeroinfl <- function(x, ...) {
  if (is.list(x$dist)) {
    distribution <- x$dist[[1]]
  } else {
    distribution <- x$dist
  }
  fitfam <- switch(distribution,
    poisson = "poisson",
    negbin = "negative binomial",
    "poisson"
  )
  .retrieve_model_info(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    link.fun = "log",
    ...
  )
}

#' @export
model_info.zerotrunc <- model_info.zeroinfl


#' @export
model_info.hurdle <- function(x, ...) {
  if (is.list(x$dist)) {
    distribution <- x$dist[[1]]
  } else {
    distribution <- x$dist
  }
  fitfam <- switch(distribution,
    poisson = "poisson",
    negbin = "negative binomial",
    "poisson"
  )
  .retrieve_model_info(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    hurdle = TRUE,
    link.fun = "log",
    ...
  )
}


#' @export
model_info.mhurdle <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    zero.inf = TRUE,
    hurdle = TRUE,
    ...
  )
}


# Bayesian Models ---------------------------


#' @export
model_info.stanreg <- function(x, ...) {
  if (inherits(x, "polr")) {
    model_info.polr(x)
  } else {
    model_info.default(x)
  }
}


#' @rdname model_info
#' @export
model_info.brmsfit <- function(x, response = NULL, ...) {
  faminfo <- stats::family(x)
  if (is_multivariate(x)) {
    out <- lapply(faminfo, function(.x) {
      ## TODO: check for multivariate, if we need "x" or ".x"
      form <- find_formula(x, verbose = FALSE)
      is_dispersion <- !is_empty_object(form$sigma) || !is_empty_object(form$kappa)
      .retrieve_model_info(
        x = x,
        fitfam = .x$family,
        zero.inf = FALSE,
        logit.link = .x$link == "logit",
        multi.var = TRUE,
        link.fun = .x$link,
        dispersion = is_dispersion,
        brms_custom_name = faminfo$name,
        ...
      )
    })
    if (is.null(response)) {
      out
    } else {
      out[[response]]
    }
  } else {
    form <- find_formula(x, verbose = FALSE)
    is_dispersion <- !is_empty_object(form$sigma) || !is_empty_object(form$kappa)
    .retrieve_model_info(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      multi.var = FALSE,
      link.fun = faminfo$link,
      dispersion = is_dispersion,
      brms_custom_name = faminfo$name,
      ...
    )
  }
}


#' @export
model_info.stanmvreg <- function(x, response = NULL, ...) {
  faminfo <- stats::family(x)
  out <- lapply(faminfo, function(.x) {
    .retrieve_model_info(
      x = x,
      fitfam = .x$family,
      zero.inf = FALSE,
      logit.link = .x$link == "logit",
      multi.var = TRUE,
      link.fun = .x$link,
      ...
    )
  })
  if (is.null(response)) {
    out
  } else {
    out[[response]]
  }
}


#' @export
model_info.oohbchoice <- function(x, ...) {
  link <- switch(x$distribution,
    normal = ,
    "log-normal" = stats::gaussian()$link,
    logistic = ,
    "log-logistic" = stats::binomial()$link,
    "log"
  )

  fam <- switch(x$distribution,
    normal = ,
    "log-normal" = "gaussian",
    logistic = ,
    "log-logistic" = "binomial",
    "weibull"
  )

  .retrieve_model_info(
    x = x,
    fitfam = fam,
    zero.inf = FALSE,
    logit.link = link == "logit",
    link.fun = link,
    ...
  )
}


#' @export
model_info.BGGM <- function(x, ...) {
  link <- switch(x$type,
    continuous = stats::gaussian(),
    stats::binomial()
  )

  fam <- switch(x$type,
    continuous = "gaussian",
    binary = "binomial",
    "ordinal"
  )

  .retrieve_model_info(
    x = x,
    fitfam = fam,
    zero.inf = FALSE,
    logit.link = link$link == "logit",
    link.fun = link$link,
    ...
  )
}


# Other models ----------------------------

#' @export
model_info.garch <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    ...
  )
}


#' @export
model_info.Rchoice <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = x$family,
    logit.link = x$link == "logit",
    link.fun = x$link,
    ...
  )
}


#' @export
model_info.ivprobit <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "binomial",
    logit.link = FALSE,
    link.fun = "probit",
    ...
  )
}


#' @export
model_info.glht <- function(x, verbose = TRUE, ...) {
  model_info(x$model, verbose = verbose, ...)
}


#' @export
model_info.coeftest <- function(x, ...) {
  NULL
}


#' @export
model_info.glmm <- function(x, ...) {
  f <- switch(tolower(x$family.glmm$family.glmm),
    bernoulli.glmm = ,
    binomial.glmm = stats::binomial("logit"),
    poisson.glmm = stats::poisson("log"),
    stats::gaussian("identity")
  )
  .retrieve_model_info(
    x = x,
    fitfam = f$family,
    logit.link = f$link == "logit",
    multi.var = FALSE,
    link.fun = f$link,
    ...
  )
}


#' @export
model_info.robmixglm <- function(x, ...) {
  f <- switch(tolower(x$family),
    gaussian = stats::gaussian("identity"),
    binomial = stats::binomial("logit"),
    poisson = stats::poisson("log"),
    gamma = stats::Gamma("inverse"),
    truncpoisson = stats::poisson("log"),
    stats::gaussian("identity")
  )
  .retrieve_model_info(
    x = x,
    fitfam = f$family,
    logit.link = f$link == "logit",
    multi.var = FALSE,
    link.fun = f$link,
    zero.inf = x$family == "truncpoisson",
    hurdle = x$family == "truncpoisson",
    ...
  )
}


#' @export
model_info.Arima <- function(x, ...) {
  .retrieve_model_info(x, ...)
}


#' @export
model_info.summary.lm <- model_info.Arima


#' @export
model_info.averaging <- function(x, ...) {
  if (is.null(attributes(x)$modelList)) {
    format_warning("Can't access model information. Please use 'fit = TRUE' in 'model.avg()'.")
    return(NULL)
  }
  model_info(x = attributes(x)$modelList[[1]])
}


#' @export
model_info.merModList <- function(x, ...) {
  model_info.default(x[[1]], ...)
}


#' @export
model_info.cglm <- function(x, ...) {
  link <- parse(text = safe_deparse(x$call))[[1]]$link
  method <- parse(text = safe_deparse(x$call))[[1]]$method

  if (!is.null(method) && method == "clm") {
    .retrieve_model_info(x, ...)
  } else if (is.null(link)) {
    .retrieve_model_info(x, ...)
  } else {
    .retrieve_model_info(
      x,
      logit.link = link == "logit",
      link.fun = link,
      ...
    )
  }
}


#' @export
model_info.cgam <- function(x, ...) {
  faminfo <- x$family
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}


#' @export
model_info.LORgee <- function(x, ...) {
  if (grepl(pattern = "logit", x = x$link, fixed = TRUE)) {
    link <- "logit"
  } else if (grepl(pattern = "probit", x = x$link, fixed = TRUE)) {
    link <- "probit"
  } else if (grepl(pattern = "cauchit", x = x$link, fixed = TRUE)) {
    link <- "cauchit"
  } else if (grepl(pattern = "cloglog", x = x$link, fixed = TRUE)) {
    link <- "cloglog"
  } else {
    link <- "logit"
  }

  if (x$link == "Cumulative logit") {
    fam <- "ordinal"
  } else {
    fam <- "multinomial"
  }

  .retrieve_model_info(
    x = x,
    fitfam = fam,
    logit.link = link == "logit",
    link.fun = link,
    ...
  )
}


#' @export
model_info.BBreg <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "betabinomial",
    logit.link = TRUE,
    multi.var = FALSE,
    zero.inf = FALSE,
    link.fun = "logit",
    ...
  )
}

#' @export
model_info.BBmm <- model_info.BBreg


#' @export
model_info.mmrm <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    ...
  )
}

#' @export
model_info.mmrm_fit <- model_info.mmrm

#' @export
model_info.mmrm_tmb <- model_info.mmrm


#' @export
model_info.glmmadmb <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = x$family,
    logit.link = x$link == "logit",
    multi.var = FALSE,
    zero.inf = x$zeroInflation,
    link.fun = x$link,
    ...
  )
}


#' @export
model_info.glmgee <- function(x, ...) {
  faminfo <- x$family
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.cpglmm <- function(x, ...) {
  link <- parse(text = safe_deparse(x@call))[[1]]$link
  if (is.null(link)) link <- "log"
  if (is.numeric(link)) link <- "tweedie"
  .retrieve_model_info(
    x = x,
    fitfam = "poisson",
    logit.link = FALSE,
    multi.var = FALSE,
    link.fun = link,
    ...
  )
}


#' @export
model_info.zcpglm <- function(x, ...) {
  link <- parse(text = safe_deparse(x@call))[[1]]$link
  if (is.null(link)) link <- "log"
  if (is.numeric(link)) link <- "tweedie"
  .retrieve_model_info(
    x = x,
    fitfam = "poisson",
    logit.link = FALSE,
    multi.var = FALSE,
    link.fun = link,
    zero.inf = TRUE,
    ...
  )
}

#' @export
model_info.cpglm <- model_info.cpglmm

#' @export
model_info.bcplm <- model_info.cpglmm


#' @export
model_info.glimML <- function(x, ...) {
  fitfam <- switch(x@method,
    BB = "betabinomial",
    NB = "negative binomial"
  )
  .retrieve_model_info(
    x = x,
    fitfam = fitfam,
    logit.link = x@link == "logit",
    multi.var = FALSE,
    zero.inf = FALSE,
    link.fun = x@link,
    ...
  )
}


#' @export
model_info.gam <- function(x, ...) {
  if (!inherits(x, c("glm", "lm"))) {
    class(x) <- c(class(x), "glm", "lm")
  }

  faminfo <- .gam_family(x)
  link <- faminfo$link[1]
  is.mv <- faminfo$family == "Multivariate normal"

  if (is.mv) {
    link <- "identity"
  }

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = !is.null(link) && (link == "logit" || faminfo$family == "multinom"),
    link.fun = link,
    multi.var = is.mv,
    ...
  )
}


#' @export
model_info.vgam <- function(x, ...) {
  faminfo <- x@family
  link.fun <- faminfo@blurb[3]
  if (grepl("^(l|L)ogit", link.fun)) {
    link.fun <- "logit"
  }
  .retrieve_model_info(
    x = x,
    fitfam = faminfo@vfamily[1],
    logit.link = any(.string_contains("logit", faminfo@blurb)),
    link.fun = link.fun,
    multi.var = is_multivariate(x),
    ...
  )
}

#' @export
model_info.vglm <- model_info.vgam


#' @export
model_info.svy_vglm <- function(x, verbose = TRUE, ...) {
  model_info(x$fit, verbose = verbose)
}


#' @export
model_info.svy2lme <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(x = x, verbose = verbose, ...)
}


#' @export
model_info.glmmTMB <- function(x, ...) {
  check_if_installed("lme4")

  faminfo <- stats::family(x)
  zero_inflated <- !is_empty_object(lme4::fixef(x)$zi) || startsWith(faminfo$family, "truncated")

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    zero.inf = zero_inflated,
    hurdle = grepl("truncated", faminfo$family, fixed = TRUE),
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    dispersion = !is.null(find_formula(x, verbose = FALSE)$dispersion),
    glmmtmb_zeroinf = zero_inflated,
    ...
  )
}


#' @export
model_info.betareg <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "beta",
    logit.link = x$link$mean$name == "logit",
    link.fun = x$link$mean$name,
    ...
  )
}


#' @export
model_info.DirichletRegModel <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "dirichlet",
    logit.link = TRUE,
    link.fun = "logit",
    ...
  )
}


#' @export
model_info.gbm <- function(x, ...) {
  faminfo <- switch(x$distribution$name,
    laplace = ,
    tdist = ,
    gaussian = list(name = "gaussian", logit = FALSE, link = NULL),
    coxph = list(name = "survival", logit = TRUE, link = NULL),
    poisson = list(name = "poisson", logit = FALSE, link = "log"),
    huberized = ,
    adaboost = ,
    bernoulli = list(name = "binomial", logit = TRUE, link = "logit"),
  )

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$name,
    logit.link = faminfo$logit,
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.MCMCglmm <- function(x, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = x$Residual$family,
    logit.link = FALSE,
    link.fun = "",
    ...
  )
}


#' @export
model_info.polr <- function(x, ...) {
  link <- x$method
  if (link == "logistic") link <- "logit"
  faminfo <- stats::binomial(link = link)
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.nestedLogit <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = TRUE,
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.hglm <- function(x, ...) {
  faminfo <- .safe({
    mc <- get_call(x)$family
    eval(mc)
  })

  if (is.null(faminfo)) {
    return(NULL)
  }

  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.orm <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.svyolr <- function(x, ...) {
  l <- switch(x$method,
    logistic = "logit",
    x$method
  )
  faminfo <- stats::binomial(link = l)
  .retrieve_model_info(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.gamlss <- function(x, ...) {
  faminfo <- get(x$family[1], asNamespace("gamlss"))()
  # for ZIBNB family, we have only one value, so next line returns NA
  fitfam <- faminfo$family[2]
  if (is.na(fitfam)) {
    fitfam <- faminfo$family
    if (is.null(fitfam)) {
      fitfam <- "unknown"
    }
  }
  .retrieve_model_info(
    x = x,
    fitfam = fitfam,
    logit.link = faminfo$mu.link == "logit",
    link.fun = faminfo$mu.link,
    ...
  )
}


#' @export
model_info.mipo <- function(x, verbose = TRUE, ...) {
  .safe({
    models <- eval(x$call$object)
    model_info(models$analyses[[1]], verbose = verbose, ...)
  })
}


#' @export
model_info.mira <- function(x, ...) {
  model_info(x$analyses[[1]], ...)
}


# mfx models -------------------------------

#' @export
model_info.betamfx <- function(x, ...) {
  model_info.betareg(x$fit)
}

#' @export
model_info.betaor <- model_info.betamfx

#' @export
model_info.logitmfx <- function(x, ...) {
  model_info.default(x$fit, ...)
}

#' @export
model_info.poissonmfx <- model_info.logitmfx

#' @export
model_info.negbinmfx <- model_info.logitmfx

#' @export
model_info.probitmfx <- model_info.logitmfx

#' @export
model_info.logitor <- model_info.logitmfx

#' @export
model_info.poissonirr <- model_info.logitmfx

#' @export
model_info.negbinirr <- model_info.logitmfx


#' @export
model_info.bfsl <- function(x, verbose = TRUE, ...) {
  .retrieve_model_info(
    x = x,
    fitfam = "gaussian",
    logit.link = FALSE,
    link.fun = "identity",
    verbose = verbose,
    ...
  )
}

# marginaleffects objects -------------------------------

#' @export
model_info.marginaleffects <- function(x, ...) {
  model_info(attributes(x)$model)
}


# not yet supported -------------------------------

#' @export
model_info.earth <- function(x, ...) {
  format_error("Models of class `earth` are not yet supported.")
}

#' @export
model_info.deltaMethod <- function(x, ...) {
  NULL
}

#' @export
model_info.ggcomparisons <- function(x, ...) {
  NULL
}
