.make_family <- function(x, fitfam = "gaussian", zero.inf = FALSE, hurdle = FALSE, logit.link = FALSE, multi.var = FALSE, link.fun = "identity", dispersion = FALSE, ...) {
  # create logical for family

  # binomial family --------

  binom_fam <-
    fitfam %in% c("bernoulli", "binomial", "quasibinomial", "binomialff") |
      grepl("\\Qbinomial\\E", fitfam, ignore.case = TRUE)


  # poisson family --------

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson", "genpois", "ziplss") |
      grepl("\\Qpoisson\\E", fitfam, ignore.case = TRUE)


  # negative binomial family --------

  neg_bin_fam <-
    grepl("\\Qnegative binomial\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnbinom\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnegbin\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnzbinom\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qgenpois\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnegbinomial\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE) |
      fitfam %in% c("ztnbinom", "nbinom")


  # beta family --------

  beta_fam <-
    inherits(x, c("betareg", "betamfx")) |
      fitfam %in% c(
        "beta",
        "Beta",
        "betabinomial",
        "Beta Inflated",
        "Zero Inflated Beta",
        "Beta Inflated zero",
        "Beta Inflated one"
      )


  # special families (beta-binomial, dirichlet) --------

  betabin_fam <- inherits(x, "BBreg") | fitfam %in% "betabinomial"
  dirichlet_fam <- inherits(x, "DirichletRegModel") | fitfam %in% "dirichlet"

  ## TODO beta-binomial = binomial?
  if (betabin_fam) binom_fam <- TRUE


  # exponential family --------

  exponential_fam <- fitfam %in% c("Gamma", "gamma", "weibull")


  # zero-inflated or hurdle component --------

  zero.inf <- zero.inf | fitfam == "ziplss" |
    grepl("\\Qzero_inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qzero-inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qhurdle\\E", fitfam, ignore.case = TRUE) |
    grepl("^(zt|zi|za|hu)", fitfam, perl = TRUE) |
    grepl("^truncated", fitfam, perl = TRUE)


  # only hurdle component --------

  hurdle <- hurdle |
    grepl("\\Qhurdle\\E", fitfam, ignore.case = TRUE) |
    grepl("^hu", fitfam, perl = TRUE) |
    grepl("^truncated", fitfam, perl = TRUE) |
    fitfam == "ztnbinom" |
    fitfam %in% c("truncpoiss", "truncnbinom", "truncnbinom1", "truncpoisson")


  # ordinal family --------

  is.ordinal <-
    inherits(x, c("svyolr", "polr", "clm", "clm2", "clmm", "mixor", "LORgee")) |
      fitfam %in% c("cumulative", "ordinal")


  # multinomial family --------

  is.multinomial <-
    inherits(x, c("gmnl", "mlogit", "DirichletRegModel", "multinom", "brmultinom")) |
      fitfam %in% c("cratio", "sratio", "acat", "multinomial", "multinomial2", "dirichlet")


  # categorical family --------

  is.categorical <- fitfam == "categorical"


  # Bayesian model --------

  is.bayes <- inherits(x, c(
    "brmsfit", "stanfit", "MCMCglmm", "stanreg",
    "stanmvreg", "bmerMod", "BFBayesFactor", "bamlss",
    "bayesx", "mcmc", "bcplm", "bayesQR", "BGGM"
  ))


  # survival model --------

  is.survival <- inherits(x, c("aareg", "survreg", "survfit", "survPresmooth", "flexsurvreg", "coxph", "coxme"))

  # check if we have binomial models with trials instead of binary outcome
  # and check if we have truncated or censored brms-regression


  # censored or truncated response --------

  is.trial <- FALSE
  is.censored <- inherits(x, c("crq", "crqs")) | all(inherits(x, c("sem", "lme")))
  is.truncated <- FALSE

  if (inherits(x, "brmsfit") && is.null(stats::formula(x)$responses)) {
    rv <- tryCatch(
      {
        .safe_deparse(stats::formula(x)$formula[[2L]])
      },
      error = function(x) {
        NULL
      }
    )

    if (!is.null(rv)) {
      is.trial <- .trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
      is.censored <- grepl("(.*)\\|(.*)cens\\(", rv)
      is.truncated <- grepl("(.*)\\|(.*)trunc\\(", rv)
    }
  }

  if (binom_fam && !inherits(x, "brmsfit")) {
    is.trial <- tryCatch(
      {
        rv <- .safe_deparse(stats::formula(x)[[2L]])
        grepl("cbind\\((.*)\\)", rv)
      },
      error = function(x) {
        FALSE
      }
    )
  }


  # save model terms --------

  dots <- list(...)
  if (.obj_has_name(dots, "no_terms") && isTRUE(dots$no_terms)) {
    model_terms <- NULL
  } else {
    if (inherits(x, "mcmc")) {
      model_terms <- find_parameters(x)
    } else {
      model_terms <- tryCatch(
        {
          find_variables(x, effects = "all", component = "all", flatten = FALSE)
        },
        error = function(x) {
          NULL
        }
      )
    }
  }


  # significance tests --------

  is_ttest <- FALSE
  is_correlation <- FALSE
  is_oneway <- FALSE

  if (inherits(x, "htest")) {
    if (grepl("t-test", x$method)) {
      is_ttest <- TRUE
    } else if (grepl("^One-way", x$method)) {
      is_oneway <- TRUE
    } else {
      is_correlation <- TRUE
    }
  } else if (inherits(x, "BGGM")) {
    is_correlation <- TRUE
  }


  # Bayesfactors terms --------

  is_meta <- FALSE
  if (inherits(x, "BFBayesFactor")) {
    is_ttest <- FALSE
    is_correlation <- FALSE
    is_oneway <- FALSE

    obj_type <- .classify_BFBayesFactor(x)

    if (obj_type == "correlation") {
      is_correlation <- TRUE
    } else if (obj_type %in% c("ttest1", "ttest2")) {
      is_ttest <- TRUE
    } else if (obj_type == "meta") {
      is_meta <- TRUE
    }
  }


  # meta analysis --------

  if (!is_meta) {
    is_meta <- inherits(x, c("rma", "metaplus", "meta_random", "meta_fixed", "meta_bma"))
  }

  if (inherits(x, "brmsfit") && !is_multivariate(x)) {
    is_meta <- grepl("(.*)\\|(.*)se\\((.*)\\)", .safe_deparse(find_formula(x)$conditional[[2]]))
  }


  # gaussian family --------

  linear_model <- (!binom_fam & !exponential_fam & !poisson_fam & !neg_bin_fam & !logit.link & !dirichlet_fam
                   & !is.ordinal & !zero.inf & !is.censored & !is.survival & !is.categorical & !hurdle & !is.multinomial) ||
    fitfam %in% c("Student's-t", "t Family", "gaussian", "Gaussian") || grepl("(\\st)$", fitfam)


  # tweedie family --------

  tweedie_fam <- grepl("^(tweedie|Tweedie)", fitfam) | grepl("^(tweedie|Tweedie)", link.fun)
  tweedie_model <- (linear_model && tweedie_fam) || inherits(x, c("bcplm", "cpglm", "cpglmm", "zcpglm"))


  # return...

  list(
    is_binomial = binom_fam & !neg_bin_fam,
    is_count = poisson_fam | neg_bin_fam,
    is_poisson = poisson_fam,
    is_negbin = neg_bin_fam,
    is_beta = beta_fam,
    is_betabinomial = betabin_fam,
    is_dirichlet = dirichlet_fam,
    is_exponential = exponential_fam,
    is_logit = logit.link,
    is_probit = isTRUE(link.fun == "probit"),
    is_censored = inherits(x, c("tobit", "crch", "censReg")) | is.censored | is.survival,
    is_truncated = inherits(x, "truncreg") | is.truncated,
    is_survival = is.survival,
    is_linear = linear_model,
    is_tweedie = tweedie_model,
    is_zeroinf = zero.inf,
    is_zero_inflated = zero.inf,
    is_dispersion = dispersion,
    is_hurdle = hurdle,
    is_ordinal = is.ordinal,
    is_cumulative = is.ordinal,
    is_multinomial = is.multinomial | is.categorical,
    is_categorical = is.categorical,
    is_mixed = .is_mixed_model(x),
    is_multivariate = multi.var,
    is_trial = is.trial,
    is_bayesian = is.bayes,
    is_anova = inherits(x, c("aov", "aovlist", "MANOVA", "RM")),
    is_timeseries = inherits(x, c("Arima")),
    is_ttest = is_ttest,
    is_correlation = is_correlation,
    is_oneway = is_oneway,
    is_meta = is_meta,
    link_function = link.fun,
    family = fitfam,
    n_obs = n_obs(x),
    model_terms = model_terms
  )
}

.get_ordinal_link <- function(x) {
  switch(
    x$link,
    logistic = "logit",
    cloglog = "log",
    x$link
  )
}


#' @importFrom stats gaussian binomial Gamma
.make_tobit_family <- function(x, dist = NULL) {
  if (is.null(dist)) {
    if (inherits(x, "flexsurvreg")) {
      dist <- parse(text = .safe_deparse(x$call))[[1]]$dist
    } else {
      dist <- x$dist
    }
  }
  f <- switch(
    dist,
    gaussian = stats::gaussian("identity"),
    logistic = stats::binomial("logit"),
    llogis = ,
    loglogistic = stats::binomial("log"),
    lnorm = ,
    lognormal = stats::gaussian("log"),
    gompertz = stats::Gamma("log"),
    gamma = ,
    gengamma = ,
    gengamma.orig = stats::Gamma(),
    exponential = ,
    exp = ,
    weibull = stats::Gamma("log"),
    stats::gaussian("identity")
  )

  if (dist == "weibull") f$family <- "weibull"
  f
}


.classify_BFBayesFactor <- function(x) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function needs `BayesFactor` to be installed.")
  }

  if (any(class(x@denominator) %in% c("BFcorrelation"))) {
    "correlation"
  } else if (any(class(x@denominator) %in% c("BFoneSample"))) {
    "ttest1"
  } else if (any(class(x@denominator) %in% c("BFindepSample"))) {
    "ttest2"
  } else if (any(class(x@denominator) %in% c("BFmetat"))) {
    "meta"
  } else if (any(class(x@denominator) %in% c("BFlinearModel"))) {
    "linear"
  } else if (any(class(x@denominator) %in% c("BFcontingencyTable"))) {
    "xtable"
  } else {
    class(x@denominator)
  }
}



.is_mixed_model <- function(x) {
  !is.null(find_random(x))
}



.is_semLme <- function(x) {
  all(inherits(x, c("sem", "lme")))
}
