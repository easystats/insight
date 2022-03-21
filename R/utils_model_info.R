.make_family <- function(x,
                         fitfam = "gaussian",
                         zero.inf = FALSE,
                         hurdle = FALSE,
                         logit.link = FALSE,
                         multi.var = FALSE,
                         link.fun = "identity",
                         dispersion = FALSE,
                         verbose = TRUE,
                         glmmtmb_zeroinf = FALSE, # needed for edge cases
                         ...) {
  dots <- list(...)
  if (isTRUE(dots$return_family_only)) {
    return(list(family = fitfam, link_function = link.fun))
  }

  fitfam_lower <- tolower(fitfam)

  # create logical for family

  # binomial family --------

  binom_fam <-
    fitfam_lower %in% c("bernoulli", "binomial", "quasibinomial", "binomialff") |
      grepl("binomial", fitfam_lower, fixed = TRUE)


  # poisson family --------

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson", "genpois", "ziplss") |
      grepl("poisson", fitfam_lower, fixed = TRUE)


  # negative binomial family --------

  neg_bin_fam <-
    grepl("negative binomial", fitfam_lower, fixed = TRUE) |
      grepl("nbinom", fitfam_lower, fixed = TRUE) |
      grepl("negbin", fitfam_lower, fixed = TRUE) |
      grepl("nzbinom", fitfam_lower, fixed = TRUE) |
      grepl("genpois", fitfam_lower, fixed = TRUE) |
      grepl("negbinomial", fitfam_lower, fixed = TRUE) |
      grepl("neg_binomial", fitfam_lower, fixed = TRUE) |
      fitfam_lower %in% c("ztnbinom", "nbinom")


  # bernoulli family --------

  is_bernoulli <- FALSE

  if (binom_fam && inherits(x, "glm") && !neg_bin_fam && !poisson_fam) {
    resp <- stats::model.response(stats::model.frame(x))
    if ((is.data.frame(resp) || is.matrix(resp)) && ncol(resp) == 1) {
      resp <- as.vector(resp[[1]])
    }
    if (!is.data.frame(resp) && !is.matrix(resp) && all(.is.int(.factor_to_numeric(resp[[1]])))) {
      is_bernoulli <- TRUE
    }
  } else if (fitfam %in% "bernoulli") {
    is_bernoulli <- TRUE
  }


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

  zero.inf <- zero.inf | fitfam_lower == "ziplss" |
    grepl("zero_inflated", fitfam_lower, fixed = TRUE) |
    grepl("zero-inflated", fitfam_lower, fixed = TRUE) |
    grepl("neg_binomial", fitfam_lower, fixed = TRUE) |
    grepl("hurdle", fitfam_lower, fixed = TRUE) |
    grepl("^(zt|zi|za|hu)", fitfam_lower, perl = TRUE) |
    grepl("truncated", fitfam_lower, fixed = TRUE)

  # overwrite for glmmTMB exceptions
  if (inherits(x, "glmmTMB")) {
    zero.inf <- glmmtmb_zeroinf
  }


  # only hurdle component --------

  hurdle <- hurdle |
    grepl("hurdle", fitfam_lower, fixed = TRUE) |
    grepl("^hu", fitfam, perl = TRUE) |
    grepl("truncated", fitfam, fixed = TRUE) |
    fitfam == "ztnbinom" |
    fitfam %in% c("truncpoiss", "truncnbinom", "truncnbinom1", "truncpoisson")


  # ordinal family --------

  is.ordinal <-
    inherits(x, c("svyolr", "polr", "clm", "clm2", "clmm", "mixor", "LORgee", "mvord")) |
      fitfam %in% c("cumulative", "ordinal")


  # multinomial family --------

  is.multinomial <-
    inherits(x, c("gmnl", "mlogit", "DirichletRegModel", "multinom", "brmultinom")) |
      fitfam %in% c("cratio", "sratio", "acat", "multinom", "multinomial", "multinomial2", "dirichlet")


  # categorical family --------

  is.categorical <- fitfam == "categorical"


  # special handling of rms --------------

  if (inherits(x, c("lrm", "blrm"))) {
    resp <- get_response(x, verbose = FALSE)
    if (n_unique(resp) == 2) {
      binom_fam <- TRUE
    } else {
      is.ordinal <- TRUE
    }
  }


  # Bayesian model --------

  is.bayes <- .is_bayesian_model(x)


  # survival model --------

  is.survival <- inherits(
    x,
    c(
      "aareg",
      "survreg",
      "survfit",
      "survPresmooth",
      "flexsurvreg",
      "coxph",
      "coxme",
      "coxr",
      "riskRegression",
      "comprisk"
    )
  )

  # check if we have binomial models with trials instead of binary outcome
  # and check if we have truncated or censored brms-regression


  # censored or truncated response --------

  is.trial <- FALSE
  is.censored <- inherits(x, c("tobit", "crch", "censReg", "crq", "crqs")) | (inherits(x, "sem") && inherits(x, "lme"))
  is.truncated <- FALSE

  if (inherits(x, "brmsfit") && is.null(stats::formula(x)$responses)) {
    rv <- tryCatch(
      {
        safe_deparse(stats::formula(x)$formula[[2L]])
      },
      error = function(x) {
        NULL
      }
    )

    if (!is.null(rv)) {
      is.trial <- trim_ws(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
      is.censored <- grepl("(.*)\\|(.*)cens\\(", rv)
      is.truncated <- grepl("(.*)\\|(.*)trunc\\(", rv)
    }
  }

  if (binom_fam && !inherits(x, "brmsfit")) {
    is.trial <- tryCatch(
      {
        rv <- safe_deparse(stats::formula(x)[[2L]])
        grepl("cbind\\((.*)\\)", rv)
      },
      error = function(x) {
        FALSE
      }
    )
  }


  # save model terms --------

  # drop this for now...

  # if (isTRUE(dots$no_terms)) {
  #   model_terms <- NULL
  # } else {
  #   if (inherits(x, "mcmc")) {
  #     model_terms <- find_parameters(x)
  #   } else {
  #     model_terms <- tryCatch(
  #       {
  #         find_variables(
  #           x,
  #           effects = "all",
  #           component = "all",
  #           flatten = FALSE,
  #           verbose = FALSE
  #         )
  #       },
  #       error = function(x) {
  #         NULL
  #       }
  #     )
  #   }
  # }


  # significance tests --------

  is_ftest <- FALSE
  is_ttest <- FALSE
  is_correlation <- FALSE
  is_oneway <- FALSE
  is_proptest <- FALSE
  is_binomtest <- FALSE
  is_chi2test <- FALSE
  is_ranktest <- FALSE
  is_xtab <- FALSE
  is_levenetest <- FALSE

  if (inherits(x, "htest")) {
    if (grepl("kruskal-wallis", tolower(x$method), fixed = TRUE) ||
      grepl("design-based kruskalwallis", tolower(x$method), fixed = TRUE) ||
      grepl("design-based median", tolower(x$method), fixed = TRUE) ||
      grepl("design-based vanderwaerden", tolower(x$method), fixed = TRUE) ||
      grepl("wilcoxon", tolower(x$method), fixed = TRUE) ||
      grepl("friedman", tolower(x$method), fixed = TRUE)) {
      is_ranktest <- TRUE
    } else if (grepl("t-test", x$method, fixed = TRUE)) {
      is_ttest <- TRUE
    } else if (grepl("F test", x$method, fixed = TRUE)) {
      is_ftest <- TRUE
    } else if (grepl("^One-way", x$method)) {
      is_oneway <- TRUE
    } else if (x$method == "Exact binomial test") {
      binom_fam <- TRUE
      is_binomtest <- TRUE
      fitfam <- "binomial"
    } else if (grepl("\\d+-sample(.*)proportions(.*)", x$method)) {
      binom_fam <- TRUE
      is_proptest <- TRUE
      fitfam <- "binomial"
    } else if (any(grepl("chi-squared", c(tolower(x$method), tolower(attributes(x$statistic)$names)), fixed = TRUE)) ||
      grepl("Fisher's Exact Test", x$method, fixed = TRUE) ||
      grepl("pearson's x^2", tolower(x$method), fixed = TRUE)) {
      is_chi2test <- TRUE
      is_xtab <- TRUE
      fitfam <- "categorical"
    } else {
      is_correlation <- TRUE
      if (grepl("Spearman's rank", x$method, fixed = TRUE)) {
        is_ranktest <- TRUE
      }
    }
  } else if (inherits(x, "BGGM")) {
    is_correlation <- TRUE
  }

  # exceptions: car::leveneTest
  if (inherits(x, "anova") && !is.null(attributes(x)$heading) && grepl("Levene's Test", attributes(x)$heading, fixed = TRUE)) {
    is_levenetest <- TRUE
  }

  # Bayesfactors terms --------

  is_meta <- FALSE
  if (inherits(x, "BFBayesFactor")) {
    is_ftest <- FALSE
    is_ttest <- FALSE
    is_correlation <- FALSE
    is_oneway <- FALSE
    is_proptest <- FALSE
    is_xtab <- FALSE

    obj_type <- .classify_BFBayesFactor(x)

    if (obj_type == "correlation") {
      is_correlation <- TRUE
    } else if (obj_type %in% c("ttest1", "ttest2")) {
      is_ttest <- TRUE
    } else if (obj_type == "meta") {
      is_meta <- TRUE
    } else if (obj_type == "proptest") {
      binom_fam <- TRUE
      is_proptest <- TRUE
      fitfam <- "binomial"
    } else if (obj_type == "xtable") {
      is_xtab <- TRUE
      fitfam <- "categorical"
    }
  }


  # meta analysis --------

  if (!is_meta) {
    is_meta <- inherits(x, c("rma", "metaplus", "meta_random", "meta_fixed", "meta_bma"))
  }

  if (inherits(x, "brmsfit") && !is_multivariate(x)) {
    is_meta <- grepl("(.*)\\|(.*)se\\((.*)\\)", safe_deparse(find_formula(x, verbose = FALSE)$conditional[[2]]))
  }


  # gaussian family --------

  linear_model <- TRUE
  if (binom_fam | exponential_fam | poisson_fam | neg_bin_fam | logit.link |
    dirichlet_fam | is.ordinal | zero.inf | is.censored | is.survival | is_binomtest |
    is.categorical | hurdle | is.multinomial | is_chi2test | is_proptest | is_xtab) {
    linear_model <- FALSE
  } else if (!(fitfam %in% c("Student's-t", "t Family", "gaussian", "Gaussian")) && !grepl("(\\st)$", fitfam)) {
    linear_model <- FALSE
  }
  if (!linear_model && is.survival && fitfam == "gaussian") {
    linear_model <- TRUE
  }


  # tweedie family --------

  tweedie_fam <- grepl("tweedie", fitfam_lower, fixed = TRUE) | grepl("tweedie", tolower(link.fun), fixed = TRUE)
  tweedie_model <- tweedie_fam | inherits(x, c("bcplm", "cpglm", "cpglmm", "zcpglm"))


  # return...

  list(
    is_binomial = binom_fam & !neg_bin_fam,
    is_bernoulli = is_bernoulli,
    is_count = poisson_fam | neg_bin_fam,
    is_poisson = poisson_fam,
    is_negbin = neg_bin_fam,
    is_beta = beta_fam,
    is_betabinomial = betabin_fam,
    is_dirichlet = dirichlet_fam,
    is_exponential = exponential_fam,
    is_logit = logit.link,
    is_probit = isTRUE(link.fun == "probit"),
    is_censored = is.censored | is.survival,
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
    is_mixed = !is_levenetest && is_mixed_model(x),
    is_multivariate = multi.var,
    is_trial = is.trial,
    is_bayesian = is.bayes,
    is_gam = is_gam_model(x),
    is_anova = inherits(x, c("aov", "aovlist", "MANOVA", "RM")),
    is_timeseries = inherits(x, c("Arima")),
    is_ttest = is_ttest,
    is_correlation = is_correlation,
    is_onewaytest = is_oneway,
    is_chi2test = is_chi2test,
    is_ranktest = is_ranktest,
    is_levenetest = is_levenetest,
    is_xtab = is_xtab,
    is_proptest = is_proptest,
    is_binomtest = is_binomtest,
    is_ftest = is_ftest,
    is_meta = is_meta,
    link_function = link.fun,
    family = fitfam,
    n_obs = n_obs(x)
  )
}

.get_ordinal_link <- function(x) {
  switch(x$link,
    logistic = "logit",
    cloglog = "log",
    x$link
  )
}


.make_tobit_family <- function(x, dist = NULL) {
  if (is.null(dist)) {
    if (inherits(x, "flexsurvreg")) {
      dist <- parse(text = safe_deparse(x$call))[[1]]$dist
    } else {
      dist <- x$dist
    }
  }
  f <- switch(dist,
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
  # installed?
  check_if_installed("BayesFactor")

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
  } else if (any(class(x@denominator) %in% c("BFproportion"))) {
    "proptest"
  } else {
    class(x@denominator)
  }
}



.is_semLme <- function(x) {
  all(inherits(x, c("sem", "lme")))
}
