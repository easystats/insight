#' @title Access information from model objects
#' @name model_info
#'
#' @description Retrieve information from model objects.
#'
#' @inheritParams find_predictors
#' @inheritParams link_inverse
#' @inheritParams find_formula
#'
#' @return A list with information about the model, like family, link-function
#'   etc. (see 'Details').
#'
#' @details \code{model_info()} returns a list with information about the
#'   model for many different model objects. Following information
#'    is returned, where all values starting with \code{is_} are logicals.
#'    \itemize{
#'      \item \code{is_binomial}: family is binomial (but not negative binomial)
#'      \item \code{is_poisson}: family is poisson
#'      \item \code{is_negbin}: family is negative binomial
#'      \item \code{is_count}: model is a count model (i.e. family is either poisson or negative binomial)
#'      \item \code{is_beta}: family is beta
#'      \item \code{is_exponential}: family is exponential (e.g. Gamma or Weibull)
#'      \item \code{is_logit}: model has logit link
#'      \item \code{is_progit}: model has probit link
#'      \item \code{is_linear}: family is gaussian
#'      \item \code{is_tweedie}: family is tweedie
#'      \item \code{is_ordinal}: family is ordinal or cumulative link
#'      \item \code{is_categorical}: family is categorical link
#'      \item \code{is_censored}: model is a censored model
#'      \item \code{is_zeroinf}: model has zero-inflation component
#'      \item \code{is_zero_inflated}: alias for \code{is_zeroinf}
#'      \item \code{is_mixed}: model is a mixed effects model (with random effects)
#'      \item \code{is_multivariate}: model is a multivariate response model (currently only works for \emph{brmsfit} objects)
#'      \item \code{is_trial}: model response contains additional information about the trials
#'      \item \code{is_bayesian}: model is a Bayesian model
#'      \item \code{is_anova}: model is an Anova object
#'      \item \code{link_function}: the link-function
#'      \item \code{family}: the family-object
#'      \item \code{n_obs}: number of observations
#'      \item \code{model_terms}: a list with all model terms, including terms such as random effects or from zero-inflated model parts.
#'    }
#'
#' @examples
#' library(glmmTMB)
#' data("Salamanders")
#' m <- glmmTMB(
#'   count ~ spp + cover + mined + (1 | site),
#'   ziformula = ~ spp + mined,
#'   dispformula = ~DOY,
#'   data = Salamanders,
#'   family = nbinom2
#' )
#'
#' model_info(m)
#' @importFrom stats formula terms
#' @export
model_info <- function(x, ...) {
  UseMethod("model_info")
}


#' @export
model_info.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}


#' @importFrom stats family
#' @export
model_info.default <- function(x, ...) {
  if (inherits(x, "list") && obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  faminfo <- tryCatch(
    {
      if (inherits(x, c("Zelig-relogit"))) {
        stats::binomial(link = "logit")
      } else {
        stats::family(x)
      }
    },
    error = function(x) { NULL }
  )

  if (!is.null(faminfo)) {
    make_family(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      link.fun = faminfo$link,
      ...
    )
  } else {
    warning("Could not access model information.", call. = FALSE)
  }
}


#' @export
model_info.glmmPQL <- function(x, ...) {
  faminfo <- x$family
  make_family(
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
model_info.MixMod <- function(x, ...) {
  faminfo <- x$family
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.tobit <- function(x, ...) {
  faminfo <- .make_tobit_family(x)

  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.crch <- function(x, ...) {
  faminfo <- .make_tobit_family(x)

  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.survreg <- function(x, ...) {
  faminfo <- .make_tobit_family(x)

  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.htest <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.lme <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.rq <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.crq <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.rqss <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.mixed <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.plm <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.gls <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.truncreg <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.lmRob <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.lmrob <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.gam <- function(x, ...) {
  if (!inherits(x, c("glm", "lm"))) {
    class(x) <- c(class(x), "glm", "lm")
  }

  faminfo <- stats::family(x)
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.vgam <- function(x, ...) {
  faminfo <- x@family
  link.fun <- faminfo@blurb[3]
  if (grepl("^\\Qlogit(\\E", link.fun, perl = TRUE)) link.fun <- "logit"
  make_family(
    x = x,
    fitfam = faminfo@vfamily[1],
    logit.link = any(string_contains("logit", faminfo@blurb)),
    link.fun = link.fun,
    ...
  )
}


#' @export
model_info.vglm <- function(x, ...) {
  faminfo <- x@family
  link.fun <- faminfo@blurb[3]
  if (grepl("^\\Qlogit(\\E", link.fun, perl = TRUE)) link.fun <- "logit"
  make_family(
    x = x,
    fitfam = faminfo@vfamily[1],
    logit.link = any(string_contains("logit", faminfo@blurb)),
    link.fun = link.fun,
    ...
  )
}


#' @export
model_info.zeroinfl <- function(x, ...) {
  if (is.list(x$dist)) {
    dist <- x$dist[[1]]
  } else {
    dist <- x$dist
  }
  fitfam <- switch(
    dist,
    poisson = "poisson",
    negbin = "negative binomial",
    "poisson"
  )
  make_family(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    link.fun = "log",
    ...
  )
}


#' @export
model_info.hurdle <- function(x, ...) {
  if (is.list(x$dist)) {
    dist <- x$dist[[1]]
  } else {
    dist <- x$dist
  }
  fitfam <- switch(
    dist,
    poisson = "poisson",
    negbin = "negative binomial",
    "poisson"
  )
  make_family(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    link.fun = "log",
    ...
  )
}


#' @export
model_info.zerotrunc <- function(x, ...) {
  if (is.list(x$dist)) {
    dist <- x$dist[[1]]
  } else {
    dist <- x$dist
  }
  fitfam <- switch(
    dist,
    poisson = "poisson",
    negbin = "negative binomial",
    "poisson"
  )
  make_family(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    link.fun = "log",
    ...
  )
}


#' @export
model_info.glmmTMB <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  faminfo <- stats::family(x)
  make_family(
    x = x,
    fitfam = faminfo$family,
    zero.inf = !is_empty_object(lme4::fixef(x)$zi),
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.lm_robust <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.iv_robust <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.felm <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.ivreg <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.betareg <- function(x, ...) {
  make_family(
    x = x,
    fitfam = "beta",
    logit.link = x$link$mean$name == "logit",
    link.fun = x$link$mean$name,
    ...
  )
}


#' @export
model_info.coxph <- function(x, ...) {
  make_family(
    x = x,
    fitfam = "survival",
    logit.link = TRUE,
    link.fun = NULL,
    ...
  )
}


#' @export
model_info.coxme <- function(x, ...) {
  make_family(
    x = x,
    fitfam = "survival",
    logit.link = TRUE,
    link.fun = NULL,
    ...
  )
}


#' @export
model_info.MCMCglmm <- function(x, ...) {
  make_family(
    x = x,
    fitfam = x$Residual$family,
    logit.link = FALSE,
    link.fun = "",
    ...
  )
}


#' @export
model_info.lrm <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.polr <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.multinom <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
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
  make_family(
    x = x,
    fitfam = faminfo$family[2],
    logit.link = faminfo$mu.link == "logit",
    link.fun = faminfo$mu.link,
    ...
  )
}


#' @export
model_info.clm2 <- function(x, ...) {
  faminfo <- stats::binomial(link = get_ordinal_link(x))
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.clm <- function(x, ...) {
  faminfo <- stats::binomial(link = get_ordinal_link(x))
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.clmm <- function(x, ...) {
  faminfo <- stats::binomial(link = get_ordinal_link(x))
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.mlogit <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.logistf <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.gmnl <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.brmsfit <- function(x, ...) {
  faminfo <- stats::family(x)
  if (is_multivariate(x)) {
    lapply(faminfo, function(.x) {
      make_family(
        x = x,
        fitfam = .x$family,
        zero.inf = FALSE,
        logit.link = .x$link == "logit",
        multi.var = TRUE,
        link.fun = .x$link,
        ...
      )
    })
  } else {
    make_family(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      multi.var = FALSE,
      link.fun = faminfo$link,
      ...
    )
  }
}


#' @export
model_info.stanmvreg <- function(x, ...) {
  faminfo <- stats::family(x)
  lapply(faminfo, function(.x) {
    make_family(
      x = x,
      fitfam = .x$family,
      zero.inf = FALSE,
      logit.link = .x$link == "logit",
      multi.var = TRUE,
      link.fun = .x$link,
      ...
    )
  })
}


#' @export
model_info.aovlist <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.mlm <- function(x, ...) {
  make_family(x, multi.var = TRUE, ...)
}


#' @keywords internal
make_family <- function(x, fitfam = "gaussian", zero.inf = FALSE, logit.link = FALSE, multi.var = FALSE, link.fun = "identity", ...) {
  # create logical for family
  binom_fam <-
    fitfam %in% c("bernoulli", "binomial", "quasibinomial", "binomialff") |
      grepl("\\Qbinomial\\E", fitfam, ignore.case = TRUE)

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson", "genpois", "ziplss") |
      grepl("\\Qpoisson\\E", fitfam, ignore.case = TRUE)

  neg_bin_fam <-
    grepl("\\Qnegative binomial\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnbinom\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnzbinom\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qgenpois\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qnegbinomial\\E", fitfam, ignore.case = TRUE) |
      grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE)

  beta_fam <- inherits(x, "betareg") | fitfam %in% c("beta")

  exponential_fam <- fitfam %in% c("Gamma", "weibull")

  linear_model <- (!binom_fam & !exponential_fam & !poisson_fam & !neg_bin_fam & !logit.link) ||
    fitfam %in% c("Student's-t", "t Family") || grepl("(\\st)$", fitfam)

  tweedie_model <- linear_model && grepl("tweedie", fitfam, fixed = TRUE)

  zero.inf <- zero.inf | fitfam == "ziplss" |
    grepl("\\Qzero_inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qzero-inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qhurdle\\E", fitfam, ignore.case = TRUE) |
    grepl("^(zt|zi|za|hu)", fitfam, perl = TRUE) |
    grepl("^truncated", fitfam, perl = TRUE)

  is.ordinal <-
    inherits(x, c("polr", "clm", "clm2", "clmm", "gmnl", "mlogit", "multinom")) |
      fitfam %in% c("cumulative", "cratio", "sratio", "acat", "ordinal", "multinomial")

  is.categorical <- fitfam == "categorical"


  # check if we have binomial models with trials instead of binary outcome

  is.trial <- FALSE

  if (inherits(x, "brmsfit") && is.null(stats::formula(x)$responses)) {
    is.trial <- tryCatch({
      rv <- deparse(stats::formula(x)$formula[[2L]], width.cutoff = 500L)
      trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
    },
    error = function(x) {
      FALSE
    }
    )
  }

  if (binom_fam && !inherits(x, "brmsfit")) {
    is.trial <- tryCatch({
      rv <- deparse(stats::formula(x)[[2L]], width.cutoff = 500L)
      grepl("cbind\\((.*)\\)", rv)
    },
    error = function(x) {
      FALSE
    }
    )
  }

  dots <- list(...)
  if (obj_has_name(dots, "no_terms") && isTRUE(dots$no_terms)) {
    model_terms <- NULL
  } else {
    model_terms <- tryCatch({
      find_terms(x, effects = "all", component = "all", flatten = FALSE)
    },
    error = function(x) {
      NULL
    }
    )
  }

  if (inherits(x, "htest")) {
    if (grepl("t-test", x$method)) {
      is_ttest <- TRUE
      is_correlation <- FALSE
    } else {
      is_ttest <- FALSE
      is_correlation <- TRUE
    }
  } else {
    is_ttest <- FALSE
    is_correlation <- FALSE
  }


  list(
    is_binomial = binom_fam & !neg_bin_fam,
    is_count = poisson_fam | neg_bin_fam,
    is_poisson = poisson_fam,
    is_negbin = neg_bin_fam,
    is_beta = beta_fam,
    is_exponential = exponential_fam,
    is_logit = logit.link,
    is_probit = link.fun == "probit",
    is_censored = inherits(x, c("tobit", "crch")),
    is_linear = linear_model,
    is_tweedie = tweedie_model,
    is_zeroinf = zero.inf,
    is_zero_inflated = zero.inf,
    is_ordinal = is.ordinal,
    is_categorical = is.categorical,
    is_mixed = !is.null(find_random(x)),
    is_multivariate = multi.var,
    is_trial = is.trial,
    is_bayesian = inherits(x, c("brmsfit", "stanfit", "stanreg", "stanmvreg", "bmerMod")),
    is_anova = inherits(x, c("aov", "aovlist")),
    is_ttest = is_ttest,
    is_correlation = is_correlation,
    link_function = link.fun,
    family = fitfam,
    n_obs = n_obs(x),
    model_terms = model_terms
  )
}

get_ordinal_link <- function(x) {
  switch(
    x$link,
    logistic = "logit",
    cloglog = "log",
    x$link
  )
}


#' @importFrom stats gaussian binomial Gamma
.make_tobit_family <- function(x) {
  f <- switch(
    x$dist,
    gaussian = stats::gaussian("identity"),
    logistic = stats::binomial("logit"),
    loglogistic = stats::binomial("log"),
    lognormal = stats::gaussian("log"),
    exponential = ,
    weibull = stats::Gamma("log"),
    stats::gaussian("identity")
  )

  if (x$dist == "weibull") f$family <- "weibull"
  f
}













#' @keywords internal
.classify_BFBayesFactor <- function(x){
  if (!requireNamespace("BayesFactor")) {
    stop("This function needs `BayesFactor` to be installed.")
  }

  if (any(class(x@denominator) %in% c("BFcorrelation"))) {
    return("correlation")
  } else if (any(class(x@denominator) %in% c("BFoneSample", "BFindepSample"))) {
    return("ttest")
  } else if (any(class(x@denominator) %in% c("BFmetat"))) {
    return("meta")
  } else if (any(class(x@denominator) %in% c("BFlinearModel"))) {
    return("linear")
  } else{
    return(class(x@denominator))
  }

}