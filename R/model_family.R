#' @rdname pred_vars
#' @importFrom sjmisc str_contains is_empty
#' @importFrom stats family formula gaussian binomial
#' @importFrom lme4 fixef
#' @export
model_family <- function(x, multi.resp = FALSE, mv = FALSE) {
  zero.inf <- FALSE
  multi.var <- FALSE

  if (!missing(multi.resp)) mv <- multi.resp

  # for gam-components from gamm4, add class attributes, so family
  # function works correctly
  if (inherits(x, "gam") && !inherits(x, c("glm", "lm")))
    class(x) <- c(class(x), "glm", "lm")

  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (inherits(x, c("glmmPQL", "MixMod"))) {
    faminfo <- x$family
    fitfam <- faminfo$family
    logit.link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  } else if (inherits(x, c("lme", "plm", "gls", "truncreg", "lmRob"))) {
    fitfam <- "gaussian"
    logit.link <- FALSE
    link.fun <- "identity"
  } else if (inherits(x, c("vgam", "vglm"))) {
    faminfo <- x@family
    fitfam <- faminfo@vfamily[1]
    logit.link <- sjmisc::str_contains(faminfo@blurb, "logit")
    link.fun <- faminfo@blurb[3]
    if (!sjmisc::is_empty(string_starts_with(pattern = "logit(", x = link.fun)))
      link.fun <- "logit"
  } else if (inherits(x, c("zeroinfl", "hurdle", "zerotrunc"))) {
    if (is.list(x$dist))
      dist <- x$dist[[1]]
    else
      dist <- x$dist
    fitfam <- switch(
      dist,
      poisson = "poisson",
      negbin = "negative binomial",
      "poisson"
    )
    logit.link <- FALSE
    link.fun <- "log"
    zero.inf <- TRUE
  } else if (inherits(x, c("lm_robust", "felm"))) {
    faminfo <- stats::gaussian(link = "identity")
    fitfam <- faminfo$family
    logit.link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  } else if (inherits(x, "betareg")) {
    fitfam <- "beta"
    logit.link <- x$link$mean$name == "logit"
    link.fun <- x$link$mean$linkfun
  } else if (inherits(x, c("coxph", "coxme"))) {
    fitfam <- "survival"
    logit.link <- TRUE
    link.fun <- NULL
  } else if (inherits(x, "glmmTMB")) {
    faminfo <- stats::family(x)
    fitfam <- faminfo$family
    logit.link <- faminfo$link == "logit"
    link.fun <- faminfo$link
    zero.inf <- !sjmisc::is_empty(lme4::fixef(x)$zi)
  } else if (inherits(x, "MCMCglmm")) {
    fitfam <- x$Residual$family
    logit.link <- FALSE
    link.fun <- ""
  } else {
    # here we have no family method, so we construct a logistic-regression-family-object
    if (inherits(x, c("lrm", "polr", "gmnl", "logistf", "mlogit", "clmm", "clm", "clm2", "multinom", "Zelig-relogit")))
      faminfo <- stats::binomial(link = "logit")
    else
      # get family info
      faminfo <- stats::family(x)

    # in case of multivariate response models for brms or rstanarm,
    # we just take the information from the first model
    if (inherits(x, "brmsfit") && !is.null(stats::formula(x)$response)) {
      multi.var <- TRUE
      if (!mv) faminfo <- faminfo[[1]]
    }

    if (inherits(x, "stanmvreg")) {
      multi.var <- TRUE
      if (!mv) faminfo <- faminfo[[1]]
    }


    if (mv && multi.var) {
      return(purrr::map(faminfo, ~ make_family(
        x,
        .x$family,
        zero.inf,
        .x$link == "logit",
        TRUE,
        .x$link
      )))
    }


    fitfam <- faminfo$family
    logit.link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  }

  make_family(x, fitfam, zero.inf, logit.link, multi.var, link.fun)
}


make_family <- function(x, fitfam, zero.inf, logit.link, multi.var, link.fun) {
  # create logical for family
  binom_fam <-
    fitfam %in% c("bernoulli", "binomial", "quasibinomial", "binomialff") |
    sjmisc::str_contains(fitfam, "binomial", ignore.case = TRUE)

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson", "genpois", "ziplss") |
    sjmisc::str_contains(fitfam, "poisson", ignore.case = TRUE)

  neg_bin_fam <-
    sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T) |
    sjmisc::str_contains(fitfam, "nbinom", ignore.case = TRUE) |
    sjmisc::str_contains(fitfam, "nzbinom", ignore.case = TRUE) |
    sjmisc::str_contains(fitfam, "genpois", ignore.case = TRUE) |
    sjmisc::str_contains(fitfam, "negbinomial", ignore.case = TRUE) |
    sjmisc::str_contains(fitfam, "neg_binomial", ignore.case = TRUE)

  beta_fam <- inherits(x, "betareg") | fitfam %in% c("beta")

  linear_model <- !binom_fam & !poisson_fam & !neg_bin_fam & !logit.link

  zero.inf <- zero.inf | fitfam == "ziplss" |
    sjmisc::str_contains(fitfam, "zero_inflated", ignore.case = T) |
    sjmisc::str_contains(fitfam, "zero-inflated", ignore.case = T) |
    sjmisc::str_contains(fitfam, "hurdle", ignore.case = T) |
    grepl("^(zt|zi|za|hu)", fitfam, perl = TRUE)

  is.ordinal <-
    inherits(x, c("polr", "clm", "clm2", "clmm", "gmnl", "mlogit", "multinom")) |
    fitfam %in% c("cumulative", "cratio", "sratio", "acat", "ordinal", "multinomial")

  is.categorical <- fitfam == "categorical"


  # check if we have binomial models with trials instead of binary outcome

  is.trial <- FALSE

  if (inherits(x, "brmsfit") && is.null(stats::formula(x)$responses)) {
    tryCatch(
      {
        rv <- deparse(stats::formula(x)$formula[[2L]], width.cutoff = 500L)
        is.trial <- sjmisc::trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
      },
      error = function(x) { NULL }
    )
  }

  if (binom_fam && !inherits(x, "brmsfit")) {
    tryCatch(
      {
        rv <- deparse(stats::formula(x)[[2L]], width.cutoff = 500L)
        is.trial <- grepl("cbind\\((.*)\\)", rv)
      },
      error = function(x) { NULL }
    )
  }


  list(
    is_bin = binom_fam & !neg_bin_fam,
    is_count = poisson_fam | neg_bin_fam,
    is_pois = poisson_fam | neg_bin_fam,
    is_negbin = neg_bin_fam,
    is_beta = beta_fam,
    is_logit = logit.link,
    is_linear = linear_model,
    is_zeroinf = zero.inf,
    is_ordinal = is.ordinal,
    is_categorical = is.categorical,
    is_multivariate = multi.var,
    is_trial = is.trial,
    link.fun = link.fun,
    family = fitfam
  )
}
