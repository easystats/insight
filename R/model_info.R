#' @title Access information from model objects
#' @name model_info
#'
#' @description Retrieve information from model objects.
#'
#' @param verbose Toggle off warnings.
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
#'      \item \code{is_betabinomial}: family is beta-binomial
#'      \item \code{is_dirichlet}: family is dirichlet
#'      \item \code{is_exponential}: family is exponential (e.g. Gamma or Weibull)
#'      \item \code{is_logit}: model has logit link
#'      \item \code{is_probit}: model has probit link
#'      \item \code{is_linear}: family is gaussian
#'      \item \code{is_tweedie}: family is tweedie
#'      \item \code{is_ordinal}: family is ordinal or cumulative link
#'      \item \code{is_cumulative}: family is ordinal or cumulative link
#'      \item \code{is_multinomial}: family is multinomial or categorical link
#'      \item \code{is_categorical}: family is categorical link
#'      \item \code{is_censored}: model is a censored model (has a censored response, including survival models)
#'      \item \code{is_truncated}: model is a truncated model (has a truncated response)
#'      \item \code{is_survival}: model is a survival model
#'      \item \code{is_zero_inflated}: model has zero-inflation component
#'      \item \code{is_hurdle}: model has zero-inflation component and is a hurdle-model (truncated family distribution)
#'      \item \code{is_dispersion}: model has dispersion component
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
#' ldose <- rep(0:5, 2)
#' numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
#' sex <- factor(rep(c("M", "F"), c(6, 6)))
#' SF <- cbind(numdead, numalive = 20 - numdead)
#' dat <- data.frame(ldose, sex, SF, stringsAsFactors = FALSE)
#' m <- glm(SF ~ sex * ldose, family = binomial)
#'
#' model_info(m)
#' \dontrun{
#' library(glmmTMB)
#' data("Salamanders")
#' m <- glmmTMB(
#'   count ~ spp + cover + mined + (1 | site),
#'   ziformula = ~ spp + mined,
#'   dispformula = ~DOY,
#'   data = Salamanders,
#'   family = nbinom2
#' )
#' }
#'
#' model_info(m)
#' @importFrom stats formula terms
#' @export
model_info <- function(x, ...) {
  UseMethod("model_info")
}





# Default methods --------------------------------------

#' @export
model_info.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}


#' @importFrom stats family
#' @rdname model_info
#' @export
model_info.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
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
    error = function(x) {
      NULL
    }
  )

  if (!is.null(faminfo)) {
    .make_family(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      link.fun = faminfo$link,
      ...
    )
  } else {
    if (isTRUE(verbose)) {
      warning("Could not access model information.", call. = FALSE)
    }
    NULL
  }
}





# Models with general handling, Gaussian ----------------------------------


#' @export
model_info.mmclogit <- function(x, ...) {
  .make_family(x, ...)
}

#' @export
model_info.maxLik <- model_info.mmclogit

#' @export
model_info.censReg <- model_info.mmclogit

#' @export
model_info.htest <- model_info.mmclogit

#' @export
model_info.BFBayesFactor <- model_info.mmclogit

#' @export
model_info.lme <- model_info.mmclogit

#' @export
model_info.bayesx <- model_info.mmclogit

#' @export
model_info.rq <- model_info.mmclogit

#' @export
model_info.crq <- model_info.mmclogit

#' @export
model_info.crqs <- model_info.mmclogit

#' @export
model_info.nlrq <- model_info.mmclogit

#' @export
model_info.rqss <- model_info.mmclogit

#' @export
model_info.mixed <- model_info.mmclogit

#' @export
model_info.plm <- model_info.mmclogit

#' @export
model_info.mcmc <- model_info.mmclogit

#' @export
model_info.gls <- model_info.mmclogit

#' @export
model_info.nls <- model_info.mmclogit

#' @export
model_info.MANOVA <- model_info.mmclogit

#' @export
model_info.RM <- model_info.mmclogit

#' @export
model_info.truncreg <- model_info.mmclogit

#' @export
model_info.lmRob <- model_info.mmclogit

#' @export
model_info.speedlm <- model_info.mmclogit

#' @export
model_info.lmrob <- model_info.mmclogit

#' @export
model_info.complmrob <- model_info.mmclogit

#' @export
model_info.lm_robust <- model_info.mmclogit

#' @export
model_info.iv_robust <- model_info.mmclogit

#' @export
model_info.felm <- model_info.mmclogit

#' @export
model_info.feis <- model_info.mmclogit

#' @export
model_info.ivreg <- model_info.mmclogit

#' @export
model_info.aovlist <- model_info.mmclogit

#' @export
model_info.rma <- model_info.mmclogit

#' @export
model_info.mlm <- function(x, ...) {
  .make_family(x, multi.var = TRUE, ...)
}

#' @export
model_info.afex_aov <- function(x, ...) {
  if ("aov" %in% names(x)) {
    .make_family(x$aov, ...)
  } else {
    .make_family(x$lm, ...)
  }
}






# Models with logit-link --------------------------------


#' @export
model_info.logistf <- function(x, ...) {
  faminfo <- stats::binomial(link = "logit")
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}

#' @export
model_info.lrm <- model_info.logistf

#' @export
model_info.multinom <- model_info.logistf

#' @export
model_info.mlogit <- model_info.logistf

#' @export
model_info.gmnl <- model_info.logistf








# Models with ordinal family ------------------------------------


#' @export
model_info.clm <- function(x, ...) {
  faminfo <- stats::binomial(link = .get_ordinal_link(x))
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}

#' @export
model_info.clm2 <- model_info.clm

#' @export
model_info.clmm <- model_info.clm

#' @export
model_info.mixor <- model_info.clm








# Models with family-function  ----------------------------------


#' @importFrom stats family
#' @export
model_info.bamlss <- function(x, ...) {
  faminfo <- stats::family(x)
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$links[1] == "logit",
    link.fun = faminfo$links[1],
    ...
  )
}


#' @export
model_info.speedglm <- function(x, ...) {
  faminfo <- stats::family(x)
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}

#' @export
model_info.brmultinom <- model_info.speedglm







# Models with tobit family ----------------------------------


#' @export
model_info.flexsurvreg <- function(x, ...) {
  dist <- parse(text = .safe_deparse(x$call))[[1]]$dist
  faminfo <- .make_tobit_family(x, dist)

  .make_family(
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

  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}

#' @export
model_info.crch <- model_info.tobit

#' @export
model_info.survreg <- model_info.tobit







# Models with family in object ----------------------------------


#' @export
model_info.MixMod <- function(x, ...) {
  faminfo <- x$family
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}

#' @export
model_info.glmmPQL <- model_info.MixMod

#' @export
model_info.bife <- model_info.MixMod


#' @export
model_info.glmx <- function(x, ...) {
  faminfo <- x$family$glm
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}


#' @export
model_info.fixest <- function(x, ...) {
  faminfo <- x$family

  if (is.null(faminfo)) {
    if (!is.null(x$method) && x$method == "feols") {
      .make_family(x, ...)
    }
  } else if (inherits(faminfo, "family")) {
    .make_family(
      x = x,
      fitfam = faminfo$family,
      logit.link = faminfo$link == "logit",
      link.fun = faminfo$link,
      ...
    )
  } else {
    fitfam <- switch(
      faminfo,
      "negbin" = "negative binomial",
      "logit" = "binomial",
      faminfo
    )

    link <- switch(
      faminfo,
      "poisson" = ,
      "negbin" = "log",
      "logit" = "logit",
      "gaussian" = "identity"
    )

    .make_family(
      x = x,
      fitfam = fitfam,
      logit.link = link == "logit",
      link.fun = link,
      ...
    )
  }
}

#' @export
model_info.feglm <- model_info.fixest







# Survival-models ----------------------------------------


#' @export
model_info.coxph <- function(x, ...) {
  .make_family(
    x = x,
    fitfam = "survival",
    logit.link = TRUE,
    link.fun = NULL,
    ...
  )
}

#' @export
model_info.aareg <- model_info.coxph

#' @export
model_info.survfit <- model_info.coxph

#' @export
model_info.coxme <- model_info.coxph







# Zero-Inflated Models ------------------------------


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
  .make_family(
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
  .make_family(
    x = x,
    fitfam = fitfam,
    zero.inf = TRUE,
    hurdle = TRUE,
    link.fun = "log",
    ...
  )
}




# Bayesian Models ---------------------------


#' @export
model_info.brmsfit <- function(x, ...) {
  faminfo <- stats::family(x)
  if (is_multivariate(x)) {
    lapply(faminfo, function(.x) {
      .make_family(
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
    .make_family(
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
    .make_family(
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







# Other models ----------------------------


#' @export
model_info.robmixglm <- function(x, ...) {
  f <- switch(
    tolower(x$family),
    gaussian = stats::gaussian("identity"),
    binomial = stats::binomial("logit"),
    poisson = stats::poisson("log"),
    gamma = stats::Gamma("inverse"),
    truncpoisson = stats::poisson("log"),
    stats::gaussian("identity")
  )
  .make_family(
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
  .make_family(x, ...)
}


#' @export
model_info.averaging <- function(x, ...) {
  if (is.null(attributes(x)$modelList)) {
    warning("Can't calculate covariance matrix. Please use 'fit = TRUE' in 'model.avg()'.", call. = FALSE)
    return(NULL)
  }
  model_info.default(x = attributes(x)$modelList[[1]])
}


#' @export
model_info.cglm <- function(x, ...) {
  link <- parse(text = .safe_deparse(x$call))[[1]]$link
  method <- parse(text = .safe_deparse(x$call))[[1]]$method

  if (!is.null(method) && method == "clm") {
    .make_family(x, ...)
  } else if (!is.null(link)) {
    .make_family(
      x,
      logit.link = link == "logit",
      link.fun = link,
      ...
    )
  } else {
    .make_family(x, ...)
  }
}


#' @export
model_info.cgam <- function(x, ...) {
  faminfo <- x$family
  .make_family(
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
    family <- "ordinal"
  } else {
    family <- "multinomial"
  }

  .make_family(
    x = x,
    fitfam = family,
    logit.link = link == "logit",
    link.fun = link,
    ...
  )
}



#' @export
model_info.BBreg <- function(x, ...) {
  .make_family(
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
model_info.glmmadmb <- function(x, ...) {
  .make_family(
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
model_info.cpglmm <- function(x, ...) {
  link <- parse(text = .safe_deparse(x@call))[[1]]$link
  if (is.null(link)) link <- "log"
  if (is.numeric(link)) link <- "tweedie"
  .make_family(
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
  link <- parse(text = .safe_deparse(x@call))[[1]]$link
  if (is.null(link)) link <- "log"
  if (is.numeric(link)) link <- "tweedie"
  .make_family(
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
  fitfam <- switch(x@method, BB = "betabinomial", NB = "negative binomial")
  .make_family(
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

  if (is.mv) link <- "identity"

  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = link == "logit",
    link.fun = link,
    multi.var = is.mv,
    ...
  )
}



#' @export
model_info.vgam <- function(x, ...) {
  faminfo <- x@family
  link.fun <- faminfo@blurb[3]
  if (grepl("^(l|L)ogit", link.fun)) link.fun <- "logit"
  .make_family(
    x = x,
    fitfam = faminfo@vfamily[1],
    logit.link = any(.string_contains("logit", faminfo@blurb)),
    link.fun = link.fun,
    ...
  )
}

#' @export
model_info.vglm <- model_info.vgam



#' @export
model_info.glmmTMB <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  faminfo <- stats::family(x)
  .make_family(
    x = x,
    fitfam = faminfo$family,
    zero.inf = !.is_empty_object(lme4::fixef(x)$zi),
    hurdle = grepl("truncated", faminfo$family),
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    dispersion = !.is_empty_object(lme4::fixef(x)$disp),
    ...
  )
}



#' @export
model_info.betareg <- function(x, ...) {
  .make_family(
    x = x,
    fitfam = "beta",
    logit.link = x$link$mean$name == "logit",
    link.fun = x$link$mean$name,
    ...
  )
}



#' @export
model_info.DirichletRegModel <- function(x, ...) {
  .make_family(
    x = x,
    fitfam = "dirichlet",
    logit.link = TRUE,
    link.fun = "logit",
    ...
  )
}



#' @export
model_info.gbm <- function(x, ...) {
  faminfo <- switch(
    x$distribution$name,
    laplace = ,
    tdist = ,
    gaussian = list(name = "gaussian", logit = FALSE, link = NULL),
    coxph = list(name = "survival", logit = TRUE, link = NULL),
    poisson = list(name = "poisson", logit = FALSE, link = "log"),
    huberized = ,
    adaboost = ,
    bernoulli = list(name = "binomial", logit = TRUE, link = "logit"),
  )

  .make_family(
    x = x,
    fitfam = faminfo$name,
    logit.link = faminfo$logit,
    link.fun = faminfo$link,
    ...
  )
}



#' @export
model_info.MCMCglmm <- function(x, ...) {
  .make_family(
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
  .make_family(
    x = x,
    fitfam = faminfo$family,
    logit.link = faminfo$link == "logit",
    link.fun = faminfo$link,
    ...
  )
}



#' @export
model_info.svyolr <- function(x, ...) {
  l <- switch(x$method, logistic = "logit", x$method)
  faminfo <- stats::binomial(link = l)
  .make_family(
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
  .make_family(
    x = x,
    fitfam = faminfo$family[2],
    logit.link = faminfo$mu.link == "logit",
    link.fun = faminfo$mu.link,
    ...
  )
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
