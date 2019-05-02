#' @title Get link-inverse function from model object
#' @name link_inverse
#'
#' @description Returns the link-inverse function from a model object.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return A function, describing the inverse-link function from a model-object.
#'    For multivariate-response models, a list of functions is returned.
#'
#' @examples
#' # example from ?stats::glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' link_inverse(m)(.3)
#' # same as
#' exp(.3)
#' @importFrom stats family make.link gaussian formula
#' @export
link_inverse <- function(x, ...) {
  UseMethod("link_inverse")
}


#' @export
link_inverse.default <- function(x, ...) {
  if (inherits(x, "list") && obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  if (inherits(x, "Zelig-relogit")) {
    stats::make.link(link = "logit")$linkinv
  } else {
    tryCatch({
      stats::family(x)$linkinv
    },
    error = function(x) {
      NULL
    }
    )
  }
}


#' @export
link_inverse.glm <- function(x, ...) {
  tryCatch({
    stats::family(x)$linkinv
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
link_inverse.speedglm <- function(x, ...) {
  stats::family(x)$linkinv
}


#' @export
link_inverse.bigglm <- function(x, ...) {
  stats::family(x)$linkinv
}


#' @export
link_inverse.gamlss <- function(x, ...) {
  faminfo <- get(x$family[1], asNamespace("gamlss"))()
  faminfo$mu.linkinv
}


#' @export
link_inverse.gam <- function(x, ...) {
  tryCatch({
    stats::family(x)$linkinv
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
link_inverse.lm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.biglm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.aovlist <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.ivreg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.iv_robust <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.coxph <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @export
link_inverse.coxme <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @export
link_inverse.zeroinfl <- function(x, ...) {
  stats::make.link("log")$linkinv
}


#' @export
link_inverse.hurdle <- function(x, ...) {
  stats::make.link("log")$linkinv
}


#' @export
link_inverse.zerotrunc <- function(x, ...) {
  stats::make.link("log")$linkinv
}


#' @export
link_inverse.glmmPQL <- function(x, ...) {
  x$family$linkinv
}


#' @export
link_inverse.MixMod <- function(x, ...) {
  x$family$linkinv
}


#' @export
link_inverse.vgam <- function(x, ...) {
  x@family@linkinv
}


#' @export
link_inverse.vglm <- function(x, ...) {
  x@family@linkinv
}


#' @export
link_inverse.lme <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.rq <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.crq <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.tobit <- function(x, ...) {
  .make_tobit_family(x)$linkinv
}


#' @export
link_inverse.crch <- function(x, ...) {
  .make_tobit_family(x)$linkinv
}


#' @export
link_inverse.survreg <- function(x, ...) {
  .make_tobit_family(x)$linkinv
}


#' @export
link_inverse.psm <- function(x, ...) {
  .make_tobit_family(x)$linkinv
}


#' @export
link_inverse.mixed <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.censReg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.plm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.lm_robust <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.truncreg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.felm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.feis <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.gls <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.lmRob <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.lmrob <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.speedlm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.betareg <- function(x, ...) {
  x$link$mean$linkinv
}


#' @export
link_inverse.polr <- function(x, ...) {
  link <- x$method
  if (link == "logistic") link <- "logit"
  stats::make.link(link)$linkinv
}


#' @export
link_inverse.LORgee <- function(x, ...) {
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

  stats::make.link(link)$linkinv
}


#' @export
link_inverse.gmnl <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @export
link_inverse.mlogit <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @export
link_inverse.clm <- function(x, ...) {
  stats::make.link(get_ordinal_link(x))$linkinv
}


#' @export
link_inverse.clmm <- function(x, ...) {
  stats::make.link(get_ordinal_link(x))$linkinv
}


#' @export
link_inverse.glmmTMB <- function(x, ...) {
  ff <- stats::family(x)

  if ("linkinv" %in% names(ff)) {
    ff$linkinv
  } else if ("link" %in% names(ff) && is.character(ff$link)) {
    stats::make.link(ff$link)$linkinv
  } else {
    match.fun("exp")
  }
}


#' @export
link_inverse.MCMCglmm <- function(x, ...) {
  NULL
}


#' @export
link_inverse.clm2 <- function(x, ...) {
  stats::make.link(get_ordinal_link(x))$linkinv
}

#' @export
link_inverse.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}


#' @export
link_inverse.lrm <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @export
link_inverse.logistf <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @export
link_inverse.multinom <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @export
link_inverse.stanmvreg <- function(x, ...) {
  fam <- stats::family(x)
  lapply(fam, function(.x) .x$linkinv)
}


#' @export
link_inverse.gbm <- function(x, ...) {
  switch(
    x$distribution$name,
    laplace = ,
    tdist = ,
    gaussian = stats::gaussian(link = "identity")$linkinv,
    poisson = stats::poisson(link = "log")$linkinv,
    huberized = ,
    adaboost = ,
    coxph = ,
    bernoulli = stats::make.link("logit")$linkinv
  )
}


#' @export
link_inverse.brmsfit <- function(x, ...) {
  fam <- stats::family(x)
  if (is_multivariate(x)) {
    lapply(fam, brms_link_inverse)
  } else {
    brms_link_inverse(fam)
  }
}


brms_link_inverse <- function(fam) {
  # do we have custom families?
  if (!is.null(fam$family) && (is.character(fam$family) && fam$family == "custom")) {
    il <- stats::make.link(fam$link)$linkinv
  } else {
    if ("linkinv" %in% names(fam)) {
      il <- fam$linkinv
    } else if ("link" %in% names(fam) && is.character(fam$link)) {
      il <- stats::make.link(fam$link)$linkinv
    } else {
      ff <- get(fam$family, asNamespace("stats"))
      il <- ff(fam$link)$linkinv
    }
  }

  il
}
