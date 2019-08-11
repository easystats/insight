#' @title Get link-function from model object
#' @name link_function
#'
#' @description Returns the link-function from a model object.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#' @inheritParams link_inverse
#'
#' @return A function, describing the link-function from a model-object.
#'    For multivariate-response models, a list of functions is returned.
#'
#' @examples
#' # example from ?stats::glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' link_function(m)(.3)
#' # same as
#' log(.3)
#' @export
#' @importFrom stats family make.link
link_function <- function(x, ...) {
  UseMethod("link_function")
}


#' @export
link_function.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  tryCatch({
    # get model family
    ff <- stats::family(x)

    # return link function, if exists
    if ("linkfun" %in% names(ff)) {
      return(ff$linkfun)
    }

    # else, create link function from link-string
    if ("link" %in% names(ff)) {
      return(match.fun(ff$link))
    }

    NULL
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
link_function.speedglm <- function(x, ...) {
  stats::family(x)$linkfun
}


#' @export
link_function.bigglm <- function(x, ...) {
  stats::family(x)$linkfun
}


#' @export
link_function.multinom <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}


#' @export
link_function.BBreg <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}


#' @export
link_function.BBmm <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}


#' @export
link_function.glimML <- function(x, ...) {
  stats::make.link(link = x@link)$linkfun
}


#' @export
link_function.gamlss <- function(x, ...) {
  faminfo <- get(x$family[1], asNamespace("gamlss"))()
  faminfo$mu.linkfun
}


#' @export
link_function.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}


#' @export
link_function.clm <- function(x, ...) {
  stats::make.link(link = get_ordinal_link(x))$linkfun
}


#' @export
link_function.clm2 <- function(x, ...) {
  stats::make.link(link = get_ordinal_link(x))$linkfun
}


#' @export
link_function.LORgee <- function(x, ...) {
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

  stats::make.link(link)$linkfun
}


#' @export
link_function.clmm <- function(x, ...) {
  stats::make.link(link = get_ordinal_link(x))$linkfun
}


#' @export
link_function.vgam <- function(x, ...) {
  x@family@linkfun
}


#' @export
link_function.vglm <- function(x, ...) {
  x@family@linkfun
}


#' @export
link_function.polr <- function(x, ...) {
  link <- switch(
    x$method,
    logistic = "logit",
    probit = "probit",
    "log"
  )

  stats::make.link(link)$linkfun
}


#' @export
link_function.svyolr <- function(x, ...) {
  link <- switch(
    x$method,
    logistic = "logit",
    probit = "probit",
    "log"
  )

  stats::make.link(link)$linkfun
}


#' @export
link_function.gmnl <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.logistf <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.lrm <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.mlogit <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.zeroinfl <- function(x, ...) {
  stats::make.link("log")$linkfun
}


#' @export
link_function.hurdle <- function(x, ...) {
  stats::make.link("log")$linkfun
}


#' @export
link_function.zerotrunc <- function(x, ...) {
  stats::make.link("log")$linkfun
}


#' @export
link_function.betareg <- function(x, ...) {
  x$link$mean$linkfun
}


#' @export
link_function.mixed <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.truncreg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.censReg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.gls <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.lme <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.rq <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.crq <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.tobit <- function(x, ...) {
  .make_tobit_family(x)$linkfun
}


#' @export
link_function.crch <- function(x, ...) {
  .make_tobit_family(x)$linkfun
}


#' @export
link_function.survreg <- function(x, ...) {
  .make_tobit_family(x)$linkfun
}


#' @export
link_function.psm <- function(x, ...) {
  .make_tobit_family(x)$linkfun
}


#' @export
link_function.lmRob <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.speedlm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.biglm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.lmrob <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.lm_robust <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.iv_robust <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.aovlist <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.felm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.feis <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.ivreg <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.plm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.coxph <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.survfit <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.coxme <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @importFrom stats poisson
#' @export
link_function.gbm <- function(x, ...) {
  switch(
    x$distribution$name,
    laplace = ,
    tdist = ,
    gaussian = stats::gaussian(link = "identity")$linkfun,
    poisson = stats::poisson(link = "log")$linkfun,
    huberized = ,
    adaboost = ,
    coxph = ,
    bernoulli = stats::make.link("logit")$linkfun
  )
}


#' @export
link_function.stanmvreg <- function(x, ...) {
  fam <- stats::family(x)
  lapply(fam, function(.x) .x$linkfun)
}


#' @export
link_function.brmsfit <- function(x, ...) {
  fam <- stats::family(x)
  if (is_multivariate(x)) {
    lapply(fam, brms_link_fun)
  } else {
    brms_link_fun(fam)
  }
}


brms_link_fun <- function(fam) {
  # do we have custom families?
  if (!is.null(fam$family) && (is.character(fam$family) && fam$family == "custom")) {
    il <- stats::make.link(fam$link)$linkfun
  } else {
    if ("linkfun" %in% names(fam)) {
      il <- fam$linkfun
    } else if ("link" %in% names(fam) && is.character(fam$link)) {
      il <- stats::make.link(fam$link)$linkfun
    } else {
      ff <- get(fam$family, asNamespace("stats"))
      il <- ff(fam$link)$linkfun
    }
  }

  il
}
