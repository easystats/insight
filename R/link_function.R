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



# Default method ---------------------------


#' @export
link_function.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  tryCatch(
    {
      # get model family
      ff <- .gam_family(x)

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





# Gaussian family ------------------------------------------


#' @export
link_function.lm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}

#' @export
link_function.lme <- link_function.lm

#' @export
link_function.bayesx <- link_function.lm

#' @export
link_function.mixed <- link_function.lm

#' @export
link_function.truncreg <- link_function.lm

#' @export
link_function.censReg <- link_function.lm

#' @export
link_function.gls <- link_function.lm

#' @export
link_function.rq <- link_function.lm

#' @export
link_function.crq <- link_function.lm

#' @export
link_function.lmRob <- link_function.lm

#' @export
link_function.complmRob <- link_function.lm

#' @export
link_function.speedlm <- link_function.lm

#' @export
link_function.biglm <- link_function.lm

#' @export
link_function.lmrob <- link_function.lm

#' @export
link_function.lm_robust <- link_function.lm

#' @export
link_function.iv_robust <- link_function.lm

#' @export
link_function.aovlist <- link_function.lm

#' @export
link_function.felm <- link_function.lm

#' @export
link_function.feis <- link_function.lm

#' @export
link_function.ivreg <- link_function.lm

#' @export
link_function.plm <- link_function.lm







# General family ---------------------------------

#' @export
link_function.glm <- link_function.default

#' @export
link_function.speedglm <- link_function.default

#' @export
link_function.bigglm <- link_function.default

#' @export
link_function.brglm <- link_function.default






# Logit link ------------------------


#' @export
link_function.multinom <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}

#' @export
link_function.BBreg <- link_function.multinom

#' @export
link_function.BBmm <- link_function.multinom

#' @export
link_function.gmnl <- link_function.multinom

#' @export
link_function.logistf <- link_function.multinom

#' @export
link_function.lrm <- link_function.multinom

#' @export
link_function.mlogit <- link_function.multinom

#' @export
link_function.coxph <- link_function.multinom

#' @export
link_function.survfit <- link_function.multinom

#' @export
link_function.coxme <- link_function.multinom






# Log links ------------------------


#' @export
link_function.zeroinfl <- function(x, ...) {
  stats::make.link("log")$linkfun
}

#' @export
link_function.hurdle <- link_function.zeroinfl

#' @export
link_function.zerotrunc <- link_function.zeroinfl







# Tobit links ---------------------------------


#' @export
link_function.tobit <- function(x, ...) {
  .make_tobit_family(x)$linkfun
}

#' @export
link_function.crch <- link_function.tobit

#' @export
link_function.survreg <- link_function.tobit

#' @export
link_function.psm <- link_function.tobit

#' @export
link_function.flexsurvreg <- function(x, ...) {
  dist <- parse(text = .safe_deparse(x$call))[[1]]$dist
  .make_tobit_family(x, dist)$linkfun
}







# Ordinal and cumulative links --------------------------


#' @export
link_function.clm <- function(x, ...) {
  stats::make.link(link = .get_ordinal_link(x))$linkfun
}

#' @export
link_function.clm2 <- link_function.clm

#' @export
link_function.clmm <- link_function.clm

#' @export
link_function.mixor <- link_function.clm






# Other models -----------------------------


#' @export
link_function.fixest <- function(x, ...) {
  if (inherits(x$family, "family")) {
    x$family$linkfun
  } else {
    link <- switch(
      x$family,
      "poisson" = ,
      "negbin" = "log",
      "logit" = "logit",
      "gaussian" = "identity"
    )
    stats::make.link(link)$linkfun
  }
}

#' @export
link_function.feglm <- link_function.fixest


#' @export
link_function.glmx <- function(x, ...) {
  x$family$glm$linkfun
}


#' @export
link_function.gam <- function(x, ...) {
  lf <- tryCatch(
    {
      # get model family
      ff <- .gam_family(x)

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

  if (is.null(lf)) {
    mi <- .gam_family(x)
    if (.obj_has_name(mi, "linfo")) {
      if (.obj_has_name(mi$linfo, "linkfun")) {
        lf <- mi$linfo$linkfun
      } else {
        lf <- mi$linfo[[1]]$linkfun
      }
    }
  }

  lf
}



#' @export
link_function.glimML <- function(x, ...) {
  stats::make.link(link = x@link)$linkfun
}



#' @export
link_function.glmmadmb <- function(x, ...) {
  x$linkfun
}



#' @rdname link_function
#' @export
link_function.gamlss <- function(x, what = c("mu", "sigma", "nu", "tau"), ...) {
  what <- match.arg(what)
  faminfo <- get(x$family[1], asNamespace("gamlss"))()
  switch(
    what,
    "mu" = faminfo$mu.linkfun,
    "sigma" = faminfo$sigma.linkfun,
    "nu" = faminfo$nu.linkfun,
    "tau" = faminfo$tau.linkfun,
    faminfo$mu.linkfun
  )
}



#' @export
link_function.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}



#' @export
link_function.bamlss <- function(x, ...) {
  flink <- stats::family(x)$links[1]
  tryCatch(
    {
      stats::make.link(flink)$linkfun
    },
    error = function(e) {
      print_colour("\nCould not find appropriate link-function.\n", "red")
    }
  )
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
link_function.betareg <- function(x, ...) {
  x$link$mean$linkfun
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
    lapply(fam, .brms_link_fun)
  } else {
    .brms_link_fun(fam)
  }
}









# helper -----------------------


.brms_link_fun <- function(fam) {
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
