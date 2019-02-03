#' @title Get link-inverse function from model object
#' @name link_inverse
#'
#' @description Returns the link-inverse function from a model object.
#'
#' @param mv_response Logical, if \code{TRUE} and model is a multivariate response
#'    model from a \code{brmsfit} object or of class \code{stanmvreg}, then a
#'    list of values (one for each regression) is returned.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return A function, describing the inverse-link function from a model-object.
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
#'
#' @export
link_inverse <- function(x, ...) {
  UseMethod("link_inverse")
}


#' @importFrom stats family make.link
#' @export
link_inverse.default <- function(x, ...) {
  if (inherits(x, "Zelig-relogit")) {
    stats::make.link(link = "logit")$linkinv
  } else {
    tryCatch(
      {stats::family(x)$linkinv},
      error = function(x) { NULL }
    )
  }
}


#' @export
link_inverse.glm <- function(x, ...) {
  tryCatch(
    {stats::family(x)$linkinv},
    error = function(x) { NULL }
  )
}


#' @export
link_inverse.gam <- function(x, ...) {
  tryCatch(
    {stats::family(x)$linkinv},
    error = function(x) { NULL }
  )
}


#' @importFrom stats gaussian
#' @export
link_inverse.lm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.zeroinfl <- function(x, ...) {
  stats::make.link("log")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.hurdle <- function(x, ...) {
  stats::make.link("log")$linkinv
}


#' @importFrom stats make.link
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


#' @importFrom stats gaussian
#' @export
link_inverse.lme <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats gaussian
#' @export
link_inverse.plm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats gaussian
#' @export
link_inverse.lm_robust <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats gaussian
#' @export
link_inverse.felm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats gaussian
#' @export
link_inverse.gls <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @importFrom stats gaussian
#' @export
link_inverse.lmRob <- function(x, ...) {
  stats::gaussian(link = "identity")$linkinv
}


#' @export
link_inverse.betareg <- function(x, ...) {
  x$link$mean$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.polr <- function(x, ...) {
  link <- x$method
  if (link == "logistic") link <- "logit"
  stats::make.link(link)$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.gmnl <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.mlogit <- function(x, ...) {
  stats::make.link("logit")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.clm <- function(x, ...) {
  stats::make.link(x$link)$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.clmm <- function(x, ...) {
  stats::make.link(x$link)$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.glmmTMB <- function(x, ...) {
  ff <- stats::family(x)

  if ("linkinv" %in% names(ff))
    ff$linkinv
  else if ("link" %in% names(ff) && is.character(ff$link))
    stats::make.link(ff$link)$linkinv
  else
    match.fun("exp")
}


#' @export
link_inverse.MCMCglmm <- function(x, ...) {
  NULL
}


#' @importFrom stats make.link
#' @export
link_inverse.clm2 <- function(x, ...) {
  switch(
    x$link,
    logistic = ,
    probit = stats::make.link("logit")$linkinv,
    cloglog = ,
    loglog = stats::make.link("log")$linkinv,
    stats::make.link("logit")$linkinv
  )
}


#' @importFrom stats make.link
#' @export
link_inverse.lrm <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.logistf <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @importFrom stats make.link
#' @export
link_inverse.multinom <- function(x, ...) {
  stats::make.link(link = "logit")$linkinv
}


#' @rdname link_inverse
#' @importFrom stats family
#' @export
link_inverse.stanmvreg <- function(x, mv_response = FALSE, ...) {
  fam <- stats::family(x)
  if (mv_response) {
    il <- lapply(fam, function(.x) { .x$linkinv })
  } else {
    fam <- fam[[1]]
    il <- fam$linkinv
  }

  il
}


#' @rdname link_inverse
#' @importFrom stats family formula
#' @export
link_inverse.brmsfit <- function(x, mv_response = FALSE, ...) {
  fam <- stats::family(x)
  if (!is.null(stats::formula(x)$response)) {
    if (mv_response) {
      il <- lapply(fam, brms_link_inverse)
    } else {
      fam <- fam[[1]]
      il <- brms_link_inverse(fam)
    }
  } else {
    il <- brms_link_inverse(fam)
  }

  il
}

#' @importFrom stats make.link
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
