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
  tryCatch({
    # get model family
    ff <- stats::family(x)

    # return link function, if exists
    if ("linkfun" %in% names(ff)) return(ff$linkfun)

    # else, create link function from link-string
    if ("link" %in% names(ff)) return(match.fun(ff$link))

    NULL
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
link_function.multinom <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}


#' @export
link_function.clm <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
}


#' @export
link_function.clm2 <- function(x, ...) {
  stats::make.link(link = "logit")$linkfun
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
link_function.gmnl <- function(x, ...) {
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
link_function.truncreg <- function(x, ...) {
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
link_function.felm <- function(x, ...) {
  stats::gaussian(link = "identity")$linkfun
}


#' @export
link_function.coxph <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @export
link_function.coxme <- function(x, ...) {
  stats::make.link("logit")$linkfun
}


#' @rdname link_function
#' @export
link_function.stanmvreg <- function(x, ...) {
  fam <- stats::family(x)
  lapply(fam, function(.x) .x$linkfun)
}


#' @rdname link_function
#' @export
link_function.brmsfit <- function(x, ...) {
  fam <- stats::family(x)
  if (is_multivariate(x))
    lapply(fam, brms_link_fun)
  else
    brms_link_fun(fam)
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