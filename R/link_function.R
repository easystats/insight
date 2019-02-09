#' @title Get link-function from model object
#' @name link_function
#'
#' @description Returns the link-function from a model object.
#'
#' @inheritParams find_predictors
#' @inheritParams find_formula
#'
#' @return A function, describing the link-function from a model-object.
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
