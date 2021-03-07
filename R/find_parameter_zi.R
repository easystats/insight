#' @title Find names of model parameters from zero-inflated models
#' @name find_parameters.zeroinfl
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the \code{summary()} output.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#' @inheritParams find_parameters.betamfx
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. The returned list may have following
#'   elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" part from the model.
#'      \item \code{zero_inflated}, the "fixed effects" part from the zero-inflation component of the model.
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @importFrom stats na.omit coef
#' @export
find_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), flatten = FALSE, ...) {
  cf <- names(stats::coef(x))
  component <- match.arg(component)

  l <- .compact_list(list(
    conditional = cf[grepl("^count_", cf, perl = TRUE)],
    zero_inflated = cf[grepl("^zero_", cf, perl = TRUE)]
  ))

  .filter_parameters(l, effects = "all", component = component, flatten = flatten, recursive = FALSE)
}

#' @export
find_parameters.hurdle <- find_parameters.zeroinfl

#' @export
find_parameters.zerotrunc <- find_parameters.zeroinfl


#' @export
find_parameters.zcpglm <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), flatten = FALSE, ...) {
  cf <- stats::coef(x)
  component <- match.arg(component)

  l <- .compact_list(list(
    conditional = names(cf$tweedie),
    zero_inflated = names(cf$zero)
  ))

  .filter_parameters(l, effects = "all", component = component, flatten = flatten, recursive = FALSE)
}


#' @rdname find_parameters.zeroinfl
#' @export
find_parameters.mhurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "infrequent_purchase", "ip", "auxiliary"), flatten = FALSE, ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  cond_pars <- which(grepl("^h2\\.", names(cf)))
  zi_pars <- which(grepl("^h1\\.", names(cf)))
  ip_pars <- which(grepl("^h3\\.", names(cf)))
  aux_pars <- (1:length(names(cf)))[-c(cond_pars, zi_pars, ip_pars)]

  # names(cf) <- gsub("^(h1|h2|h3)\\.(.*)", "\\2", names(cf))

  l <- .compact_list(list(
    conditional = names(cf)[cond_pars],
    zero_inflated = names(cf)[zi_pars],
    infrequent_purchase = names(cf)[ip_pars],
    auxiliary = names(cf)[aux_pars]
  ))

  .filter_parameters(l, effects = "all", component = component, flatten = flatten, recursive = FALSE)
}