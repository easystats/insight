#' @title Find name of model parameters
#' @name find_parameters
#'
#' @description Get model formula
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. For simple models, only one list-element,
#'    \code{conditional}, is returned. For more complex models, the returned
#'    list may have following elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" part from the model
#'      \item \code{random}, the "random effects" part from the model
#'      \item \code{zero_inflated}, the "fixed effects" part from the zero-inflation component of the model
#'      \item \code{zero_inflated_random}, the "random effects" part from the zero-inflation component of the model
#'      \item \code{dispersion}, the dispersion formula
#'    }
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @importFrom stats coef
#' @export
find_parameters <- function(x, ...) {
  UseMethod("find_parameters")
}


#' @export
find_parameters.default <- function(x, ...) {
  tryCatch({
    list(conditional = names(stats::coef(x)))
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
find_parameters.MixMod <- function(x,  ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  re.names <- dimnames(lme4::ranef(x))[[2]]

  compact_list(list(
    conditional = names(lme4::fixef(x, sub_model = "main")),
    random = re.names[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = names(lme4::fixef(x, sub_model = "zero_part")),
    zero_inflated_random = re.names[grepl("^zi_", re.names, perl = TRUE)]
  ))
}


#' @export
find_parameters.merMod <- function(x,  ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = lapply(lme4::ranef(x), colnames)
  ))
}


#' @export
find_parameters.glmmTMB <- function(x,  ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x)$cond),
    random = lapply(lme4::ranef(x)$cond, colnames),
    zero_inflated = names(lme4::fixef(x)$zi),
    zero_inflated_random = lapply(lme4::ranef(x)$zi, colnames),
    dispersion = names(lme4::fixef(x)$disp)
  ))
}


#' @export
find_parameters.brmsfit <- function(x,  ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  fe <- colnames(as.data.frame(x))

  cond <- fe[grepl(pattern = "b_(?!zi_)(.*)", fe, perl = TRUE)]
  zi <- fe[grepl(pattern = "b_zi_", fe, perl = TRUE)]
  rand <- fe[grepl(pattern = "(?!.*__zi)(?=.*r_)", fe, perl = TRUE)]
  randzi <- fe[grepl(pattern = "r_(.*__zi)", fe, perl = TRUE)]

  compact_list(list(
    conditional = cond,
    random = rand,
    zero_inflated = zi,
    zero_inflated_random = randzi
  ))
}


#' @export
find_parameters.stanreg <- function(x,  ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  fe <- colnames(as.data.frame(x))

  cond <- fe[grepl(pattern = "^(?!(b\\[|sigma|Sigma))", fe, perl = TRUE)]
  rand <- fe[grepl(pattern = "^b\\[", fe, perl = TRUE)]

  compact_list(list(
    conditional = cond,
    random = rand
  ))
}
