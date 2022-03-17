#' @title Find possible offset terms in a model
#' @name find_offset
#'
#' @description Returns a character vector with the name(s) of offset terms.
#'
#' @inheritParams find_predictors
#'
#' @return A character vector with the name(s) of offset terms.
#'
#' @examples
#' # Generate some zero-inflated data
#' set.seed(123)
#' N <- 100 # Samples
#' x <- runif(N, 0, 10) # Predictor
#' off <- rgamma(N, 3, 2) # Offset variable
#' yhat <- -1 + x * 0.5 + log(off) # Prediction on log scale
#' dat <- data.frame(y = NA, x, logOff = log(off))
#' dat$y <- rpois(N, exp(yhat)) # Poisson process
#' dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) # Zero-inflation process
#'
#' if (require("pscl")) {
#'   m1 <- zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
#'   find_offset(m1)
#'
#'   m2 <- zeroinfl(y ~ x | 1, data = dat, offset = logOff, dist = "poisson")
#'   find_offset(m2)
#' }
#' @export
find_offset <- function(x) {
  terms <- as.character(attributes(stats::terms(find_formula(x)[[1]]))$variables)
  offset <- NULL

  offcol <- grep("offset(", terms, fixed = TRUE)
  if (length(offcol)) {
    offset <- clean_names(terms[offcol])
  }

  if (is.null(offset) && object_has_names(x, "call") && object_has_names(x$call, "offset")) {
    offset <- clean_names(safe_deparse(x$call$offset))
  }

  offset
}
