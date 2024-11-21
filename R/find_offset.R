#' @title Find possible offset terms in a model
#' @name find_offset
#'
#' @description Returns a character vector with the name(s) of offset terms.
#'
#' @inheritParams find_predictors
#'
#' @return A character vector with the name(s) of offset terms.
#'
#' @examplesIf require("pscl")
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
#' m1 <- zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
#' find_offset(m1)
#'
#' m2 <- zeroinfl(y ~ x | 1, data = dat, offset = logOff, dist = "poisson")
#' find_offset(m2)
#' @export
find_offset <- function(x) {
  model_terms <- .safe(
    as.character(attributes(stats::terms(find_formula(x, verbose = FALSE)[[1]]))$variables),
    find_terms(x)
  )
  model_offset <- NULL

  offcol <- grep("offset(", model_terms, fixed = TRUE)
  if (length(offcol)) {
    model_offset <- model_terms[offcol]
  }

  model_call <- get_call(x)
  if (is.null(model_offset) && object_has_names(model_call, "offset")) {
    model_offset <- safe_deparse(model_call$offset)
  }

  # fixest sometimes returns a weird macro syntax instead of the real offset
  # if we have to implement too many model-specific workarounds, it may eventually be worth it to do S3
  # VAB: no test because I can only replicate in a weird {etwfe} example
  if (inherits(x, "fixest")) {
    if (is.null(model_offset) || startsWith(model_offset, "..")) {
      model_offset <- x[["model_info"]][["offset"]]
    }
    model_offset <- sub("^~", "", model_offset)
  }

  model_offset <- clean_names(model_offset)

  # sometimes we get an empty list (e.g., fixest with iris dataset)
  if (length(model_offset)) {
    model_offset
  } else {
    NULL
  }
}
