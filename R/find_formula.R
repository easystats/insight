#' @title Find model formula
#' @name find_formula
#'
#' @description Get model formula
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of formulas that describe the model. For simple models,
#'    only one list-element, \code{conditional}, is returned. For more complex
#'    models, the returned list may have following elements:
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
#' find_formula(m)
#' @importFrom stats formula terms as.formula
#' @export
find_formula <- function(x, ...) {
  UseMethod("find_formula")
}


#' @export
find_formula.default <- function(x, ...) {
  tryCatch({
    list(conditional = stats::formula(x))
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
find_formula.hurdle <- function(x, ...) {
  zeroinf_formula(x)
}


#' @export
find_formula.zeroinfl <- function(x, ...) {
  zeroinf_formula(x)
}


#' @export
find_formula.zerotrunc <- function(x, ...) {
  zeroinf_formula(x)
}


#' @export
find_formula.clm2 <- function(x, ...) {
  list(conditional = attr(x$location, "terms", exact = TRUE))
}


#' @export
find_formula.aovlist <- function(x, ...) {
  list(conditional = attr(x, "terms", exact = TRUE))
}


#' @export
find_formula.glmmTMB <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, component = "zi")
  f.disp <- stats::formula(x, component = "disp")

  if (identical(deparse(f.zi, width.cutoff = 500), "~0") ||
    identical(deparse(f.zi, width.cutoff = 500), "~1")) {
    f.zi <- NULL
  }

  if (identical(deparse(f.disp, width.cutoff = 500), "~0") ||
    identical(deparse(f.disp, width.cutoff = 500), "~1")) {
    f.disp <- NULL
  }


  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
    f <- deparse(.x, width.cutoff = 500)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.zirandom <- lapply(lme4::findbars(f.zi), function(.x) {
    f <- deparse(.x, width.cutoff = 500)
    if (f == "NULL") {
      return(NULL)
    }
    stats::as.formula(paste0("~", f))
  })

  if (length(f.zirandom) == 1) {
    f.zirandom <- f.zirandom[[1]]
  }


  f.cond <- stats::as.formula(get_fixed_effects(f.cond))
  if (!is.null(f.zi)) f.zi <- stats::as.formula(get_fixed_effects(f.zi))

  compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom,
    dispersion = f.disp
  ))
}


#' @export
find_formula.merMod <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- stats::formula(x)
  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
    f <- deparse(.x, width.cutoff = 500)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(get_fixed_effects(f.cond))

  compact_list(list(conditional = f.cond, random = f.random))
}


#' @export
find_formula.brmsfit <- function(x, ...) {
  ## TODO check for ZI and multivariate response models
  list(conditional = stats::formula(x))
}


#' @export
find_formula.MCMCglmm <- function(x, ...) {
  fm <- x$Fixed$formula
  fmr <- x$Random$formula

  compact_list(list(conditional = fm, random = fmr))
}


#' @export
find_formula.lme <- function(x, ...) {
  fm <- eval(x$call$fixed)
  fmr <- eval(x$call$random)

  compact_list(list(conditional = fm, random = fmr))
}


#' @export
find_formula.MixMod <- function(x, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, type = "zi_fixed")
  f.random <- stats::formula(x, type = "random")
  f.zirandom <- stats::formula(x, type = "zi_random")

  compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom
  ))
}


#' @export
find_formula.stanmvreg <- function(x, ...) {
  ## TODO check for ZI and multivariate response models
  list(conditional = stats::formula(x))
}


zeroinf_formula <- function(x) {
  f <- tryCatch({
    stats::formula(x)
  },
  error = function(x) {
    NULL
  }
  )

  if (is.null(f)) {
    return(NULL)
  }

  f <- trim(unlist(strsplit(deparse(f, width.cutoff = 500L), "\\|")))

  c.form <- stats::as.formula(f[1])
  if (length(f) == 2) {
    zi.form <- stats::as.formula(paste0("~", f[2]))
  } else {
    zi.form <- NULL
  }

  compact_list(list(conditional = c.form, zero_inflated = zi.form))
}
