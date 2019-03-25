#' @title Find names of model parameters
#' @name find_parameters
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the \code{summary()} output. For Bayesian models, the parameter
#'     names equal the column names of the posterior samples after coercion
#'     from \code{as.data.frame()}.
#'
#' @param parameters Regular expression pattern that describes the parameters that
#'   should be returned.
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
#'      \item \code{dispersion}, the dispersion parameters
#'      \item \code{simplex}, simplex parameters of monotonic effects (\pkg{brms} only)
#'      \item \code{smooth_terms}, the smooth parameters
#'      \item \code{within}, the within-subject effects of Anovas (\code{aov()}) with error term
#'      \item \code{between}, the between-subjects effects of Anovas (\code{aov()}) with error term
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
  if (inherits(x, "list") && obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    return(find_parameters.gam(x))
  }

  tryCatch({
    list(conditional = names(stats::coef(x)))
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
find_parameters.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}


#' @export
find_parameters.gamlss <- function(x, ...) {
  pars <- list(
    conditional = names(stats::coef(x)),
    sigma = names(stats::coef(x, what = "sigma")),
    nu = names(stats::coef(x, what = "nu")),
    tau = names(stats::coef(x, what = "tau"))
  )
  compact_list(pars)
}


#' @export
find_parameters.gam <- function(x, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  st <- summary(x)$s.table

  pars$conditional <- pars$conditional[.grep_non_smoothers(pars$conditional)]
  pars$smooth_terms <- row.names(st)

  compact_list(pars)
}


#' @export
find_parameters.Gam <- function(x, ...) {
  pars <- names(stats::coef(x))

  compact_list(list(
    conditional = pars[.grep_non_smoothers(pars)],
    smooth_terms = pars[.grep_smoothers(pars)]
  ))
}


#' @export
find_parameters.vgam <- function(x, ...) {
  pars <- names(stats::coef(x))

  compact_list(list(
    conditional = pars[.grep_non_smoothers(pars)],
    smooth_terms = pars[.grep_smoothers(pars)]
  ))
}


#' @export
find_parameters.lrm <- function(x, ...) {
  list(conditional = names(stats::coef(x)))
}


#' @export
find_parameters.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  find_parameters.gam(x)
}


#' @export
find_parameters.aovlist <- function(x, ...) {
  l <- lapply(stats::coef(x), names)
  names(l) <- c("conditional", "between", "within")
  l
}


#' @export
find_parameters.MixMod <- function(x, ...) {
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
find_parameters.merMod <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = lapply(lme4::ranef(x), colnames)
  ))
}


#' @export
find_parameters.rlmerMod <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = lapply(lme4::ranef(x), colnames)
  ))
}


#' @export
find_parameters.mixed <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x$full_model)),
    random = lapply(lme4::ranef(x$full_model), colnames)
  ))
}


#' @export
find_parameters.coxme <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = names(lme4::ranef(x))
  ))
}


#' @export
find_parameters.lme <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  re <- lme4::ranef(x)
  if (is.data.frame(re))
    rn <- colnames(re)
  else
    rn <- lapply(re, colnames)

  compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = rn
  ))
}


#' @export
find_parameters.MCMCglmm <- function(x, ...) {
  sc <- summary(x)
  compact_list(list(
    conditional = rownames(sc$solutions),
    random = rownames(sc$Gcovariances)
  ))
}


#' @export
find_parameters.crq <- function(x, ...) {
  sc <- summary(x)
  list(conditional = rownames(sc$coefficients))
}


#' @export
find_parameters.rqss <- function(x, ...) {
  sc <- summary(x)
  list(conditional = rownames(sc$coef))
}


#' @export
find_parameters.glmmTMB <- function(x, ...) {
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
find_parameters.zeroinfl <- function(x, ...) {
  cf <- names(stats::coef(x))
  compact_list(list(
    conditional = cf[grepl("^count_", cf, perl = TRUE)],
    zero_inflated = cf[grepl("^zero_", cf, perl = TRUE)]
  ))
}


#' @export
find_parameters.hurdle <- function(x, ...) {
  cf <- names(stats::coef(x))
  compact_list(list(
    conditional = cf[grepl("^count_", cf, perl = TRUE)],
    zero_inflated = cf[grepl("^zero_", cf, perl = TRUE)]
  ))
}


#' @export
find_parameters.zerotrunc <- function(x, ...) {
  cf <- names(stats::coef(x))
  compact_list(list(
    conditional = cf[grepl("^count_", cf, perl = TRUE)],
    zero_inflated = cf[grepl("^zero_", cf, perl = TRUE)]
  ))
}


#' @rdname find_parameters
#' @export
find_parameters.brmsfit <- function(x, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))

  cond <- fe[grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!zi_)(.*)", fe, perl = TRUE)]
  zi <- fe[grepl(pattern = "(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)", fe, perl = TRUE)]
  rand <- fe[grepl(pattern = "(?!.*__zi)(?=.*r_)", fe, perl = TRUE)]
  randzi <- fe[grepl(pattern = "r_(.*__zi)", fe, perl = TRUE)]
  simo <- fe[grepl(pattern = "^simo_", fe, perl = TRUE)]
  smooth_terms <- fe[grepl(pattern = "^sds_", fe, perl = TRUE)]

  l <- compact_list(list(
    conditional = cond,
    random = rand,
    zero_inflated = zi,
    zero_inflated_random = randzi,
    simplex = simo,
    smooth_terms = smooth_terms
  ))

  if (is_multivariate(x)) {
    rn <- names(find_response(x))
    l <- lapply(rn, function(i) {
      if (obj_has_name(l, "conditional")) {
        conditional <- l$conditional[grepl(sprintf("^(b_|bs_|bsp_|bcs_)\\Q%s\\E_", i), l$conditional)]
      } else {
        conditional <- NULL
      }

      if (obj_has_name(l, "random")) {
        random <- l$random[grepl(sprintf("__\\Q%s\\E\\.", i), l$random)]
      } else {
        random <- NULL
      }

      if (obj_has_name(l, "zero_inflated")) {
        zero_inflated <- l$zero_inflated[grepl(sprintf("^(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)\\Q%s\\E_", i), l$zero_inflated)]
      } else {
        zero_inflated <- NULL
      }

      if (obj_has_name(l, "zero_inflated_random")) {
        zero_inflated_random <- l$zero_inflated_random[grepl(sprintf("__zi_\\Q%s\\E\\.", i), l$zero_inflated_random)]
      } else {
        zero_inflated_random <- NULL
      }

      if (obj_has_name(l, "simplex")) {
        simplex <- l$simplex
      } else {
        simplex <- NULL
      }

      if (obj_has_name(l, "smooth_terms")) {
        smooth_terms <- l$smooth_terms
      } else {
        smooth_terms <- NULL
      }

      compact_list(list(
        conditional = conditional,
        random = random,
        zero_inflated = zero_inflated,
        zero_inflated_random = zero_inflated_random,
        simplex = simplex,
        smooth_terms = smooth_terms
      ))
    })

    names(l) <- rn
    attr(l, "is_mv") <- "1"
  }

  .filter_pars(l, parameters)
}


#' @rdname find_parameters
#' @export
find_parameters.stanreg <- function(x, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))

  cond <- fe[grepl(pattern = "^(?!(b\\[|sigma|Sigma))", fe, perl = TRUE) & .grep_non_smoothers(fe)]
  rand <- fe[grepl(pattern = "^b\\[", fe, perl = TRUE)]
  smooth_terms <- fe[grepl(pattern = "^smooth_sd", fe, perl = TRUE)]

  l <- compact_list(list(
    conditional = cond,
    random = rand,
    smooth_terms = smooth_terms
  ))

  .filter_pars(l, parameters)
}


#' @rdname find_parameters
#' @export
find_parameters.stanmvreg <- function(x, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))
  rn <- names(find_response(x))

  cond <- fe[grepl(pattern = "^(?!(b\\[|sigma|Sigma))", fe, perl = TRUE) & .grep_non_smoothers(fe)]
  rand <- fe[grepl(pattern = "^b\\[", fe, perl = TRUE)]

  l <- compact_list(list(
    conditional = cond,
    random = rand
  ))

  x1 <- sub(pattern = "(.*)(\\|)(.*)", "\\1", l$conditional)
  x2 <- sub(pattern = "(.*)(\\|)(.*)", "\\3", l$conditional)

  l.cond <- lapply(rn, function(i) {
    list(conditional = x2[which(x1 == i)])
  })
  names(l.cond) <- rn

  x1 <- sub(pattern = "b\\[(.*)(\\|)(.*)", "\\1", l$random)
  x2 <- sub(pattern = "(b\\[).*(.*)(\\|)(.*)", "\\1\\4", l$random)

  l.random <- lapply(rn, function(i) {
    list(random = x2[which(x1 == i)])
  })
  names(l.random) <- rn

  l <- mapply(c, l.cond, l.random, SIMPLIFY = FALSE)
  attr(l, "is_mv") <- "1"

  .filter_pars(l, parameters)
}
