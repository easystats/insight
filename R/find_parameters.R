#' @title Find names of model parameters
#' @name find_parameters
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the \code{summary()} output. For Bayesian models, the parameter
#'     names equal the column names of the posterior samples after coercion
#'     from \code{as.data.frame()}. See the documentation for your object's class:
#'    \itemize{
#'      \item{\link[=find_parameters.BGGM]{Bayesian models} (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
#'      \item{\link[=get_parameters.emmGrid]{Estimated marginal means} (\pkg{emmeans})}
#'      \item{\link[=get_parameters.gamm]{Generalized additive models} (\pkg{mgcv}, \pkg{VGAM}, ...)}
#'      \item{\link[=find_parameters.betamfx]{Marginal effects models} (\pkg{mfx})}
#'      \item{\link[=find_parameters.glmmTMB]{Mixed models} (\pkg{lme4}, \pkg{glmmTMB}, \pkg{GLMMadaptive}, ...)}
#'      \item{\link[=get_parameters.zeroinfl]{Zero-inflated and hurdle models} (\pkg{pscl}, ...)}
#'      \item{\link[=get_parameters.betareg]{Models with special components} (\pkg{betareg}, \pkg{MuMIn}, ...)}
#'    }
#'
#' @param parameters Regular expression pattern that describes the parameters that
#'   should be returned.
#' @param component Which type of parameters to return, such as parameters for the
#'    conditional model, the zero-inflated part of the model, the dispersion
#'    term, the instrumental variables or marginal effects be returned? Applies
#'    to models with zero-inflated and/or dispersion formula, or to models with
#'    instrumental variables (so called fixed-effects regressions), or models
#'    with marginal effects from \pkg{mfx}. May be abbreviated. Note that the
#'   \emph{conditional} component is also called \emph{count} or \emph{mean}
#'   component, depending on the model. There are two convenient shortcuts:
#'   If \code{component = "location"}, location parameters such as \code{conditional},
#'   \code{zero_inflated}, \code{smooth_terms}, or \code{instruments} are returned.
#'   For \code{component = "distributional"} (or \code{"auxiliary"}), components
#'   like \code{sigma}, \code{dispersion}, \code{beta} or \code{precision} (and
#'   other auxiliary parameters) are returned.
#' @param verbose Toggle messages and warnings.
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
#'      \item \code{simplex}, simplex parameters of monotonic effects (\pkg{brms} only)
#'      \item \code{smooth_terms}, the smooth parameters
#'      \item \code{marginal}, the marginal effects (for models from \pkg{mfx})
#'      \item \code{sigma}, the residual standard deviation (auxiliary parameter)
#'      \item \code{dispersion}, the dispersion parameters (auxiliary parameter)
#'      \item \code{beta}, the beta parameter (auxiliary parameter)
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



# Default methods -------------------------------------------


#' @rdname find_parameters
#' @export
find_parameters.default <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    pars <- find_parameters.gam(x)
  } else {
    pars <- tryCatch(
      {
        p <- .remove_backticks_from_string(names(stats::coef(x)))
        list(conditional = p)
      },
      error = function(x) {
        NULL
      }
    )
  }


  if (is.null(pars$conditional) || is.null(pars)) {
    if (isTRUE(verbose)) {
      warning(sprintf("Parameters can't be retrieved for objects of class '%s'.", class(x)[1]), call. = FALSE)
    }
    return(NULL)
  }


  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.data.frame <- function(x, flatten = FALSE, ...) {
  stop("A data frame is no valid object for this function.")
}



#' @export
find_parameters.summary.lm <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  l <- list(conditional = .remove_backticks_from_string(rownames(cf)))

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}




# Ordinal -----------------------------------------------


#' @export
find_parameters.polr <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = c(sprintf("Intercept: %s", names(x$zeta)), names(stats::coef(x))))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.clm2 <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(x)
  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  if (n_scale == 0) {
    pars <- list(conditional = names(cf))
    pars$conditional <- .remove_backticks_from_string(pars$conditional)
  } else {
    pars <- .compact_list(list(
      conditional = names(cf)[1:(n_intercepts + n_location)],
      scale = names(cf)[(1 + n_intercepts + n_location):(n_scale + n_intercepts + n_location)]
    ))
    pars <- rapply(pars, .remove_backticks_from_string, how = "list")
  }

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.clmm2 <- find_parameters.clm2



#' @export
find_parameters.bracl <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.betareg <- function(x, flatten = FALSE, ...) {
  pars <- list(
    conditional = names(x$coefficients$mean),
    precision = names(x$coefficients$precision)
  )

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.DirichletRegModel <- function(x, flatten = FALSE, ...) {
  if (x$parametrization == "common") {
    pars <- list(conditional = names(unlist(stats::coef(x))))
  } else {
    pars <- .compact_list(list(
      conditional = names(unlist(stats::coef(x)[["beta"]])),
      precision = names(unlist(stats::coef(x)[["gamma"]]))
    ))
    pars$precision <- .remove_backticks_from_string(pars$precision)
  }

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}





#' @export
find_parameters.mixor <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  effects <- match.arg(effects)
  coefs <- x$Model
  random_start <- grep("(\\(Intercept\\) \\(Intercept\\)|Random\\.\\(Intercept\\))", rownames(coefs))
  thresholds <- grep("Threshold\\d", rownames(coefs))

  l <- list(
    conditional = rownames(coefs)[c(1, thresholds, 2:(random_start - 1))],
    random = rownames(coefs)[random_start:(thresholds[1] - 1)]
  )

  .filter_parameters(l, effects = effects, flatten = flatten)
}



#' @export
find_parameters.multinom <- function(x, flatten = FALSE, ...) {
  params <- stats::coef(x)


  pars <- if (is.matrix(params)) {
    list(conditional = colnames(params))
  } else {
    list(conditional = names(params))
  }

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.brmultinom <- find_parameters.multinom







# GAM (additive models) ---------------------------------------------


#' @importFrom stats na.omit coef
#' @export
find_parameters.gamlss <- function(x, flatten = FALSE, ...) {
  pars <- lapply(x$parameters, function(i) {
    .remove_backticks_from_string(names(stats::na.omit(stats::coef(x, what = i))))
  })

  names(pars) <- x$parameters
  if ("mu" %in% names(pars)) names(pars)[1] <- "conditional"

  pars <- .compact_list(pars)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @rdname find_parameters
#' @export
find_parameters.gam <- function(x, component = c("all", "conditional", "smooth_terms", "location"), flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  st <- summary(x)$s.table

  pars$conditional <- pars$conditional[.grep_non_smoothers(pars$conditional)]
  pars$smooth_terms <- row.names(st)

  pars <- .compact_list(pars)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  pars <- .compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.scam <- find_parameters.gam



#' @export
find_parameters.Gam <- function(x, component = c("all", "conditional", "smooth_terms", "location"), flatten = FALSE, ...) {
  pars <- names(stats::coef(x))
  component <- match.arg(component)

  l <- .compact_list(list(
    conditional = pars[.grep_non_smoothers(pars)],
    smooth_terms = pars[.grep_smoothers(pars)]
  ))

  .filter_parameters(l, effects = "all", component = component, flatten = flatten, recursive = TRUE)
}


#' @export
find_parameters.vgam <- find_parameters.Gam



#' @export
find_parameters.gamm <- function(x, component = c("all", "conditional", "smooth_terms", "location"), flatten = FALSE, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  component <- match.arg(component)

  l <- find_parameters.gam(x, component = component)

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_parameters.cgam <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, ...) {
  component <- match.arg(component)
  sc <- summary(x)

  estimates <- sc$coefficients
  smooth_terms <- sc$coefficients2

  l <- .compact_list(list(
    conditional = rownames(estimates),
    smooth_terms = rownames(smooth_terms)
  ))

  l <- lapply(l, .remove_backticks_from_string)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}






# zero-inflated models --------------------------------------------

#' @rdname find_parameters
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
find_parameters.zerotrunc <- find_parameters.default


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








# SEM models ------------------------------------------------------

#' @export
find_parameters.blavaan <- function(x, flatten = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  pars <- data.frame(
    pars = names(lavaan::coef(x)),
    comp = NA,
    stringsAsFactors = FALSE
  )

  pars$comp[grepl("~", pars$pars, fixed = TRUE)] <- "regression"
  pars$comp[grepl("=~", pars$pars, fixed = TRUE)] <- "latent"
  pars$comp[grepl("~~", pars$pars, fixed = TRUE)] <- "residual"
  pars$comp[grepl("~1", pars$pars, fixed = TRUE)] <- "intercept"

  pos_latent <- grep("=~", pars$pars, fixed = TRUE)
  pos_residual <- grep("~~", pars$pars, fixed = TRUE)
  pos_intercept <- grep("~1", pars$pars, fixed = TRUE)
  pos_regression <- setdiff(1:nrow(pars), c(pos_latent, pos_residual, pos_intercept))

  pos <- c(min(pos_latent), min(pos_residual), min(pos_intercept), min(pos_regression))

  comp_levels <- c("latent", "residual", "intercept", "regression")
  comp_levels <- comp_levels[order(pos)]

  pars$comp <- factor(pars$comp, levels = comp_levels)
  pars <- split(pars, pars$comp)
  pars <- lapply(pars, function(i) i$pars)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.lavaan <- function(x, flatten = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  pars <- get_parameters(x)
  pars$Component <- factor(pars$Component, levels = unique(pars$Component))
  pars <- split(pars$Parameter, pars$Component)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}




# Panel models ----------------------------------------


#' @export
find_parameters.wbm <- function(x, flatten = FALSE, ...) {
  s <- summary(x)

  pars <- .compact_list(list(
    conditional = rownames(s$within_table),
    instruments = rownames(s$between_table),
    random = rownames(s$ints_table)
  ))

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.wbgee <- find_parameters.wbm






# Other models -----------------------------------


#' @export
find_parameters.rms <- find_parameters.default


#' @export
find_parameters.tobit <- find_parameters.default


#' @importFrom utils capture.output
#' @importFrom stats coef
#' @export
find_parameters.riskRegression <- function(x, flatten = FALSE, ...) {
  junk <- utils::capture.output(cs <- stats::coef(x))
  out <- list(conditional = as.vector(cs[, 1]))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.lmodel2 <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = c("Intercept", "Slope"))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.ivFixed <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = rownames(x$coefficients))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.ivprobit <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = x$names)

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.mediate <- function(x, flatten = FALSE, ...) {
  info <- model_info(x$model.y)
  if (info$is_linear && !x$INT) {
    out <-
      list(conditional = c("ACME", "ADE", "Total Effect", "Prop. Mediated"))
  } else {
    out <- list(
      conditional = c(
        "ACME (control)",
        "ACME (treated)",
        "ADE (control)",
        "ADE (treated)",
        "Total Effect",
        "Prop. Mediated (control)",
        "Prop. Mediated (treated)",
        "ACME (average)",
        "ADE (average)",
        "Prop. Mediated (average)"
      )
    )
  }
  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.ridgelm <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = names(x$coef))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.survreg <- function(x, flatten = FALSE, ...) {
  s <- summary(x)
  out <- list(conditional = rownames(s$table))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.mle2 <- function(x, flatten = FALSE, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract parameter names.", call. = FALSE)
  }
  s <- bbmle::summary(x)
  out <- list(conditional = rownames(s@coef))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}

#' @export
find_parameters.mle <- find_parameters.mle2


#' @export
find_parameters.glht <- function(x, flatten = FALSE, ...) {
  s <- summary(x)
  alt <- switch(
    x$alternative,
    two.sided = "==",
    less = ">=",
    greater = "<="
  )

  l <- list(conditional = paste(names(s$test$coefficients), alt, x$rhs))

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_parameters.emmGrid <- function(x, flatten = TRUE, ...) {
  s <- summary(x)
  estimate_pos <- which(colnames(s) == x@misc$estName)
  out <- list(conditional = colnames(s)[1:(estimate_pos - 1)])

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.emm_list <- function(x, flatten = TRUE, ...) {
  s <- summary(x)[[1]]
  estimate_pos <- which(colnames(s) == x[[1]]@misc$estName)
  out <- list(conditional = colnames(s)[1:(estimate_pos - 1)])

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @importFrom stats na.omit coef
#' @export
find_parameters.manova <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = .remove_backticks_from_string(rownames(stats::na.omit(stats::coef(x)))))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}

#' @export
find_parameters.maov <- find_parameters.manova



#' @rdname find_parameters
#' @export
find_parameters.averaging <- function(x, component = c("conditional", "full"), flatten = FALSE, ...) {
  component <- match.arg(component)
  cf <- stats::coef(x, full = component == "full")
  out <- list(conditional = .remove_backticks_from_string(names(cf)))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.afex_aov <- function(x, flatten = FALSE, ...) {
  if ("aov" %in% names(x)) {
    find_parameters(x$aov, flatten = flatten, ...)
  } else {
    find_parameters(x$lm, flatten = flatten, ...)
  }
}


#' @export
find_parameters.mlm <- function(x, flatten = FALSE, ...) {
  cs <- stats::coef(summary(x))

  out <- lapply(cs, function(i) {
    list(conditional = .remove_backticks_from_string(rownames(i)))
  })

  names(out) <- gsub("^Response (.*)", "\\1", names(cs))
  attr(out, "is_mv") <- TRUE

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @export
find_parameters.glmx <- function(x, flatten = FALSE, ...) {
  cf <- stats::coef(summary(x))

  out <- list(
    conditional = .remove_backticks_from_string(names(cf$glm[, 1])),
    extra = .remove_backticks_from_string(rownames(cf$extra))
  )

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @export
find_parameters.gbm <- function(x, flatten = FALSE, ...) {
  s <- summary(x, plotit = FALSE)
  pars <- list(conditional = as.character(s$var))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.BBreg <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = rownames(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.lrm <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.flexsurvreg <- find_parameters.lrm



#' @export
find_parameters.BBmm <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  l <- .compact_list(list(
    conditional = names(x$fixed.coef),
    random = x$namesRand
  ))

  effects <- match.arg(effects)
  .filter_parameters(l, effects = effects, flatten = flatten, recursive = FALSE)
}



#' @export
find_parameters.glimML <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  l <- .compact_list(list(
    conditional = names(x@fixed.param),
    random = names(x@random.param)
  ))

  effects <- match.arg(effects)
  .filter_parameters(l, effects = effects, flatten = flatten, recursive = FALSE)
}



#' @export
find_parameters.aovlist <- function(x, flatten = FALSE, ...) {
  l <- list(conditional = unname(.remove_backticks_from_string(unlist(lapply(stats::coef(x), names)))))

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.rqs <- function(x, flatten = FALSE, ...) {
  sc <- suppressWarnings(summary(x))

  if (all(unlist(lapply(sc, is.list)))) {
    pars <- list(conditional = rownames(stats::coef(sc[[1]])))
  } else {
    return(find_parameters.default(x, flatten = flatten, ...))
  }
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.crq <- function(x, flatten = FALSE, ...) {
  sc <- suppressWarnings(summary(x))

  if (all(unlist(lapply(sc, is.list)))) {
    pars <- list(conditional = rownames(sc[[1]]$coefficients))
  } else {
    pars <- list(conditional = rownames(sc$coefficients))
  }
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.crqs <- find_parameters.crq



#' @importFrom stats coef
#' @export
find_parameters.lqmm <- function(x, flatten = FALSE, ...) {
  cs <- stats::coef(x)

  if (is.matrix(cs)) {
    pars <- list(conditional = rownames(cs))
  } else {
    pars <- list(conditional = names(cs))
  }
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.lqm <- find_parameters.lqmm



#' @export
find_parameters.rqss <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(
    conditional = rownames(sc$coef),
    smooth_terms = rownames(sc$qsstab)
  )

  pars$conditional <- .remove_backticks_from_string(pars$conditional)
  pars$smooth_terms <- .remove_backticks_from_string(pars$smooth_terms)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component)
  pars <- .compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.aareg <- function(x, flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(conditional = rownames(sc$table))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.rma <- function(x, flatten = FALSE, ...) {
  tryCatch(
    {
      cf <- stats::coef(x)
      pars <- list(conditional = names(cf))

      pars$conditional[grepl("intrcpt", pars$conditional)] <- "(Intercept)"
      pars$conditional <- .remove_backticks_from_string(pars$conditional)

      if (flatten) {
        unique(unlist(pars))
      } else {
        pars
      }
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
find_parameters.meta_random <- function(x, flatten = FALSE, ...) {
  tryCatch(
    {
      cf <- x$estimates
      pars <- list(conditional = rownames(cf))
      pars$conditional[pars$conditional == "d"] <- "(Intercept)"

      if (flatten) {
        unique(unlist(pars))
      } else {
        pars
      }
    },
    error = function(x) {
      NULL
    }
  )
}


#' @export
find_parameters.meta_fixed <- find_parameters.meta_random

#' @export
find_parameters.meta_bma <- find_parameters.meta_random



#' @export
find_parameters.metaplus <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = rownames(x$results))
  pars$conditional[grepl("muhat", pars$conditional)] <- "(Intercept)"
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.mipo <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = as.vector(summary(x)$term))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.mira <- function(x, flatten = FALSE, ...) {
  find_parameters(x$analyses[[1]], flatten = flatten, ...)
}






# helper ----------------------------

.filter_parameters <- function(l, effects, component = "all", flatten, recursive = TRUE) {
  if (isTRUE(recursive)) {
    # recursively remove back-ticks from all list-elements parameters
    l <- rapply(l, .remove_backticks_from_string, how = "list")
  } else {
    l <- lapply(l, .remove_backticks_from_string)
  }

  # keep only requested effects
  elements <- .get_elements(effects, component = component)

  # remove empty list-elements
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}
