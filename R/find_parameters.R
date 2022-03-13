#' @title Find names of model parameters
#' @name find_parameters
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the `summary()` output. For Bayesian models, the parameter
#'     names equal the column names of the posterior samples after coercion
#'     from `as.data.frame()`. See the documentation for your object's class:
#'    \itemize{
#'      \item{[Bayesian models][find_parameters.BGGM] (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
#'      \item{[Generalized additive models][find_parameters.gamlss] (\pkg{mgcv}, \pkg{VGAM}, ...)}
#'      \item{[Marginal effects models][find_parameters.betamfx] (\pkg{mfx})}
#'      \item{[Estimated marginal means][find_parameters.emmGrid] (\pkg{emmeans})}
#'      \item{[Mixed models][find_parameters.glmmTMB] (\pkg{lme4}, \pkg{glmmTMB}, \pkg{GLMMadaptive}, ...)}
#'      \item{[Zero-inflated and hurdle models][find_parameters.zeroinfl] (\pkg{pscl}, ...)}
#'      \item{[Models with special components][find_parameters.averaging] (\pkg{betareg}, \pkg{MuMIn}, ...)}
#'    }
#'
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. For simple models, only one list-element,
#'    `conditional`, is returned.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @export
find_parameters <- function(x, ...) {
  UseMethod("find_parameters")
}



# Default methods -------------------------------------------


#' @rdname find_parameters
#' @export
find_parameters.default <- function(x, flatten = FALSE, verbose = TRUE, ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    pars <- find_parameters.gam(x)
  } else {
    pars <- tryCatch(
      {
        p <- text_remove_backticks(names(stats::coef(x)))
        list(conditional = p)
      },
      error = function(x) {
        NULL
      }
    )
  }


  if (is.null(pars$conditional) || is.null(pars)) {
    if (isTRUE(verbose)) {
      warning(format_message(sprintf("Parameters can't be retrieved for objects of class '%s'.", class(x)[1])), call. = FALSE)
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
  l <- list(conditional = text_remove_backticks(rownames(cf)))

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
  pars$conditional <- text_remove_backticks(pars$conditional)

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
    pars$conditional <- text_remove_backticks(pars$conditional)
  } else {
    pars <- compact_list(list(
      conditional = names(cf)[1:(n_intercepts + n_location)],
      scale = names(cf)[(1 + n_intercepts + n_location):(n_scale + n_intercepts + n_location)]
    ))
    pars <- rapply(pars, text_remove_backticks, how = "list")
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
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.multinom <- function(x, flatten = FALSE, ...) {
  params <- stats::coef(x)


  pars <- if (is.matrix(params)) {
    list(conditional = colnames(params))
  } else {
    list(conditional = names(params))
  }

  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.brmultinom <- find_parameters.multinom


# SEM models ------------------------------------------------------

#' @export
find_parameters.blavaan <- function(x, flatten = FALSE, ...) {
  # installed?
  check_if_installed("lavaan")

  param_tab <- lavaan::parameterEstimates(x)
  params <- paste0(param_tab$lhs, param_tab$op, param_tab$rhs)

  coef_labels <- names(lavaan::coef(x))

  if ("group" %in% colnames(param_tab) && .n_unique(param_tab$group) > 1) {
    params <- paste0(params, " (group ", param_tab$group, ")")
    groups <- grepl("(.*)\\.g(.*)", coef_labels)
    coef_labels[!groups] <- paste0(coef_labels[!groups], " (group 1)")
    coef_labels[groups] <- gsub("(.*)\\.g(.*)", "\\1 \\(group \\2\\)", coef_labels[groups])
  }

  are_labels <- !coef_labels %in% params
  if (any(are_labels)) {
    unique_labels <- unique(coef_labels[are_labels])
    for (ll in seq_along(unique_labels)) {
      coef_labels[coef_labels == unique_labels[ll]] <-
        params[param_tab$label == unique_labels[ll]]
    }
  }

  pars <- data.frame(
    pars = coef_labels,
    comp = NA,
    stringsAsFactors = FALSE
  )

  pars$comp[grepl("=~", pars$pars, fixed = TRUE)] <- "latent"
  pars$comp[grepl("~~", pars$pars, fixed = TRUE)] <- "residual"
  pars$comp[grepl("~1", pars$pars, fixed = TRUE)] <- "intercept"
  pars$comp[is.na(pars$comp)] <- "regression"

  pars$comp <- factor(pars$comp, levels = unique(pars$comp))
  pars <- split(pars, pars$comp)
  pars <- compact_list(lapply(pars, function(i) i$pars))

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.lavaan <- function(x, flatten = FALSE, ...) {
  # installed?
  check_if_installed("lavaan")

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
find_parameters.pgmm <- function(x,
                                 component = c("conditional", "all"),
                                 flatten = FALSE,
                                 ...) {
  component <- match.arg(component)
  s <- summary(x, robust = FALSE)

  l <- list(
    conditional = rownames(s$coefficients),
    time_dummies = x$args$namest
  )

  .filter_parameters(
    l,
    effects = "all",
    component = component,
    flatten = flatten,
    recursive = FALSE
  )
}


#' @export
find_parameters.wbm <- function(x, flatten = FALSE, ...) {
  s <- summary(x)

  pars <- compact_list(list(
    conditional = rownames(s$within_table),
    instruments = rownames(s$between_table),
    random = rownames(s$ints_table)
  ))

  pars$conditional <- text_remove_backticks(pars$conditional)

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


#' @export
find_parameters.Rchoice <- function(x, flatten = FALSE, ...) {
  cf <- names(stats::coef(x))
  if (cf[1] == "constant") {
    cf[1] <- "(Intercept)"
  }
  out <- list(conditional = cf)

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.btergm <- function(x, flatten = FALSE, ...) {
  cf <- x@coef
  out <- list(conditional = names(cf))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


#' @export
find_parameters.crr <- function(x, flatten = FALSE, ...) {
  cs <- x$coef
  out <- list(conditional = names(cs))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}


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
  # installed?
  check_if_installed("bbmle")

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
  alt <- switch(x$alternative,
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
find_parameters.manova <- function(x, flatten = FALSE, ...) {
  out <- list(conditional = text_remove_backticks(rownames(stats::na.omit(stats::coef(x)))))

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}

#' @export
find_parameters.maov <- find_parameters.manova



#' @export
find_parameters.afex_aov <- function(x, flatten = FALSE, ...) {
  if (!is.null(x$aov)) {
    find_parameters(x$aov, flatten = flatten, ...)
  } else {
    find_parameters(x$lm, flatten = flatten, ...)
  }
}


#' @export
find_parameters.mlm <- function(x, flatten = FALSE, ...) {
  cs <- stats::coef(summary(x))

  out <- lapply(cs, function(i) {
    list(conditional = text_remove_backticks(rownames(i)))
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
find_parameters.mvord <- function(x, flatten = FALSE, ...) {
  junk <- utils::capture.output(s <- summary(x))

  out <- list(
    thresholds = text_remove_backticks(rownames(s$thresholds)),
    conditional = text_remove_backticks(rownames(s$coefficients)),
    correlation = text_remove_backticks(rownames(s$error.structure))
  )
  attr(out, "is_mv") <- TRUE

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
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.BBreg <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = rownames(stats::coef(x)))
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.lrm <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.flexsurvreg <- find_parameters.lrm



#' @export
find_parameters.aovlist <- function(x, flatten = FALSE, ...) {
  l <- list(conditional = unname(text_remove_backticks(unlist(lapply(stats::coef(x), names)))))

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
  pars$conditional <- text_remove_backticks(pars$conditional)

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
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.crqs <- find_parameters.crq



#' @export
find_parameters.lqmm <- function(x, flatten = FALSE, ...) {
  cs <- stats::coef(x)

  if (is.matrix(cs)) {
    pars <- list(conditional = rownames(cs))
  } else {
    pars <- list(conditional = names(cs))
  }
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.lqm <- find_parameters.lqmm




#' @export
find_parameters.aareg <- function(x, flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(conditional = rownames(sc$table))
  pars$conditional <- text_remove_backticks(pars$conditional)

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
      pars$conditional <- text_remove_backticks(pars$conditional)

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
  pars$conditional <- text_remove_backticks(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.mipo <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = as.vector(summary(x)$term))
  pars$conditional <- text_remove_backticks(pars$conditional)

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

## For questions or problems with this ask Fernando Miguez (femiguez@iastate.edu)
#' @export
find_parameters.nls <- function(x, flatten = FALSE, ...) {
  f <- find_formula(x)
  elements <- .get_elements(effects = "fixed", component = "all")
  f <- .prepare_predictors(x, f, elements)
  pars <- .return_vars(f, x)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

# helper ----------------------------

.filter_parameters <- function(l, effects, component = "all", flatten, recursive = TRUE) {
  if (isTRUE(recursive)) {
    # recursively remove back-ticks from all list-elements parameters
    l <- rapply(l, text_remove_backticks, how = "list")
  } else {
    l <- lapply(l, text_remove_backticks)
  }

  # keep only requested effects
  elements <- .get_elements(effects, component = component)

  # remove empty list-elements
  l <- compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}
