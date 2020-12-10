#' @title Get model parameters
#' @name get_parameters
#'
#' @description Returns the coefficients (or posterior samples for Bayesian
#'    models) from a model.
#'
#' @param iterations Number of posterior draws.
#' @param progress Display progress.
#' @param summary Logical, indicates whether the full posterior samples
#'   (\code{summary = FALSE})) or the summarized centrality indices of
#'   the posterior samples (\code{summary = TRUE})) should be returned as
#'   estimates.
#' @param centrality Only for models with posterior samples, and when
#'   \code{summary = TRUE}. In this case, \code{centrality = "mean"} would
#'   calculate means of posterior samples for each parameter, while
#'   \code{centrality = "median"} would use the more robust median value as
#'   measure of central tendency.
#' @param verbose Toggle messages and warnings.
#' @param merge_parameters Logical, if \code{TRUE} and \code{x} has multiple
#'   columns for parameter names (like \code{emmGrid} objects may have), these
#'   are merged into a single parameter column, with parameters names and values
#'   as values.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return \itemize{
#'   \item for non-Bayesian models and if \code{effects = "fixed"}, a data frame with two columns: the parameter names and the related point estimates
#'   \item if \code{effects = "random"}, a list of data frames with the random effects (as returned by \code{ranef()}), unless the random effects have the same simplified structure as fixed effects (e.g. for models from \pkg{MCMCglmm})
#'   \item for Bayesian models, the posterior samples from the requested parameters as data frame
#'   \item for Anova (\code{aov()}) with error term, a list of parameters for the conditional and the random effects parameters
#'   \item for models with smooth terms or zero-inflation component, a data frame with three columns: the parameter names, the related point estimates and the component
#' }
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' \code{effects} and \code{component} can be used.
#' \cr \cr
#' \code{get_parameters()} is comparable to \code{coef()}, however, the coefficients
#' are returned as data frame (with columns for names and point estimates of
#' coefficients). For Bayesian models, the posterior samples of parameters are
#' returned.
#'
#' @section BFBayesFactor Models:
#' Note that for \code{BFBayesFactor} models (from the \pkg{BayesFactor}
#' package), posteriors are only extracted from the first numerator model (i.e.,
#' \code{model[1]}). If you want to apply some function \code{foo()} to another
#' model stored in the \code{BFBayesFactor} object, index it directly, e.g.
#' \code{foo(model[2])}, \code{foo(1/model[5])}, etc.
#' See also \code{\link[bayestestR]{weighted_posteriors}}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @importFrom stats coef
#' @export
get_parameters <- function(x, ...) {
  UseMethod("get_parameters")
}



# Default models ---------------------------------------------


#' @export
get_parameters.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    return(get_parameters.gam(x, ...))
  }

  tryCatch(
    {
      cf <- stats::coef(x)

      params <- data.frame(
        Parameter = names(cf),
        Estimate = unname(cf),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      .remove_backticks_from_parameter_names(params)
    },
    error = function(x) {
      if (isTRUE(verbose)) {
        warning(sprintf("Parameters can't be retrieved for objects of class '%s'.", class(x)[1]), call. = FALSE)
      }
      return(NULL)
    }
  )
}


#' @export
get_parameters.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}



# Survival and censored  models ---------------------------------------------


#' @export
get_parameters.flexsurvreg <- function(x, ...) {
  cf <- stats::coef(x)
  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.aareg <- function(x, ...) {
  sc <- summary(x)

  params <- data.frame(
    Parameter = rownames(sc$table),
    Estimate = unname(sc$table[, 2]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.crq <- function(x, ...) {
  sc <- summary(x)

  if (all(unlist(lapply(sc, is.list)))) {
    list_sc <- lapply(sc, function(i) {
      .x <- as.data.frame(i)
      .x$Parameter <- rownames(.x)
      .x
    })
    out <- do.call(rbind, list_sc)
    params <- data.frame(
      Parameter = out$Parameter,
      Estimate = out$coefficients.Value,
      Component = sprintf("tau (%g)", out$tau),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    params <- data.frame(
      Parameter = names(sc$coefficients[, 1]),
      Estimate = unname(sc$coefficients[, 1]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  .remove_backticks_from_parameter_names(params)
}

#' @export
get_parameters.crqs <- get_parameters.crq


#' @importFrom stats coef
#' @export
get_parameters.lqmm <- function(x, ...) {
  cs <- stats::coef(x)

  if (is.matrix(cs)) {
    params <- .gather(as.data.frame(cs), names_to = "Component", values_to = "Estimate")
    params$Component <- sprintf("tau (%s)", params$Component)
    params$Parameter <- rep(rownames(cs), length.out = nrow(params))
    params <- params[c("Parameter", "Estimate", "Component")]
    row.names(params) <- NULL
  } else {
    params <- data.frame(
      Parameter = names(cs),
      Estimate = unname(cs),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  .remove_backticks_from_parameter_names(params)
}

#' @export
get_parameters.lqm <- get_parameters.lqmm



#' @importFrom stats setNames
#' @export
get_parameters.rqss <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  sc <- summary(x)

  smooth_terms <- sc$qsstab[, 3]
  names(smooth_terms) <- rownames(sc$qsstab)

  .return_smooth_parms(
    conditional = stats::setNames(sc$coef[, 1], rownames(sc$coef)),
    smooth_terms = smooth_terms,
    component = component
  )
}


#' @importFrom stats setNames
#' @export
get_parameters.cgam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  sc <- summary(x)

  estimates <- sc$coefficients
  smooth_terms <- sc$coefficients2

  if (!is.null(smooth_terms)) smooth_terms <- stats::setNames(smooth_terms[, 1], rownames(smooth_terms))

  .return_smooth_parms(
    conditional = stats::setNames(estimates[, 1], rownames(estimates)),
    smooth_terms = smooth_terms,
    component = component
  )
}







# mfx models ---------------------------------------------


#' @rdname get_parameters
#' @export
get_parameters.betamfx <- function(x, component = c("all", "conditional", "precision", "marginal"), ...) {
  component <- match.arg(component)
  params <- get_parameters.betareg(x$fit, component = "all", ...)
  mfx <- x$mfxest

  params <- rbind(
    data.frame(
      Parameter = gsub("^\\(phi\\)_", "", rownames(mfx)),
      Estimate = as.vector(mfx[, 1]),
      Component = "marginal",
      stringsAsFactors = FALSE
    ),
    params
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @export
get_parameters.betaor <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  get_parameters.betareg(x$fit, component = component, ...)
}



#' @rdname get_parameters
#' @export
get_parameters.logitmfx <- function(x, component = c("all", "conditional", "marginal"), ...) {
  params <- get_parameters.default(x$fit, ...)
  params$Component <- "conditional"
  mfx <- x$mfxest

  params <- rbind(
    data.frame(
      Parameter = rownames(mfx),
      Estimate = as.vector(mfx[, 1]),
      Component = "marginal",
      stringsAsFactors = FALSE
    ),
    params
  )

  component <- match.arg(component)
  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}

#' @export
get_parameters.poissonmfx <- get_parameters.logitmfx

#' @export
get_parameters.negbinmfx <- get_parameters.logitmfx

#' @export
get_parameters.probitmfx <- get_parameters.logitmfx

#' @export
get_parameters.logitor <- function(x, ...) {
  get_parameters.default(x$fit, ...)
}

#' @export
get_parameters.poissonirr <- get_parameters.logitor

#' @export
get_parameters.negbinirr <- get_parameters.logitor








# Special models ---------------------------------------------


#' @export
get_parameters.rms <- get_parameters.default


#' @export
get_parameters.tobit <- get_parameters.default


#' @export
get_parameters.mediate <- function(x, ...) {
  info <- model_info(x$model.y)
  if (info$is_linear && !x$INT) {
    out <- data.frame(
      Parameter = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      Estimate = c(x$d1, x$z0, x$tau.coef, x$n0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      Parameter = c(
        "ACME (control)", "ACME (treated)", "ADE (control)",
        "ADE (treated)", "Total Effect", "Prop. Mediated (control)",
        "Prop. Mediated (treated)", "ACME (average)", "ADE (average)",
        "Prop. Mediated (average)"
      ),
      Estimate = c(x$d0, x$d1, x$z0, x$z1, x$tau.coef, x$n0, x$n1, x$d.avg, x$z.avg, x$n.avg),
      stringsAsFactors = FALSE
    )
  }
  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.ridgelm <- function(x, ...) {
  out <- data.frame(
    Parameter = names(x$coef),
    Estimate = as.vector(x$coef),
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.survreg <- function(x, ...) {
  s <- summary(x)
  out <- data.frame(
    Parameter = rownames(s$table),
    Estimate = as.vector(s$table[, 1]),
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.mipo <- function(x, ...) {
  out <- data.frame(
    Parameter = as.vector(summary(x)$term),
    Estimate = as.vector(summary(x)$estimate),
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.mira <- function(x, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package `mice` required. Please install it.", call. = FALSE)
  }
  get_parameters(mice::pool(x), ...)
}


#' @export
get_parameters.margins <- function(x, ...) {
  s <- summary(x)
  param <- as.vector(s$factor)
  estimate_pos <- which(colnames(s) == "AME")

  if (estimate_pos > 2) {
    out <- s[1:(estimate_pos - 1)]
    r <- apply(out, 1, function(i) paste0(colnames(out), " [", i, "]"))
    param <- unname(sapply(as.data.frame(r), paste, collapse = ", "))
  }

  out <- data.frame(
    Parameter = param,
    Estimate = as.vector(summary(x)$AME),
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.glht <- function(x, ...) {
  s <- summary(x)
  alt <- switch(
    x$alternative,
    two.sided = "==",
    less = ">=",
    greater = "<="
  )
  out <- data.frame(
    Parameter = paste(names(s$test$coefficients), alt, x$rhs),
    Estimate = unname(s$test$coefficients),
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}


#' @rdname get_parameters
#' @export
get_parameters.emmGrid <- function(x, summary = FALSE, merge_parameters = FALSE, ...) {
  # check if we have a Bayesian model here
  if (!.is_baysian_emmeans(x) || isTRUE(summary)) {
    s <- summary(x)
    estimate_pos <- which(colnames(s) == x@misc$estName)
    params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    if (isTRUE(merge_parameters) && ncol(params) > 1) {
      r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
      out <- data.frame(
        Parameter = unname(sapply(as.data.frame(r), paste, collapse = ", ")),
        Estimate = s[[estimate_pos]],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    } else {
      out <- data.frame(
        params,
        Estimate = s[[estimate_pos]],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      if (isTRUE(merge_parameters)) {
        colnames(out)[1] <- "Parameter"
      }
    }
    .remove_backticks_from_parameter_names(out)
  } else {
    .clean_emmeans_draws(x)
  }
}


#' @export
get_parameters.emm_list <- function(x, summary = FALSE, ...) {
  if (!.is_baysian_emmeans(x) || isTRUE(summary)) {
    do.call(rbind, lapply(names(x), function(i) {
      out <- get_parameters(x[[i]], summary = summary)
      if (ncol(out) > 2) {
        est <- out$Estimate
        out$Estimate <- NULL
        r <- apply(out, 1, function(i) paste0(colnames(out), " [", i, "]"))
        out <- data.frame(
          Parameter = unname(sapply(as.data.frame(r), paste, collapse = ", ")),
          Estimate = unname(est),
          stringsAsFactors = FALSE
        )
      }
      out$Component <- i
      colnames(out)[1] <- "Parameter"
      out
    }))
  } else {
    do.call(cbind, lapply(names(x), function(i) {
      .clean_emmeans_draws(x[[i]])
    }))
  }
}


#' @export
get_parameters.mle2 <- function(x, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract parameters.", call. = FALSE)
  }
  s <- bbmle::summary(x)

  params <- data.frame(
    Parameter = names(s@coef[, 1]),
    Estimate = unname(s@coef[, 1]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}

#' @export
get_parameters.mle <- get_parameters.mle2



#' @rdname get_parameters
#' @export
get_parameters.averaging <- function(x, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x, full = component == "full")

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.lrm <- function(x, ...) {
  tryCatch(
    {
      cf <- stats::coef(x)

      params <- data.frame(
        Parameter = names(cf),
        Estimate = unname(cf),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      .remove_backticks_from_parameter_names(params)
    },
    error = function(x) {
      NULL
    }
  )
}


#' @export
get_parameters.orm <- get_parameters.lrm


#' @export
get_parameters.multinom <- function(x, ...) {
  params <- stats::coef(x)

  if (is.matrix(params)) {
    out <- data.frame()
    for (i in 1:nrow(params)) {
      out <- rbind(out, data.frame(
        Parameter = colnames(params),
        Estimate = unname(params[i, ]),
        Response = rownames(params)[i],
        stringsAsFactors = FALSE,
        row.names = NULL
      ))
    }
  } else {
    out <- data.frame(
      Parameter = names(params),
      Estimate = unname(params),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  .remove_backticks_from_parameter_names(out)
}


#' @export
get_parameters.brmultinom <- get_parameters.multinom



#' @export
get_parameters.glmx <- function(x, component = c("all", "conditional", "extra"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(summary(x))

  params <- rbind(
    data.frame(
      Parameter = names(cf$glm[, 1]),
      Estimate = unname(cf$glm[, 1]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = rownames(cf$extra),
      Estimate = cf$extra[, 1],
      Component = "extra",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @export
get_parameters.mlm <- function(x, ...) {
  cs <- stats::coef(summary(x))

  out <- lapply(names(cs), function(i) {
    params <- data.frame(
      Parameter = rownames(cs[[i]]),
      Estimate = cs[[i]][, 1],
      Response = gsub("^Response (.*)", "\\1", i),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    .remove_backticks_from_parameter_names(params)
  })

  do.call(rbind, out)
}


#' @export
get_parameters.gbm <- function(x, ...) {
  s <- summary(x, plotit = FALSE)

  params <- data.frame(
    Parameter = as.character(s$var),
    Estimate = s$rel.inf,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @rdname get_parameters
#' @export
get_parameters.betareg <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = gsub("^\\(phi\\)_", "", names(cf)),
    Estimate = unname(cf),
    Component = c(rep("conditional", length(x$coefficients$mean)), rep("precision", length(x$coefficients$precision))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @rdname get_parameters
#' @export
get_parameters.DirichletRegModel <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  if (x$parametrization == "common") {
    component <- "all"
    n_comp <- lapply(cf, length)
    pattern <- paste0("(", paste(x$varnames, collapse = "|"), ")\\.(.*)")
    p_names <- gsub(pattern, "\\2", names(unlist(cf)))

    params <- data.frame(
      Parameter = p_names,
      Estimate = unname(unlist(cf)),
      Response = rep(names(n_comp), sapply(n_comp, function(i) i)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    out1 <- .gather(data.frame(do.call(cbind, cf$beta)), names_to = "Response", values_to = "Estimate")
    out2 <- .gather(data.frame(do.call(cbind, cf$gamma)), names_to = "Component", values_to = "Estimate")
    out1$Component <- "conditional"
    out2$Component <- "precision"
    out2$Response <- NA
    params <- merge(out1, out2, all = TRUE, sort = FALSE)
    params$Parameter <- gsub("(.*)\\.(.*)\\.(.*)", "\\3", names(unlist(cf)))
    params <- params[c("Parameter", "Estimate", "Component", "Response")]
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @export
get_parameters.BBreg <- function(x, ...) {
  pars <- summary(x)$coefficients

  params <- data.frame(
    Parameter = rownames(pars),
    Estimate = pars[, "Estimate"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.rma <- function(x, ...) {
  tryCatch(
    {
      cf <- stats::coef(x)

      params <- data.frame(
        Parameter = names(cf),
        Estimate = unname(cf),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      params$Parameter[grepl("intrcpt", params$Parameter)] <- "(Intercept)"
      .remove_backticks_from_parameter_names(params)
    },
    error = function(x) {
      NULL
    }
  )
}


#' @export
get_parameters.meta_random <- function(x, ...) {
  tryCatch(
    {
      cf <- x$estimates

      params <- data.frame(
        Parameter = rownames(cf),
        Estimate = unname(cf[, 1]),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      params$Parameter[grepl("d", params$Parameter)] <- "(Intercept)"
      .remove_backticks_from_parameter_names(params)
    },
    error = function(x) {
      NULL
    }
  )
}


#' @export
get_parameters.meta_fixed <- get_parameters.meta_random

#' @export
get_parameters.meta_bma <- get_parameters.meta_random


#' @export
get_parameters.metaplus <- function(x, ...) {
  params <- data.frame(
    Parameter = rownames(x$results),
    Estimate = unname(x$results[, 1]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params$Parameter[grepl("muhat", params$Parameter)] <- "(Intercept)"
  .remove_backticks_from_parameter_names(params)
}






# SEM models ---------------------------------------------


#' @export
get_parameters.blavaan <- function(x, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  if (!requireNamespace("blavaan", quietly = TRUE)) {
    stop("Package 'blavaan' required for this function to work. Please install it.")
  }

  draws <- blavaan::blavInspect(x, "draws")
  posteriors <- as.data.frame(as.matrix(draws))

  names(posteriors) <- names(lavaan::coef(x))
  posteriors
}



#' @export
get_parameters.lavaan <- function(x, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  params <- lavaan::parameterEstimates(x)

  params$parameter <- paste0(params$lhs, params$op, params$rhs)
  params$comp <- NA

  params$comp[params$op == "~"] <- "regression"
  params$comp[params$op == "=~"] <- "latent"
  params$comp[params$op == "~~"] <- "residual"
  params$comp[params$op == "~1"] <- "intercept"

  params <- data.frame(
    Parameter = params$parameter,
    Estimate = params$est,
    Component = params$comp,
    stringsAsFactors = FALSE
  )

  .remove_backticks_from_parameter_names(params)
}




# Ordinal models ---------------------------------------------


#' @export
get_parameters.polr <- function(x, ...) {
  pars <- c(sprintf("Intercept: %s", names(x$zeta)), names(x$coefficients))

  params <- data.frame(
    Parameter = pars,
    Estimate = c(unname(x$zeta), unname(x$coefficients)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.bracl <- function(x, ...) {
  pars <- stats::coef(x)

  params <- data.frame(
    Parameter = names(pars),
    Estimate = unname(pars),
    Response = gsub("(.*):(.*)", "\\1", names(pars)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @rdname get_parameters
#' @export
get_parameters.clm2 <- function(x, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  cf <- stats::coef(summary(x))
  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  params <- data.frame(
    Parameter = rownames(cf),
    Estimate = unname(cf[, "Estimate"]),
    Component = c(rep("conditional", times = n_intercepts + n_location), rep("scale", times = n_scale)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}






# Mixed models ---------------------------------------------


#' @export
get_parameters.glmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  params <- data.frame(
    Parameter = names(c(x$beta, x$nu)),
    Estimate = unname(c(x$beta, x$nu)),
    Effects = c(rep("fixed", times = length(x$beta)), rep("random", times = length(x$nu))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects != "all") {
    params <- params[params$Effects == effects, , drop = FALSE]
    params$Effects <- NULL
  }

  .remove_backticks_from_parameter_names(params)
}



#' @export
get_parameters.clmm2 <- get_parameters.clm2



#' @rdname get_parameters
#' @export
get_parameters.coxme <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- .compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.wbm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  if (effects == "fixed") {
    s <- summary(x)

    terms <- c(
      rownames(s$within_table),
      rownames(s$between_table),
      rownames(s$ints_table)
    )

    wt <- s$within_table
    bt <- s$between_table
    it <- s$ints_table

    if (!is.null(wt)) {
      wt <- data.frame(params = wt, component = "within", stringsAsFactors = FALSE)
    }
    if (!is.null(bt)) {
      bt <- data.frame(params = bt, component = "between", stringsAsFactors = FALSE)
    }
    if (!is.null(it)) {
      it <- data.frame(params = it, component = "interactions", stringsAsFactors = FALSE)
    }

    params <- rbind(wt, bt, it)

    out <- data.frame(
      Parameter = terms,
      Estimate = params[[1]],
      Component = params[["component"]],
      stringsAsFactors = FALSE
    )

    .remove_backticks_from_parameter_names(out)
  } else {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("To use this function, please install package 'lme4'.")
    }
    lme4::ranef(x)
  }
}


#' @export
get_parameters.wbgee <- function(x, ...) {
  get_parameters.wbm(x, effects = "fixed")
}



#' @export
get_parameters.nlmerMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  startvectors <- .get_startvector_from_env(x)
  fx <- lme4::fixef(x)

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors]
    ))
  } else {
    l <- .compact_list(list(
      conditional = fx[setdiff(names(fx), startvectors)],
      nonlinear = fx[startvectors],
      random = lapply(lme4::ranef(x), colnames)
    ))
  }


  fixed <- data.frame(
    Parameter = c(
      names(l$conditional),
      names(l$nonlinear)
    ),
    Estimate = c(unname(l$conditional), unname(l$nonlinear)),
    Component = c(
      rep("fixed", length(l$conditional)),
      rep("nonlinear", length(l$nonlinear))
    ),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @rdname get_parameters
#' @export
get_parameters.merMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    l <- .compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}

#' @export
get_parameters.rlmerMod <- get_parameters.merMod

#' @export
get_parameters.glmmadmb <- get_parameters.merMod

#' @export
get_parameters.lme <- get_parameters.merMod

#' @export
get_parameters.merModList <- function(x, ...) {
  s <- suppressWarnings(summary(x))
  fixed <- data.frame(
    Parameter = s$fe$term,
    Estimate = s$fe$estimate,
    stringsAsFactors = FALSE
  )

  .remove_backticks_from_parameter_names(fixed)
}

#' @export
get_parameters.HLfit <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x))
  } else {
    utils::capture.output(s <- summary(x))
    l <- .compact_list(list(
      conditional = lme4::fixef(x),
      random = lme4::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.sem <- function(x, effects = c("fixed", "random"), ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = x$coef)
  } else {
    l <- .compact_list(list(
      conditional = x$coef,
      random = x$ranef
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.cpglmm <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = cplm::fixef(x))
  } else {
    l <- .compact_list(list(
      conditional = cplm::fixef(x),
      random = cplm::ranef(x)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.mixed <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  if (effects == "fixed") {
    l <- list(conditional = lme4::fixef(x$full_model))
  } else {
    l <- .compact_list(list(
      conditional = lme4::fixef(x$full_model),
      random = lme4::ranef(x$full_model)
    ))
  }

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}



#' @export
get_parameters.MixMod <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  has_zeroinf <- !is.null(find_formula(x)[["zero_inflated"]])

  if (component %in% c("zi", "zero_inflated") && !has_zeroinf) {
    stop("Model has no zero-inflation component.", call. = FALSE)
  }


  re.names <- dimnames(lme4::ranef(x))[[2]]
  re <- lme4::ranef(x)


  if (has_zeroinf) {
    z_inflated <- lme4::fixef(x, sub_model = "zero_part")
    z_inflated_random <- re[grepl("^zi_", re.names, perl = TRUE)]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
    component <- "conditional"
  }


  l <- .compact_list(list(
    conditional = lme4::fixef(x, sub_model = "main"),
    random = re[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  if (has_zeroinf) {
    fixedzi <- data.frame(
      Parameter = names(l$zero_inflated),
      Estimate = unname(l$zero_inflated),
      Component = "zero_inflated",
      stringsAsFactors = FALSE
    )
  } else {
    fixedzi <- NULL
  }

  if (effects == "fixed") {
    params <- switch(
      component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
    .remove_backticks_from_parameter_names(params)
  } else if (effects == "random") {
    switch(
      component,
      all = .compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = list(random = l$random),
      zi = ,
      zero_inflated = list(zero_inflated_random = l$zero_inflated_random)
    )
  }
}



#' @rdname get_parameters
#' @export
get_parameters.glmmTMB <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  if (effects == "fixed") {
    l <- .compact_list(list(
      conditional = lme4::fixef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      dispersion = lme4::fixef(x)$disp
    ))
  } else {
    l <- .compact_list(list(
      conditional = lme4::fixef(x)$cond,
      random = lme4::ranef(x)$cond,
      zero_inflated = lme4::fixef(x)$zi,
      zero_inflated_random = lme4::ranef(x)$zi,
      dispersion = lme4::fixef(x)$disp
    ))
  }

  # ---- fixed effects (conditional model)

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = unname(l$conditional),
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  # ---- fixed effects (zero_inflated model)

  if (.obj_has_name(l, "zero_inflated")) {
    fixedzi <- data.frame(
      Parameter = names(l$zero_inflated),
      Estimate = unname(l$zero_inflated),
      Component = "zero_inflated",
      stringsAsFactors = FALSE
    )
  } else {
    fixedzi <- NULL
  }

  # ---- fixed effects (dispersion model)

  if (.obj_has_name(l, "dispersion")) {
    fixeddisp <- data.frame(
      Parameter = names(l$dispersion),
      Estimate = unname(l$dispersion),
      Component = "dispersion",
      stringsAsFactors = FALSE
    )
  } else {
    fixeddisp <- NULL
  }

  # ---- build result

  if (effects == "fixed") {
    out <- switch(
      component,
      all = rbind(fixed, fixedzi, fixeddisp),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi,
      dispersion = fixeddisp
    )
    .remove_backticks_from_parameter_names(out)
  } else if (effects == "random") {
    switch(
      component,
      all = .compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = l$random,
      zi = ,
      zero_inflated = l$zero_inflated_random
    )
  }
}



#' @export
get_parameters.mixor <- function(x, effects = c("all", "fixed", "random"), ...) {
  coefs <- stats::coef(x)
  effects <- match.arg(effects)

  params <- find_parameters(x, effects = "fixed", flatten = TRUE)
  fixed <- data.frame(
    Parameter = params,
    Estimate = unname(coefs[params]),
    Effects = "fixed",
    stringsAsFactors = FALSE
  )

  if (effects != "fixed") {
    params <- find_parameters(x, effects = "random", flatten = TRUE)
    random <- data.frame(
      Parameter = params,
      Estimate = unname(coefs[params]),
      Effects = "random",
      stringsAsFactors = FALSE
    )
  } else {
    random <- NULL
  }

  switch(
    effects,
    "all" = rbind(fixed, random),
    "fixed" = fixed,
    "random" = random
  )
}



#' @rdname get_parameters
#' @export
get_parameters.BBmm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  l <- .compact_list(list(
    conditional = x$fixed.coef,
    random = x$random.coef
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects == "fixed") {
    .remove_backticks_from_parameter_names(fixed)
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.glimML <- function(x, effects = c("fixed", "random", "all"), ...) {
  effects <- match.arg(effects)

  l <- .compact_list(list(
    conditional = x@fixed.param,
    random = x@random.param
  ))

  fixed <- data.frame(
    Parameter = names(l$conditional),
    Estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  fixed <- .remove_backticks_from_parameter_names(fixed)

  random <- data.frame(
    Parameter = names(l$random),
    Estimate = l$random,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  random <- .remove_backticks_from_parameter_names(random)

  all <- rbind(
    cbind(fixed, data.frame(Effects = "fixed", stringsAsFactors = FALSE)),
    cbind(random, data.frame(Effects = "random", stringsAsFactors = FALSE))
  )

  if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all
  }
}








# GAM models ---------------------------------------------


#' @export
get_parameters.gamm <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  get_parameters.gam(x, component, ...)
}



#' @export
get_parameters.Gam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  .return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = pars[.grep_smoothers(names(pars))],
    component = component
  )
}



#' @rdname get_parameters
#' @export
get_parameters.gam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  st <- summary(x)$s.table
  smooth_terms <- st[, 1]
  names(smooth_terms) <- row.names(st)

  .return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = smooth_terms,
    component = component
  )
}


#' @export
get_parameters.scam <- get_parameters.gam



#' @export
get_parameters.vgam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  .return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = pars[.grep_smoothers(names(pars))],
    component = component
  )
}



#' @importFrom stats na.omit
#' @export
get_parameters.gamlss <- function(x, ...) {
  pars <- lapply(x$parameters, function(i) {
    stats::na.omit(stats::coef(x, what = i))
  })

  names(pars) <- x$parameters
  if ("mu" %in% names(pars)) names(pars)[1] <- "conditional"

  do.call(rbind, lapply(names(pars), function(i) {
    params <- data.frame(
      Parameter = names(pars[[i]]),
      Estimate = pars[[i]],
      Component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    .remove_backticks_from_parameter_names(params)
  }))

  # data.frame(
  #   Parameter = c(names(pars$conditional), names(pars$sigma), names(pars$nu), names(pars$tau)),
  #   Estimate = c(unname(pars$conditional), unname(pars$sigma), unname(pars$nu), unname(pars$tau)),
  #   Component = c(
  #     rep("conditional", length(pars$conditional)),
  #     rep("sigma", length(pars$sigma)),
  #     rep("nu", length(pars$nu)),
  #     rep("tau", length(pars$tau))
  #   ),
  #   stringsAsFactors = FALSE,
  #   row.names = NULL
  # )
}







# Zero-Inflated models -------------------------------------


#' @rdname get_parameters
#' @export
get_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .return_zeroinf_parms(x, component)
}

#' @export
get_parameters.hurdle <- get_parameters.zeroinfl

#' @export
get_parameters.zerotrunc <- get_parameters.default

#' @rdname get_parameters
#' @export
get_parameters.zcpglm <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  cond <- data.frame(
    Parameter = names(cf$tweedie),
    Estimate = unname(cf$tweedie),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zi <- data.frame(
    Parameter = names(cf$zero),
    Estimate = unname(cf$zero),
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  .remove_backticks_from_parameter_names(pars)
}







# Standard models --------------------------------------------------


#' @export
get_parameters.aov <- function(x, ...) {
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}



#' @export
get_parameters.aovlist <- function(x, ...) {
  cs <- stats::coef(x)
  out <- do.call(rbind, lapply(names(cs), function(i) {
    params <- data.frame(
      Parameter = names(cs[[i]]),
      Estimate = unname(cs[[i]]),
      Group = i,
      stringsAsFactors = FALSE
    )
    .remove_backticks_from_parameter_names(params)
  }))
  rownames(out) <- NULL
  out
}



#' @importFrom stats na.omit coef
#' @export
get_parameters.manova <- function(x, ...) {
  params <- stats::na.omit(stats::coef(x))
  out <- .gather(as.data.frame(params), names_to = "Response", values_to = "Estimate")
  out$Parameter <- rownames(out)

  out <- out[c("Parameter", "Estimate", "Response")]
  rownames(out) <- NULL

  pattern <- paste0("(", paste0(paste0(".", unique(out$Response)), collapse = "|"), ")$")
  out$Parameter <- gsub(pattern, "", out$Parameter)

  .remove_backticks_from_parameter_names(out)
}

#' @export
get_parameters.maov <- get_parameters.manova



#' @export
get_parameters.afex_aov <- function(x, ...) {
  if ("aov" %in% names(x)) {
    get_parameters(x$aov, ...)
  } else {
    get_parameters(x$lm, ...)
  }
}












# Bayesian models -------------------------------------


#' @rdname get_parameters
#' @export
get_parameters.BGGM <- function(x, component = c("correlation", "conditional", "intercept", "all"), summary = FALSE, centrality = "mean", ...) {
  if (!requireNamespace("BGGM", quietly = TRUE)) {
    stop("Package 'BGGM' required for this function to work. Please install it.")
  }

  out <- as.data.frame(BGGM::posterior_samples(x))
  intercepts <- grepl("_\\(Intercept\\)$", colnames(out))
  correlations <- grepl("(.*)--(.*)", colnames(out))
  conditional <- !intercepts & !correlations

  component <- match.arg(component)
  out <- switch(
    component,
    "conditional" = out[, conditional, drop = FALSE],
    "correlation" = out[, correlations, drop = FALSE],
    "intercept" = out[, intercepts, drop = FALSE],
    out
  )
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters
#' @export
get_parameters.MCMCglmm <- function(x, effects = c("fixed", "random", "all"), summary = FALSE, centrality = "mean", ...) {
  effects <- match.arg(effects)

  nF <- x$Fixed$nfl
  fixed <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  random <- as.data.frame(x$VCV[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE])
  all <- cbind(fixed, random)

  out <- if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters
#' @export
get_parameters.BFBayesFactor <- function(x, effects = c("all", "fixed", "random"), component = c("all", "extra"), iterations = 4000, progress = FALSE, verbose = TRUE, summary = FALSE, centrality = "mean", ...) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function requires package `BayesFactor` to work. Please install it.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)
  bf_type <- .classify_BFBayesFactor(x)

  # check if valid model was indexed...

  if (length(x@numerator) > 1 ||
    !xor(
      x@denominator@shortName == "Intercept only",
      grepl("^(Null|Indep)", x@denominator@shortName)
    )) {
    if (verbose) {
      message(
        "Multiple `BFBayesFactor` models detected - posteriors are extracted from the first numerator model.\n",
        'See help("get_parameters", package = "insight").'
      )
    }
  }


  params <- find_parameters(x, effects = effects, component = component, flatten = TRUE, ...)

  if (bf_type %in% c("correlation", "ttest1", "ttest2", "meta", "linear")) {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1, ...)
      ))

    out <- switch(
      bf_type,
      "correlation" = data.frame("rho" = as.numeric(posteriors$rho)),
      "ttest1" = data.frame("Difference" = x@numerator[[1]]@prior$mu - as.numeric(posteriors[, 1])),
      "ttest2" = data.frame("Difference" = x@numerator[[1]]@prior$mu - as.numeric(posteriors[, 2])),
      "meta" = data.frame("Effect" = as.numeric(posteriors$delta)),
      "linear" = .get_bf_posteriors(posteriors, params),
      NULL
    )
  } else if (bf_type == "proptest") {
    posteriors <- as.data.frame(as.matrix(suppressMessages(
      BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1)
    )[, "p"]))
    colnames(posteriors) <- "p"
    out <- posteriors
  } else {
    out <- NULL
  }

  if (isTRUE(summary) && !is.null(out)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }

  out
}



#' @rdname get_parameters
#' @export
get_parameters.stanmvreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  effects <- match.arg(effects)
  elements <- .get_elements(effects, "all")
  parms <- find_parameters(x, flatten = FALSE, parameters = parameters)

  for (i in names(parms)) {
    parms[[i]]$conditional <- sprintf("%s|%s", i, parms[[i]]$conditional)
    find_bracket <- regexpr(pattern = "\\[", parms[[i]]$random)
    parms[[i]]$random <- paste0(
      substr(parms[[i]]$random, start = 1, stop = find_bracket),
      i, "|",
      substr(parms[[i]]$random, start = find_bracket + 1, stop = 1000000L)
    )
    parms[[i]]$sigma <- NULL
  }

  out <- as.data.frame(x)[unlist(lapply(.compact_list(parms), function(i) i[elements]))]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters
#' @export
get_parameters.brmsfit <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "simplex", "sigma", "smooth_terms"), parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (is_multivariate(x)) {
    parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
    elements <- .get_elements(effects, component)
    ## TODO remove "optional = FALSE" in a future update
    out <- as.data.frame(x, optional = FALSE)[unlist(lapply(parms, function(i) i[elements]))]
  } else {
    ## TODO remove "optional = FALSE" in a future update
    out <- as.data.frame(x, optional = FALSE)[.get_parms_data(x, effects, component, parameters)]
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @rdname get_parameters
#' @export
get_parameters.stanreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  effects <- match.arg(effects)
  out <- as.data.frame(x)[.get_parms_data(x, effects, "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}

#' @export
get_parameters.stanfit <- get_parameters.stanreg


#' @export
get_parameters.bcplm <- function(x, parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  out <- as.data.frame(do.call(rbind, x$sims.list))
  if (!is.null(parameters)) {
    out <- out[grepl(pattern = parameters, x = colnames(out), perl = TRUE)]
  }
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.bayesx <- function(x, component = c("conditional", "smooth_terms", "all"), summary = FALSE, centrality = "mean", ...) {
  component <- match.arg(component)

  smooth_dat <- data.frame(
    Parameter = find_parameters(x, component = "smooth_terms", flatten = TRUE),
    Estimate = x$smooth.hyp[, 1],
    Component = "smooth_terms",
    stringsAsFactors = FALSE
  )

  fixed_dat <- data.frame(
    Parameter = find_parameters(x, component = "conditional", flatten = TRUE),
    Estimate = x$fixed.effects[, 1],
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  params <- switch(
    component,
    "all" = rbind(fixed_dat, smooth_dat),
    "conditional" = fixed_dat,
    "smooth_terms" = smooth_dat
  )

  out <- .remove_backticks_from_parameter_names(params)

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @export
get_parameters.mcmc.list <- function(x, parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  out <- as.data.frame(do.call(rbind, x))
  if (!is.null(parameters)) {
    out <- out[grepl(pattern = parameters, x = colnames(out), perl = TRUE)]
  }
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}









# simulations ---------------------------------


#' @rdname get_parameters
#' @export
get_parameters.sim.merMod <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  effects <- match.arg(effects)
  fe <- re <- NULL
  if (effects %in% c("fixed", "all")) fe <- .get_armsim_fixef_parms(x)
  if (effects %in% c("random", "all")) re <- .get_armsim_ranef_parms(x)

  dat <- do.call(cbind, .compact_list(list(fe, re)))

  out <- as.data.frame(dat)[.get_parms_data(x, effects, "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @export
get_parameters.sim <- function(x, parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  dat <- .get_armsim_fixef_parms(x)
  out <- as.data.frame(dat)[.get_parms_data(x, "all", "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.mcmc <- function(x, parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  out <- as.data.frame(x)[.get_parms_data(x, "all", "all", parameters)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.bayesQR <- function(x, parameters = NULL, summary = FALSE, centrality = "mean", ...) {
  out <- as.data.frame(x[[1]]$betadraw)
  names(out) <- x[[1]]$names
  out <- out[.get_parms_data(x, "all", "all", parameters)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}








# utility functions ---------------------------------


.get_parms_data <- function(x, effects, component, parameters = NULL) {
  elements <- .get_elements(effects, component)
  unlist(find_parameters(x, flatten = FALSE, parameters = parameters)[elements])
}



.return_zeroinf_parms <- function(x, component) {
  cf <- stats::coef(x)

  conditional <- grepl("^count_", names(cf), perl = TRUE)
  zero_inflated <- grepl("^zero_", names(cf), perl = TRUE)

  cond <- data.frame(
    Parameter = names(cf)[conditional],
    Estimate = unname(cf)[conditional],
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zi <- data.frame(
    Parameter = names(cf)[zero_inflated],
    Estimate = unname(cf)[zero_inflated],
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  .remove_backticks_from_parameter_names(pars)
}



.return_smooth_parms <- function(conditional, smooth_terms, component) {
  cond <- data.frame(
    Parameter = names(conditional),
    Estimate = conditional,
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (!is.null(smooth_terms)) {
    smooth <- data.frame(
      Parameter = names(smooth_terms),
      Estimate = smooth_terms,
      Component = "smooth_terms",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    smooth <- NULL
  }

  pars <- switch(
    component,
    all = rbind(cond, smooth),
    conditional = cond,
    smooth_terms = smooth
  )

  if (component != "all") {
    pars <- .remove_column(pars, "Component")
  }

  .remove_backticks_from_parameter_names(pars)
}



#' @importFrom methods slot slotNames
.get_armsim_fixef_parms <- function(x) {
  sn <- methods::slotNames(x)
  as.data.frame(methods::slot(x, sn[1]))
}



#' @importFrom methods .hasSlot
.get_armsim_ranef_parms <- function(x) {
  dat <- NULL
  if (methods::.hasSlot(x, "ranef")) {
    re <- x@ranef
    dat <- data.frame()

    for (i in 1:length(re)) {
      dn <- dimnames(re[[i]])[[2]]
      cn <- dimnames(re[[i]])[[3]]
      l <- lapply(1:length(dn), function(j) {
        d <- as.data.frame(re[[i]][, j, ])
        colnames(d) <- sprintf("%s.%s", cn, dn[j])
        d
      })
      if (ncol(dat) == 0) {
        dat <- do.call(cbind, l)
      } else {
        dat <- cbind(dat, do.call(cbind, l))
      }
    }
  }

  dat
}



.get_bf_posteriors <- function(posteriors, params) {
  cn <- intersect(colnames(posteriors), params)
  posteriors[, cn, drop = FALSE]
}



#' @importFrom stats median
.summary_of_posteriors <- function(out, centrality = "mean", ...) {
  s <- switch(
    centrality,
    "mean" = sapply(out, mean),
    "median" = sapply(out, stats::median),
    sapply(out, mean)
  )
  data.frame(
    Parameter = names(s),
    Estimate = unname(s),
    stringsAsFactors = FALSE
  )
}


.clean_emmeans_draws <- function(x, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop(
      "Package 'emmeans' required for this function to work.\n",
      "Please install it by running `install.packages('emmeans')`."
    )
  }

  if (!is.null(attributes(x)$misc$predict.type) && attributes(x)$misc$predict.type != "none") {
    x <- emmeans::regrid(x, transform = attributes(x)$misc$predict.type, ...)
  }

  draws <- emmeans::as.mcmc.emmGrid(
    x,
    names = FALSE,
    sep.chains = FALSE,
    NE.include = TRUE,
    ...
  )
  data.frame(draws, check.names = FALSE)
}
