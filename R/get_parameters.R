#' @title Get model parameters
#' @name get_parameters
#'
#' @description Returns the coefficients (or posterior samples for Bayesian
#' models) from a model. See the documentation for your object's class:
#'
#' - [Bayesian models][get_parameters.BGGM] (**rstanarm**, **brms**, **MCMCglmm**, ...)
#' - [Estimated marginal means][get_parameters.emmGrid] (**emmeans**)
#' - [Generalized additive models][get_parameters.gamm] (**mgcv**, **VGAM**, ...)
#' - [Marginal effects models][get_parameters.betamfx] (**mfx**)
#' - [Mixed models][get_parameters.glmmTMB] (**lme4**, **glmmTMB**, **GLMMadaptive**, ...)
#' - [Zero-inflated and hurdle models][get_parameters.zeroinfl] (**pscl**, ...)
#' - [Models with special components][get_parameters.betareg] (**betareg**, **MuMIn**, ...)
#' - [Hypothesis tests][get_parameters.htest] (`htest`)
#'
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @inheritSection find_predictors Model components
#'
#' @return
#' - for non-Bayesian models, a data frame with two columns: the parameter names
#'   and the related point estimates.
#' - for Anova (`aov()`) with error term, a list of parameters for the
#'   conditional and the random effects parameters
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' `effects` and `component` can be used.
#'
#' `get_parameters()` is comparable to `coef()`, however, the coefficients
#' are returned as data frame (with columns for names and point estimates of
#' coefficients). For Bayesian models, the posterior samples of parameters are
#' returned.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters <- function(x, ...) {
  UseMethod("get_parameters")
}


# Default models ---------------------------------------------


#' @rdname get_parameters
#' @export
get_parameters.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, "list") && object_has_names(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    return(get_parameters.gam(x, ...))
  }

  tryCatch(
    {
      cf <- stats::coef(x)
      params <- names(cf)
      if (is.null(params)) {
        params <- paste(seq_along(cf))
      }

      params <- data.frame(
        Parameter = params,
        Estimate = unname(cf),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      text_remove_backticks(params)
    },
    error = function(x) {
      if (isTRUE(verbose)) {
        format_warning(
          sprintf("Parameters can't be retrieved for objects of class `%s`.", class(x)[1])
        )
      }
      NULL
    }
  )
}


#' @export
get_parameters.summary.lm <- function(x, ...) {
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = names(cf[, 1]),
    Estimate = unname(cf[, 1]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.data.frame <- function(x, ...) {
  format_error("A data frame is no valid object for this function.")
}


# Special models ---------------------------------------------


#' @export
get_parameters.rms <- get_parameters.default


#' @export
get_parameters.tobit <- get_parameters.default


#' @export
get_parameters.model_fit <- function(x, ...) {
  get_parameters(x$fit, ...)
}


#' @export
get_parameters.ordinal_weightit <- function(x, ...) {
  out <- get_parameters.default(x, ...)
  out$Component <- "conditional"
  out
}


#' @export
get_parameters.bfsl <- function(x, ...) {
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = rownames(cf),
    Estimate = unname(cf[, "Estimate"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.selection <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "selection", "outcome", "auxiliary"))
  s <- summary(x)
  rn <- row.names(s$estimate)
  estimates <- as.data.frame(s$estimate, row.names = FALSE)
  params <- data.frame(
    Parameter = rn,
    Estimate = estimates[[1]],
    Component = "selection",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  params$Component[s$param$index$errTerms] <- "auxiliary"
  params$Component[s$param$index$outcome] <- "outcome"

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  text_remove_backticks(params)
}


#' @export
get_parameters.epi.2by2 <- function(x, ...) {
  coef_names <- grepl("^([^NNT]*)(\\.strata\\.wald)", names(x$massoc.detail), perl = TRUE)
  cf <- x$massoc.detail[coef_names]
  names(cf) <- gsub(".strata.wald", "", names(cf), fixed = TRUE)

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unlist(lapply(cf, function(i) i["est"]), use.names = FALSE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  text_remove_backticks(params)
}


#' @export
get_parameters.Rchoice <- function(x, ...) {
  cf <- stats::coef(x)
  params <- data.frame(
    Parameter = find_parameters(x, flatten = TRUE),
    Estimate = as.vector(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  text_remove_backticks(params)
}


#' @export
get_parameters.btergm <- function(x, ...) {
  cf <- x@coef
  params <- data.frame(
    Parameter = names(cf),
    Estimate = as.vector(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  text_remove_backticks(params)
}


#' @export
get_parameters.sdmTMB <- function(x, component = "all", verbose = TRUE, ...) {
  delta_comp <- isTRUE(x$family$delta)
  valid_comp <- compact_character(c("all", "conditional", ifelse(delta_comp, "delta", "")))
  component <- validate_argument(component, valid_comp)

  cf <- suppressMessages(stats::coef(x, model = 1))
  conditional <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (delta_comp) {
    cf <- suppressMessages(stats::coef(x, model = 2))
    delta <- data.frame(
      Parameter = names(cf),
      Estimate = unname(cf),
      Component = "delta",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  if (delta_comp) {
    params <- rbind(conditional, delta)
  } else {
    params <- conditional
  }

  params <- switch(component,
    all = params,
    conditional = conditional,
    delta = delta
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.mediate <- function(x, ...) {
  info <- model_info(x$model.y, verbose = FALSE)
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
  text_remove_backticks(out)
}


#' @export
get_parameters.ridgelm <- function(x, ...) {
  out <- data.frame(
    Parameter = names(x$coef),
    Estimate = as.vector(x$coef),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.ivFixed <- function(x, ...) {
  out <- data.frame(
    Parameter = rownames(x$coefficients),
    Estimate = as.vector(x$coefficients),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.ivprobit <- function(x, ...) {
  out <- data.frame(
    Parameter = x$names,
    Estimate = as.vector(x$coefficients),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.survreg <- function(x, ...) {
  s <- summary(x)
  out <- data.frame(
    Parameter = rownames(s$table),
    Estimate = as.vector(s$table[, 1]),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.riskRegression <- function(x, ...) {
  junk <- utils::capture.output(cs <- stats::coef(x)) # nolint
  out <- data.frame(
    Parameter = as.vector(cs[, 1]),
    Estimate = as.numeric(cs[, 2]),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.mipo <- function(x, ...) {
  s <- summary(x)
  out <- data.frame(
    Parameter = as.vector(s$term),
    Estimate = as.vector(s$estimate),
    stringsAsFactors = FALSE
  )
  # check for ordinal-alike models
  if (!is.null(x$pooled) && "y.level" %in% colnames(x$pooled)) {
    out$Response <- as.vector(x$pooled$y.level)
  }
  text_remove_backticks(out)
}


#' @export
get_parameters.mira <- function(x, ...) {
  check_if_installed("mice")
  get_parameters(mice::pool(x), ...)
}


#' @export
get_parameters.margins <- function(x, ...) {
  s <- summary(x)
  param <- as.vector(s$factor)
  estimate_pos <- which(colnames(s) == "AME")

  if (estimate_pos > 2L) {
    out <- s[1:(estimate_pos - 1)]
    r <- apply(out, 1, function(i) paste0(colnames(out), " [", i, "]"))
    param <- unname(sapply(as.data.frame(r), paste, collapse = ", "))
  }

  out <- data.frame(
    Parameter = param,
    Estimate = as.vector(summary(x)$AME),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.glht <- function(x, ...) {
  s <- summary(x)
  alt <- switch(x$alternative,
    two.sided = "==",
    less = ">=",
    greater = "<="
  )
  out <- data.frame(
    Parameter = paste(names(s$test$coefficients), alt, x$rhs),
    Estimate = unname(s$test$coefficients),
    stringsAsFactors = FALSE
  )
  text_remove_backticks(out)
}


#' @export
get_parameters.mle2 <- function(x, ...) {
  check_if_installed("bbmle")
  s <- bbmle::summary(x)

  params <- data.frame(
    Parameter = names(s@coef[, 1]),
    Estimate = unname(s@coef[, 1]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}

#' @export
get_parameters.mle <- get_parameters.mle2


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

      text_remove_backticks(params)
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
    for (i in seq_len(nrow(params))) {
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

  text_remove_backticks(out)
}


#' @export
get_parameters.multinom_weightit <- function(x, ...) {
  params <- stats::coef(x)

  out <- data.frame(
    Parameter = gsub("(.*)~(.*)", "\\2", names(params)),
    Estimate = unname(params),
    Response = gsub("(.*)~(.*)", "\\1", names(params)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(out)
}


#' @export
get_parameters.brmultinom <- get_parameters.multinom


#' @export
get_parameters.mblogit <- function(x, ...) {
  params <- stats::coef(x)

  out <- data.frame(
    Parameter = gsub("(.*)~(.*)", "\\2", names(params)),
    Estimate = unname(params),
    Response = gsub("(.*)~(.*)", "\\1", names(params)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(out)
}


#' @export
get_parameters.mclogit <- function(x, ...) {
  params <- stats::coef(x)

  out <- data.frame(
    Parameter = names(params),
    Estimate = unname(params),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(out)
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

    text_remove_backticks(params)
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

  text_remove_backticks(params)
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

  text_remove_backticks(params)
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

      params$Parameter[grepl("intrcpt", params$Parameter, fixed = TRUE)] <- "(Intercept)"
      text_remove_backticks(params)
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

      params$Parameter[grepl("d", params$Parameter, fixed = TRUE)] <- "(Intercept)"
      text_remove_backticks(params)
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

  params$Parameter[grepl("muhat", params$Parameter, fixed = TRUE)] <- "(Intercept)"
  text_remove_backticks(params)
}


# SEM models ---------------------------------------------


#' @export
get_parameters.blavaan <- function(x, summary = FALSE, standardize = FALSE, ...) {
  if (isTRUE(summary)) {
    return(get_parameters.lavaan(x, standardize = standardize, ...))
  }

  check_if_installed("lavaan")
  check_if_installed("blavaan")

  if (isTRUE(standardize)) {
    draws <- blavaan::standardizedPosterior(x)
  } else {
    draws <- blavaan::blavInspect(x, "draws")
  }
  posteriors <- as.data.frame(as.matrix(draws))

  param_tab <- lavaan::parameterEstimates(x)
  params <- paste0(param_tab$lhs, param_tab$op, param_tab$rhs)

  coef_labels <- names(lavaan::coef(x))

  if ("group" %in% colnames(param_tab) && n_unique(param_tab$group) > 1L) {
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

  colnames(posteriors) <- coef_labels

  posteriors
}


#' @export
get_parameters.lavaan <- function(x, standardize = FALSE, ...) {
  check_if_installed("lavaan")

  if (standardize) {
    params <- lavaan::standardizedSolution(x)
  } else {
    params <- lavaan::parameterEstimates(x)
  }

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

  text_remove_backticks(params)
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

  text_remove_backticks(params)
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

  text_remove_backticks(params)
}


# Anova and Standard models --------------------------------------------------


#' @export
get_parameters.aov <- function(x, ...) {
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
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
    text_remove_backticks(params)
  }))
  rownames(out) <- NULL
  out
}


#' @export
get_parameters.manova <- function(x, ...) {
  params <- stats::na.omit(stats::coef(x))
  out <- .gather(as.data.frame(params), names_to = "Response", values_to = "Estimate")
  out$Parameter <- rownames(out)

  out <- out[c("Parameter", "Estimate", "Response")]
  rownames(out) <- NULL

  pattern <- paste0("(", paste(paste0(".", unique(out$Response)), collapse = "|"), ")$")
  out$Parameter <- gsub(pattern, "", out$Parameter)

  text_remove_backticks(out)
}

#' @export
get_parameters.maov <- get_parameters.manova


#' @export
get_parameters.afex_aov <- function(x, ...) {
  if (is.null(x$aov)) {
    get_parameters(x$lm, ...)
  } else {
    get_parameters(x$aov, ...)
  }
}


#' @export
get_parameters.pgmm <- function(x, component = "conditional", ...) {
  component <- validate_argument(component, c("conditional", "all"))
  cs <- stats::coef(summary(x, time.dummies = TRUE, robust = FALSE))
  params <- data.frame(
    Parameter = rownames(cs),
    Estimate = unname(cs[, 1]),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params$Component[params$Parameter %in% x$args$namest] <- "time_dummies"

  if (component == "conditional") {
    params <- params[params$Component == "conditional", ]
    params <- .remove_column(params, "Component")
  }

  text_remove_backticks(params)
}


# utility functions ---------------------------------


.get_armsim_fixef_parms <- function(x) {
  sn <- methods::slotNames(x)
  as.data.frame(methods::slot(x, sn[1]))
}


.get_armsim_ranef_parms <- function(x) {
  dat <- NULL
  if (methods::.hasSlot(x, "ranef")) {
    re <- x@ranef
    dat <- data.frame()

    for (i in seq_along(re)) {
      dn <- dimnames(re[[i]])[[2]]
      cn <- dimnames(re[[i]])[[3]]
      l <- lapply(seq_along(dn), function(j) {
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


#' @export
get_parameters.lm_robust <- function(x, ...) {
  if (is_multivariate(x)) {
    get_parameters.mlm(x, ...)
  } else {
    get_parameters.default(x, ...)
  }
}
