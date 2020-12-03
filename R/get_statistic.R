#' @title Get statistic associated with estimates
#' @description Returns the statistic (\emph{t}, \code{z}, ...) for model estimates.
#'   In most cases, this is the related column from \code{coef(summary())}.
#' @name get_statistic
#'
#' @param x A model.
#' @param column_index For model objects that have no defined \code{get_statistic()}
#'   method yet, the default method is called. This method tries to extract the
#'   statistic column from \code{coef(summary())}, where the index of the column
#'   that is being pulled is \code{column_index}. Defaults to 3, which is the
#'   default statistic column for most models' summary-output.
#' @param component Should all parameters, parameters for the conditional model,
#'   or for the zero-inflated part of the model be returned? Applies to models
#'   with zero-inflated component. \code{component} may be one of
#'   \code{"conditional"}, \code{"zi"}, \code{"zero-inflated"} or \code{"all"}
#'   (default). For models with smooth terms, \code{component = "smooth_terms"}
#'   is also possible. May be abbreviated. Note that the \emph{conditional}
#'   component is also called \emph{count} or \emph{mean} component, depending
#'   on the model.
#' @param robust Logical, if \code{TRUE}, test statistic based on robust standard
#'   errors is returned.
#' @param adjust Character value naming the method used to adjust p-values or confidence intervals. See \code{?emmeans::summary.emmGrid} for details.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%). Currently only applies to objects of class \code{emmGrid}.
#' @param ... Currently not used.
#' @inheritParams get_parameters
#'
#' @return A data frame with the model's parameter names and the related test statistic.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_statistic(m)
#' @export
get_statistic <- function(x, ...) {
  UseMethod("get_statistic")
}



# Default models ----------------------------------------------------------


#' @rdname get_statistic
#' @export
get_statistic.default <- function(x, column_index = 3, ...) {
  cs <- stats::coef(summary(x))

  out <- data.frame(
    Parameter = rownames(cs),
    Statistic = as.vector(cs[, column_index]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.mlm <- function(x, ...) {
  cs <- stats::coef(summary(x))

  out <- lapply(names(cs), function(i) {
    params <- cs[[i]]
    data.frame(
      Parameter = rownames(params),
      Statistic = as.vector(params[, 3]),
      Response = gsub("^Response (.*)", "\\1", i),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  out <- .remove_backticks_from_parameter_names(do.call(rbind, out))
  attr(out, "statistic") <- find_statistic(x)

  out
}


#' @export
get_statistic.lme <- function(x, ...) {
  get_statistic.default(x, column_index = 4)
}

#' @export
get_statistic.lmerModLmerTest <- get_statistic.lme


#' @export
get_statistic.merModList <- function(x, ...) {
  s <- suppressWarnings(summary(x))
  out <- data.frame(
    Parameter = s$fe$term,
    Statistic = s$fe$statistic,
    stringsAsFactors = FALSE
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.plm <- get_statistic.default


#' @export
get_statistic.maxLik <- get_statistic.default


#' @export
get_statistic.glmmadmb <- get_statistic.default


#' @export
get_statistic.lm_robust <- get_statistic.default


#' @export
get_statistic.geeglm <- get_statistic.default


#' @export
get_statistic.truncreg <- get_statistic.default


#' @export
get_statistic.tobit <- get_statistic.default


#' @export
get_statistic.censReg <- get_statistic.default


#' @export
get_statistic.negbin <- get_statistic.default


#' @export
get_statistic.feis <- get_statistic.default







# Models with zero-inflation component --------------------------------------


#' @importFrom stats coef
#' @rdname get_statistic
#' @export
get_statistic.glmmTMB <- function(x, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(x)))
  out <- lapply(names(cs), function(i) {
    data.frame(
      Parameter = find_parameters(x, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, out)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")
  stat$Component <- .rename_values(stat$Component, "disp", "dispersion")

  stat <- .filter_component(stat, component)
  stat <- .remove_backticks_from_parameter_names(stat)
  attr(stat, "statistic") <- find_statistic(x)

  stat
}


#' @export
get_statistic.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(x)))
  out <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    data.frame(
      Parameter = find_parameters(x, effects = "fixed", component = comp, flatten = TRUE),
      Statistic = as.vector(stats[, 3]),
      Component = comp,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, out)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  stat <- .remove_backticks_from_parameter_names(stat)
  attr(stat, "statistic") <- find_statistic(x)

  stat
}

#' @export
get_statistic.hurdle <- get_statistic.zeroinfl

#' @export
get_statistic.zerocount <- get_statistic.zeroinfl


#' @export
get_statistic.MixMod <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  s <- summary(x)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)

  out <- lapply(names(cs), function(i) {
    data.frame(
      Parameter = find_parameters(x, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- .filter_component(do.call(rbind, out), component)
  stat <- .remove_backticks_from_parameter_names(stat)
  attr(stat, "statistic") <- find_statistic(x)

  stat
}






# gam models --------------------------------------------------------------


#' @importFrom stats na.omit
#' @export
get_statistic.Gam <- function(x, ...) {
  p.aov <- stats::na.omit(summary(x)$parametric.anova)

  out <- data.frame(
    Parameter = rownames(p.aov),
    Statistic = as.vector(p.aov[, 4]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.gam <- function(x, ...) {
  cs <- summary(x)$p.table
  cs.smooth <- summary(x)$s.table

  out <- data.frame(
    Parameter = c(rownames(cs), rownames(cs.smooth)),
    Statistic = c(as.vector(cs[, 3]), as.vector(cs.smooth[, 3])),
    Component = c(rep("conditional", nrow(cs)), rep("smooth_terms", nrow(cs.smooth))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.scam <- get_statistic.gam



#' @export
get_statistic.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c("gam", "lm", "glm")
  get_statistic.gam(x, ...)
}


#' @export
get_statistic.list <- function(x, ...) {
  if ("gam" %in% names(x)) {
    x <- x$gam
    class(x) <- c("gam", "lm", "glm")
    get_statistic.gam(x, ...)
  }
}


#' @importFrom utils capture.output
#' @export
get_statistic.gamlss <- function(x, ...) {
  parms <- get_parameters(x)
  utils::capture.output(cs <- summary(x))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(cs[, 3]),
    Component = parms$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.vglm <- function(x, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' needed for this function to work. Please install it.")
  }

  cs <- VGAM::coef(VGAM::summary(x))

  out <- data.frame(
    Parameter = rownames(cs),
    Statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.vgam <- function(x, ...) {
  params <- get_parameters(x)
  out <- data.frame(
    Parameter = names(x@nl.chisq),
    Statistic = x@nl.chisq,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- merge(params, out, all.x = TRUE)
  out <- out[order(out$Parameter, params$Parameter), ]

  out <- .remove_backticks_from_parameter_names(out[c("Parameter", "Statistic", "Component")])
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.cgam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  sc <- summary(x)
  stat <- as.vector(sc$coefficients[, 3])
  if (!is.null(sc$coefficients2)) stat <- c(stat, rep(NA, nrow(sc$coefficients2)))

  params <- get_parameters(x, component = "all")

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = stat,
    Component = params$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}








# Survival models ------------------------------------------


#' @export
get_statistic.coxph <- function(x, ...) {
  get_statistic.default(x, column_index = 4)
}


#' @importFrom stats vcov
#' @export
get_statistic.coxme <- function(x, ...) {
  beta <- x$coefficients
  out <- NULL

  if (length(beta) > 0) {
    out <- data.frame(
      Parameter = names(beta),
      Statistic = as.vector(beta / sqrt(diag(stats::vcov(x)))),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    out <- .remove_backticks_from_parameter_names(out)
    attr(out, "statistic") <- find_statistic(x)
  }

  out
}



#' @export
get_statistic.survreg <- function(x, ...) {
  parms <- get_parameters(x)
  s <- summary(x)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = s$table[, 3],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.BBmm <- function(x, ...) {
  parms <- get_parameters(x)
  s <- summary(x)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = s$fixed.coefficients[, 3],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.flexsurvreg <- function(x, ...) {
  parms <- get_parameters(x)
  se <- x$res[, "se"]

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.aareg <- function(x, ...) {
  sc <- summary(x)
  parms <- get_parameters(x)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = unname(sc$test.statistic),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}







# Ordinal models --------------------------------------------------


#' @rdname get_statistic
#' @export
get_statistic.clm2 <- function(x, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  stats <- stats::coef(summary(x))
  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  out <- data.frame(
    Parameter = rownames(stats),
    Statistic = unname(stats[, "z value"]),
    Component = c(rep("conditional", times = n_intercepts + n_location), rep("scale", times = n_scale)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.clmm2 <- get_statistic.clm2


#' @export
get_statistic.glmm <- function(x, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  s <- summary(x)

  out <- get_parameters(x, effects = "all")
  out$Statistic <- c(s$coefmat[, 3], s$nucoefmat[, 3])
  out <- out[, c("Parameter", "Statistic", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.mixor <- function(x, effects = c("all", "fixed", "random"), ...) {
  stats <- x$Model[, "z value"]
  effects <- match.arg(effects)

  parms <- get_parameters(x, effects = effects)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = stats[parms$Parameter],
    Effects = parms$Effects,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.multinom <- function(x, ...) {
  parms <- get_parameters(x)
  stderr <- summary(x)$standard.errors

  if (is.matrix(stderr)) {
    se <- c()
    for (i in 1:nrow(stderr)) {
      se <- c(se, as.vector(stderr[i, ]))
    }
  } else {
    se <- as.vector(stderr)
  }

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if ("Response" %in% colnames(parms)) {
    out$Response <- parms$Response
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.brmultinom <- get_statistic.multinom


#' @export
get_statistic.bracl <- function(x, ...) {
  parms <- get_parameters(x)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = stats::coef(summary(x))[, "z value"],
    Response = parms$Response,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.mlogit <- function(x, ...) {
  if (requireNamespace("mlogit", quietly = TRUE)) {
    cs <- stats::coef(summary(x))

    out <- data.frame(
      Parameter = rownames(cs),
      Statistic = as.vector(cs[, 3]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    out <- .remove_backticks_from_parameter_names(out)
    attr(out, "statistic") <- find_statistic(x)
    out
  } else {
    NULL
  }
}







# mfx models -------------------------------------------------------


#' @rdname get_statistic
#' @importFrom stats coef
#' @export
get_statistic.betamfx <- function(x, component = c("all", "conditional", "precision", "marginal"), ...) {
  component <- match.arg(component)
  parms <- get_parameters(x, component = "all", ...)
  cs <- do.call(rbind, stats::coef(summary(x$fit)))
  stat <- c(as.vector(x$mfxest[, 3]), as.vector(cs[, 3]))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = stat,
    Component = parms$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.betaor <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  parms <- get_parameters(x, component = "all", ...)
  cs <- do.call(rbind, stats::coef(summary(x$fit)))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(cs[, 3]),
    Component = parms$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @rdname get_statistic
#' @export
get_statistic.logitmfx <- function(x, component = c("all", "conditional", "marginal"), ...) {
  parms <- get_parameters(x, component = "all", ...)
  cs <- stats::coef(summary(x$fit))
  stat <- c(as.vector(x$mfxest[, 3]), as.vector(cs[, 3]))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = stat,
    Component = parms$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.poissonmfx <- get_statistic.logitmfx

#' @export
get_statistic.negbinmfx <- get_statistic.logitmfx

#' @export
get_statistic.probitmfx <- get_statistic.logitmfx

#' @export
get_statistic.logitor <- function(x, ...) {
  get_statistic.default(x$fit)
}

#' @export
get_statistic.poissonirr <- get_statistic.logitor

#' @export
get_statistic.negbinirr <- get_statistic.logitor







# Other models -------------------------------------------------------


#' @export
get_statistic.ridgelm <- function(x, ...) {
  NULL
}


#' @export
get_statistic.HLfit <- function(x, ...) {
  utils::capture.output(s <- summary(x))

  out <- data.frame(
    Parameter = rownames(s$beta_table),
    Statistic = as.vector(s$beta_table[, "t-value"]),
    stringsAsFactors = FALSE
  )
  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.margins <- function(x, ...) {
  out <- data.frame(
    Parameter = get_parameters(x)$Parameter,
    Statistic = as.vector(summary(x)$z),
    stringsAsFactors = FALSE
  )
  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.lqmm <- function(x, ...) {
  cs <- summary(x, ...)
  params <- get_parameters(x)

  if (is.list(cs$tTable)) {
    stats <- do.call(rbind, cs$tTable)
    params$Statistic <- params$Estimate / stats[, 2]
    params <- params[c("Parameter", "Statistic", "Component")]
  } else {
    params$Statistic <- params$Estimate / cs$tTable[, 2]
    params <- params[c("Parameter", "Statistic")]
  }

  out <- .remove_backticks_from_parameter_names(params)
  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.lqm <- get_statistic.lqmm


#' @export
get_statistic.mipo <- function(x, ...) {
  params <- data.frame(
    Parameter = as.vector(summary(x)$term),
    Statistic = as.vector(summary(x)$statistic),
    stringsAsFactors = FALSE
  )
  out <- .remove_backticks_from_parameter_names(params)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.mira <- function(x, ...) {
  get_statistic(x$analyses[[1]], ...)
}


#' @export
get_statistic.mle2 <- function(x, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract test statistic.", call. = FALSE)
  }
  s <- bbmle::summary(x)

  params <- data.frame(
    Parameter = names(s@coef[, 3]),
    Statistic = unname(s@coef[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(params)
  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.mle <- get_statistic.mle2


#' @export
get_statistic.glht <- function(x, ...) {
  s <- summary(x)
  alt <- switch(
    x$alternative,
    two.sided = "==",
    less = ">=",
    greater = "<="
  )
  out <- data.frame(
    Parameter = paste(names(s$test$coefficients), alt, x$rhs),
    Statistic = unname(s$test$tstat),
    stringsAsFactors = FALSE
  )
  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @rdname get_statistic
#' @export
get_statistic.emmGrid <- function(x, ci = .95, adjust = "none", merge_parameters = FALSE, ...) {
  s <- summary(x, level = ci, adjust = adjust)

  # check if DF exist
  if (is.null(s$df)) {
    return(NULL)
  }

  estimate_pos <- which(colnames(s) == x@misc$estName)
  ci_level <- .95

  if (length(estimate_pos)) {
    msg <- attributes(s)$mesg
    if (!is.null(msg)) {
      msg <- msg[grepl("^Confidence level", msg)]
      if (length(msg)) {
        ci_level <- tryCatch(
          {
            as.numeric(trimws(gsub("Confidence level used:", "", msg, fixed = TRUE)))
          },
          warning = function(w) {
            .95
          },
          error = function(e) {
            .95
          }
        )
      }
    }

    fac <- stats::qt((1 + ci_level) / 2, df = s$df)

    if ("asymp.LCL" %in% colnames(s)) {
      se <- (s$asymp.UCL - s$asymp.LCL) / (2 * fac)
    } else {
      se <- (s$upper.CL - s$lower.CL) / (2 * fac)
    }
    stat <- s[[x@misc$estName]] / se

    # 2nd try
    if (.is_empty_object(stat)) {
      stat <- s[["t.ratio"]]
    }

    # quit
    if (.is_empty_object(stat)) {
      return(NULL)
    }

    if (isTRUE(merge_parameters)) {
      params <- get_parameters(x, merge_parameters = TRUE)["Parameter"]
    } else {
      params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    }

    out <- data.frame(
      params,
      Statistic = as.vector(stat),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    out <- .remove_backticks_from_parameter_names(out)
    attr(out, "statistic") <- find_statistic(x)
    out
  } else {
    return(NULL)
  }
}


#' @export
get_statistic.emm_list <- function(x, ci = .95, adjust = "none", ...) {
  params <- get_parameters(x)
  s <- summary(x, level = ci, adjust = adjust)
  se <- unlist(lapply(s, function(i) {
    if (is.null(i$SE)) {
      rep(NA, nrow(i))
    } else {
      i$SE
    }
  }))

  stat <- params$Estimate / se

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(stat),
    Component = params$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.robmixglm <- function(x, ...) {
  cs <- stats::coef(summary(x))

  out <- data.frame(
    Parameter = rownames(cs),
    Statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- out[!is.na(out$Statistic), ]
  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.averaging <- function(x, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- get_parameters(x, component = component)
  if (component == "full") {
    s <- summary(x)$coefmat.full
  } else {
    s <- summary(x)$coefmat.subset
  }

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = s[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom stats coef
#' @export
get_statistic.bayesx <- function(x, ...) {
  out <- data.frame(
    Parameter = find_parameters(x, component = "conditional", flatten = TRUE),
    Statistic = x$fixed.effects[, 3],
    stringsAsFactors = FALSE
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.Arima <- function(x, ...) {
  params <- get_parameters(x)
  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(params$Estimate / sqrt(diag(get_varcov(x)))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .remove_backticks_from_parameter_names(out)
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.wbm <- function(x, ...) {
  s <- summary(x)

  statistic_column <- if ("t val." %in% c(
    colnames(s$within_table),
    colnames(s$between_table),
    colnames(s$ints_table)
  )) {
    "t val."
  } else {
    "z val."
  }

  stat <- c(
    s$within_table[, statistic_column],
    s$between_table[, statistic_column],
    s$ints_table[, statistic_column]
  )

  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(stat),
    Component = params$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.wbgee <- get_statistic.wbm



#' @export
get_statistic.cpglmm <- function(x, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  stats <- cplm::summary(x)$coefs
  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(stats[, "t value"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.sem <- function(x, ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  params <- get_parameters(x, effects = "fixed")

  if (is.null(x$se)) {
    warning("Model has no standard errors. Please fit model again with bootstrapped standard errors.", call. = FALSE)
    return(NULL)
  }

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(x$coef / x$se),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @importFrom utils capture.output
#' @export
get_statistic.cpglm <- function(x, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(x)$coefficients)
  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(stats[, "t value"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom utils capture.output
#' @export
get_statistic.zcpglm <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(x)$coefficients)
  params <- get_parameters(x)

  tweedie <- data.frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    Statistic = as.vector(stats$tweedie[, "z value"]),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zero <- data.frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    Statistic = as.vector(stats$zero[, "z value"]),
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out <- .remove_backticks_from_parameter_names(out)

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.manova <- function(x, ...) {
  stats <- as.data.frame(summary(x)$stats)

  out <- data.frame(
    Parameter = rownames(stats),
    Statistic = as.vector(stats[["approx F"]]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.maov <- function(x, ...) {
  s <- summary(x)
  out <- do.call(rbind, lapply(names(s), function(i) {
    stats <- s[[i]]
    missing <- is.na(stats[["F value"]])
    data.frame(
      Parameter = rownames(stats)[!missing],
      Statistic = as.vector(stats[["F value"]][!missing]),
      Response = gsub("\\s*Response ", "", i),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.MANOVA <- function(x, ...) {
  stats <- as.data.frame(x$WTS)

  out <- data.frame(
    Parameter = rownames(stats),
    Statistic = as.vector(stats[[1]]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.RM <- get_statistic.MANOVA



#' @export
get_statistic.rq <- function(x, ...) {
  stat <- tryCatch(
    {
      cs <- stats::coef(summary(x))
      cs[, "t value"]
    },
    error = function(e) {
      cs <- stats::coef(summary(x, covariance = TRUE))
      cs[, "t value"]
    }
  )

  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = stat,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.crq <- function(x, ...) {
  sc <- summary(x)
  params <- get_parameters(x)

  if (all(unlist(lapply(sc, is.list)))) {
    list_sc <- lapply(sc, function(i) {
      .x <- as.data.frame(i)
      .x$Parameter <- rownames(.x)
      .x
    })
    out <- do.call(rbind, list_sc)
    out <- data.frame(
      Parameter = params$Parameter,
      Statistic = out$coefficients.T.Value,
      Component = params$Component,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    out <- data.frame(
      Parameter = params$Parameter,
      Statistic = unname(sc$coefficients[, 5]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.crqs <- get_statistic.crq

#' @export
get_statistic.nlrq <- get_statistic.rq



#' @export
get_statistic.rqss <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  cs <- summary(x)
  stat <- c(as.vector(cs$coef[, "t value"]), as.vector(cs$qsstab[, "F value"]))

  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = unname(stat),
    Component = params$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.bigglm <- function(x, ...) {
  parms <- get_parameters(x)
  cs <- summary(x)$mat
  se <- as.vector(cs[, 4])

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.biglm <- function(x, ...) {
  parms <- get_parameters(x)
  cs <- summary(x)$mat
  se <- as.vector(cs[, 4])

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.LORgee <- function(x, ...) {
  out <- get_statistic.default(x)
  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.crch <- function(x, ...) {
  cs <- do.call(rbind, stats::coef(summary(x), model = "full"))
  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.fixest <- function(x, ...) {
  cs <- summary(x)$coeftable
  params <- get_parameters(x)

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.glmx <- function(x, component = c("all", "conditional", "extra"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(summary(x))
  parms <- get_parameters(x)

  out <- rbind(
    data.frame(
      Parameter = parms$Parameter[parms$Component == "conditional"],
      Statistic = unname(cf$glm[, 3]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = parms$Parameter[parms$Component == "extra"],
      Statistic = cf$extra[, 3],
      Component = "extra",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @rdname get_statistic
#' @importFrom stats coef
#' @export
get_statistic.gee <- function(x, robust = FALSE, ...) {
  parms <- get_parameters(x)
  cs <- stats::coef(summary(x))

  if (isTRUE(robust)) {
    stats <- as.vector(cs[, "Robust z"])
  } else {
    stats <- as.vector(cs[, "Naive z"])
  }

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = stats,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.complmrob <- function(x, ...) {
  parms <- get_parameters(x)
  stat <- summary(x)$stats

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(stat[, "t value"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom stats qchisq
#' @importFrom utils capture.output
#' @export
get_statistic.logistf <- function(x, ...) {
  parms <- get_parameters(x)
  utils::capture.output(s <- summary(x))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(stats::qchisq(1 - s$prob, df = 1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom stats vcov
#' @export
get_statistic.svyglm.nb <- function(x, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }

  parms <- get_parameters(x)
  se <- sqrt(diag(stats::vcov(x, stderr = "robust")))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.svyglm.zip <- get_statistic.svyglm.nb



#' @rdname get_statistic
#' @importFrom stats coef
#' @export
get_statistic.betareg <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  parms <- get_parameters(x)
  cs <- do.call(rbind, stats::coef(summary(x)))
  se <- as.vector(cs[, 2])

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    Component = parms$Component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @rdname get_statistic
#' @importFrom stats coef
#' @importFrom utils capture.output
#' @export
get_statistic.DirichletRegModel <- function(x, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  parms <- get_parameters(x)
  junk <- utils::capture.output(cs <- summary(x)$coef.mat)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = unname(cs[, "z value"]),
    Response = parms$Response,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (!is.null(parms$Component)) {
    out$Component <- parms$Component
  } else {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom methods slot
#' @export
get_statistic.glimML <- function(x, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }

  parms <- get_parameters(x)
  s <- methods::slot(aod::summary(x), "Coef")

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = s[, 3],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom stats coef vcov
#' @export
get_statistic.lrm <- function(x, ...) {
  parms <- get_parameters(x)
  stat <- stats::coef(x) / sqrt(diag(stats::vcov(x)))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(stat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}

#' @export
get_statistic.ols <- get_statistic.lrm

#' @export
get_statistic.rms <- get_statistic.lrm

#' @export
get_statistic.orm <- get_statistic.lrm

#' @export
get_statistic.psm <- get_statistic.lrm



#' @export
get_statistic.rma <- function(x, ...) {
  parms <- get_parameters(x)
  stat <- x$zval

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(stat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @importFrom stats qnorm
#' @export
get_statistic.metaplus <- function(x, ...) {
  params <- get_parameters(x)

  ci_low <- as.vector(x$results[, "95% ci.lb"])
  ci_high <- as.vector(x$results[, "95% ci.ub"])
  cis <- apply(cbind(ci_low, ci_high), MARGIN = 1, diff)
  se <- cis / (2 * stats::qnorm(.975))

  out <- data.frame(
    Parameter = params$Parameter,
    Statistic = as.vector(params$Estimate / se),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}



#' @export
get_statistic.bife <- function(x, ...) {
  parms <- get_parameters(x)
  cs <- summary(x)

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(cs$cm[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- find_statistic(x)
  out
}


#' @export
get_statistic.mediate <- function(x, ...) {
  NULL
}
