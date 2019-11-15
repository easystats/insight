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
#'   (default). May be abbreviated.
#' @param ... Currently not used.
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
get_statistic.plm <- get_statistic.default


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
get_statistic.glmmTMB <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
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
    data.frame(
      Parameter = find_parameters(x, effects = "fixed", component = comp, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
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
    Response = parms$Response,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

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







# Other models -------------------------------------------------------


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
get_statistic.crq <- get_statistic.rq

#' @export
get_statistic.nlrq <- get_statistic.rq


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


#' @importFrom stats coef
#' @export
get_statistic.gee <- function(x, ...) {
  parms <- get_parameters(x)
  cs <- stats::coef(summary(x))

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = as.vector(cs[, "Naive z"]),
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



#' @importFrom stats coef
#' @export
get_statistic.betareg <- function(x, ...) {
  parms <- get_parameters(x)
  cs <- do.call(rbind, stats::coef(summary(x)))
  se <- as.vector(cs[, 2])

  out <- data.frame(
    Parameter = parms$Parameter,
    Statistic = parms$Estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

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
