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
#' @return
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
  cs_names <- tolower(dimnames(cs)[[2]])

  out <- data.frame(
    parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    statistic = as.vector(cs[, column_index]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (any(c("t val.", "t", "t-value", "t.value", "t value", "tvalue") %in% cs_names))
    attr(out, "statistic") <- "t"
  else if (any(c("z val.", "z", "z-value", "z.value", "z value", "zvalue", "wald") %in% cs_names))
    attr(out, "statistic") <- "z"
  else
    attr(out, "statistic") <- "statistic"

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


#' @export
get_statistic.coxph <- function(x, ...) {
  get_statistic.default(x, column_index = 4)
}





# Models with zero-inflation component --------------------------------------


#' @importFrom stats coef
#' @rdname get_statistic
#' @export
get_statistic.glmmTMB <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(x)))
  out <- lapply(names(cs), function(i) {
    data.frame(
      parameter = find_parameters(x, effects = "fixed", component = i, flatten = TRUE),
      statistic = as.vector(cs[[i]][, 3]),
      component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, out)
  stat$component <- .rename_values(stat$component, "cond", "conditional")
  stat$component <- .rename_values(stat$component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}


#' @export
get_statistic.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(x)))
  out <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    data.frame(
      parameter = find_parameters(x, effects = "fixed", component = comp, flatten = TRUE),
      statistic = as.vector(cs[[i]][, 3]),
      component = comp,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, out)
  stat$component <- .rename_values(stat$component, "cond", "conditional")
  stat$component <- .rename_values(stat$component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

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
      parameter = find_parameters(x, effects = "fixed", component = i, flatten = TRUE),
      statistic = as.vector(cs[[i]][, 3]),
      component = i,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  stat <- do.call(rbind, out)
  .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}






# gam models --------------------------------------------------------------


#' @importFrom stats na.omit
#' @export
get_statistic.Gam <- function(model, ...) {
  p.aov <- stats::na.omit(summary(model)$parametric.anova)

  out <- data.frame(
    Parameter = rownames(p.aov),
    Statistic = as.vector(p.aov[, 4]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "F"
  out
}


#' @export
get_statistic.gam <- function(model, ...) {
  cs <- summary(model)$p.table
  cs.smooth <- summary(model)$s.table

  out <- data.frame(
    parameter = c(rownames(cs), rownames(cs.smooth)),
    statistic = c(as.vector(cs[, 3]), as.vector(cs.smooth[, 3])),
    component = c(rep("conditional", nrow(cs)), rep("smooth_terms", nrow(cs.smooth))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (model_info(model)$is_binomial) {
    attr(out, "statistic") <- "z / Chisq"
  } else {
    attr(out, "statistic") <- "t / F"
  }

  out
}


#' @export
get_statistic.gamm <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  get_statistic.gam(model, ...)
}


#' @export
get_statistic.list <- function(model, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    get_statistic.gam(model, ...)
  }
}


#' @importFrom utils capture.output
#' @export
get_statistic.gamlss <- function(model, ...) {
  parms <- get_parameters(model)
  utils::capture.output(cs <- summary(model))

  out <- data.frame(
    parameter = parms$parameter,
    statistic = as.vector(cs[, 3]),
    component = parms$component,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "t"
  out
}



#' @export
get_statistic.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' needed for this function to work. Please install it.")
  }

  cs <- VGAM::coef(VGAM::summary(model))

  out <- data.frame(
    parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}






# Other models -------------------------------------------------------


#' @export
get_statistic.LORgee <- function(x, ...) {
  out <- get_statistic.default(x)
  attr(out, "statistic") <- "z"
  out
}


#' @export
get_statistic.crch <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model), model = "full"))
  params <- get_parameters(model)

  out <- data.frame(
    Parameter = params$parameter,
    Statistic = as.vector(cs[, 3]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}


#' @importFrom stats coef
#' @export
get_statistic.gee <- function(model, ...) {
  parms <- get_parameters(model)
  cs <- stats::coef(summary(model))

  out <- data.frame(
    parameter = parms$parameter,
    statistic = as.vector(cs[, "Naive z"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}



#' @importFrom stats qchisq
#' @importFrom utils capture.output
#' @export
get_statistic.logistf <- function(model, ...) {
  parms <- get_parameters(model)
  utils::capture.output(s <- summary(model))

  out <- data.frame(
    parameter = parms$parameter,
    statistic = as.vector(stats::qchisq(1 - s$prob, df = 1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "chisq"
  out
}



#' @importFrom stats vcov
#' @export
get_statistic.svyglm.nb <- function(x, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }

  parms <- get_parameters(x)
  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))

  out <- data.frame(
    parameter = parms$parameter,
    statistic = parms$estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "t"
  out
}



#' @export
get_statistic.svyglm.zip <- get_statistic.svyglm.nb



#' @importFrom stats coef
#' @export
get_statistic.betareg <- function(model, ...) {
  parms <- get_parameters(model)
  cs <- do.call(rbind, stats::coef(summary(model)))
  se <- as.vector(cs[, 2])

  out <- data.frame(
    parameter = parms$parameter,
    statistic = parms$estimate / se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}



#' @importFrom stats vcov
#' @export
get_statistic.coxme <- function(model, ...) {
  beta <- model$coefficients
  out <- NULL

  if (length(beta) > 0) {
    out <- data.frame(
      Parameter = names(beta),
      Statistic = as.vector(beta / sqrt(diag(stats::vcov(model)))),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    attr(out, "statistic") <- "z"
  }

  out
}



#' @export
get_statistic.survreg <- function(model, ...) {
  parms <- get_parameters(model)
  s <- summary(model)
  out <- data.frame(
    parameter = parms$parameter,
    statistic = s$table[, 3],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}



#' @importFrom methods slot
#' @export
get_statistic.glimML <- function(model, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }

  parms <- get_parameters(model)
  s <- methods::slot(aod::summary(model), "Coef")

  out <- data.frame(
    parameter = parms$parameter,
    statistic = s[, 3],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}



#' @importFrom stats coef vcov
#' @export
get_statistic.lrm <- function(model, ...) {
  parms <- get_parameters(model)
  stat <- stats::coef(model) / sqrt(diag(stats::vcov(model)))

  out <- data.frame(
    parameter = parms$parameter,
    statistic = as.vector(stat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(out, "statistic") <- "z"
  out
}


#' @export
get_statistic.ols <- get_statistic.lrm


#' @export
get_statistic.rms <- get_statistic.lrm


#' @export
get_statistic.psm <- get_statistic.lrm
