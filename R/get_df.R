#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract residual or model-based degrees of freedom
#'   from regression models.
#'
#' @param x A statistical model.
#' @param type Can be `"residual"`, `"wald"`, `"normal"`, or
#'   `"model"`. `"analytical"` is an alias for `"residual"`.
#'
#' - `"residual"` (aka `"analytical"`) returns `n-k` (number of observations
#'   minus number of estimated parameters). This is what [`stats::df.residual()]`
#'   usually returns. If residual degrees of freedom cannot be extracted,
#'   returns `Inf`.
#' - `"wald"`` returns residual (aka analytical) degrees of freedom for models
#'   with t-statistic, `1` for models with Chi-squared statistic, and `Inf` for
#'   all other models. Also returns `Inf` if residual degrees of freedom cannot
#'   be extracted.
#' - `"normal"` always returns `"Inf"`.
#' - `"model"` returns model-based degrees of freedom, i.e. the number of
#'   (estimated) parameters.
#'
#' For mixed models, can also be `"ml1"` or `"betwithin"`, and for models of
#' class `merMod`, `type` can also be `"satterthwaite"` or `"kenward-roger"`.
#' See 'Details'.
#'
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @details Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statistics. Unlike simpler approximation heuristics
#' like the "m-l-1" rule (`type = "ml1"`), the Satterthwaite or Kenward-Rogers
#' approximation is also applicable in more complex multilevel designs. However,
#' the "m-l-1" heuristic also applies to generalized mixed models, while
#' approaches like Kenward-Roger or Satterthwaite are limited to linear mixed
#' models only. The *between-within* denominator degrees of freedom approximation
#' is recommended in particular for (generalized) linear mixed models with repeated
#' measurements (longitudinal design). `get_df(type = "betwithin")` implements a
#' heuristic based on the between-within approach. **Note** that this implementation
#' does not return exactly the same results as shown in *Li and Redden 2015*,
#' but similar.
#'
#' @references
#' - Kenward, M. G., & Roger, J. H. (1997). Small sample inference for
#'   fixed effects from restricted maximum likelihood. Biometrics, 983-997.
#' - Satterthwaite FE (1946) An approximate distribution of estimates of
#'   variance components. Biometrics Bulletin 2 (6):110–4.
#' - Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019). Multilevel
#'   Analysis with Few Clusters: Improving Likelihood-based Methods to Provide
#'   Unbiased Estimates and Accurate Inference, British Journal of Political
#'   Science.
#' - Li, P., Redden, D. T. (2015). Comparing denominator degrees of freedom
#'   approximations for the generalized linear mixed model in analyzing binary
#'   outcome in small sample cluster-randomized trials. BMC Medical Research
#'   Methodology, 15(1), 38
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' get_df(model) # same as df.residual(model)
#' get_df(model, type = "model") # same as attr(logLik(model), "df")
#' @export
get_df <- function(x, ...) {
  UseMethod("get_df")
}


#' @rdname get_df
#' @export
get_df.default <- function(x, type = "residual", verbose = TRUE, ...) {
  # check valid options
  type <- match.arg(
    tolower(type),
    choices = c("residual", "model", "analytical", "wald", "normal", "ml1", "betwithin")
  )

  # check if user already passed "statistic" argument, to
  # avoid multiple calls to "find_statistic()"
  dots <- list(...)
  statistic <- dots$statistic
  if (is.null(statistic)) {
    statistic <- find_statistic(x)
  }

  # handle aliases
  if (type == "analytical") {
    type <- "residual"
  }


  # Wald normal approximation - always Inf -----
  if (type == "normal") {
    return(Inf)

  # residual/analytical df, falls back to Inf if we have no residual df method -----
  } else if (type == "residual") {
    dof <- .get_residual_df(x, verbose)

  # Wald df - always Inf for z-statistic, 1 for Chi2-statistic, else residual df -----
  } else if (type == "wald") {
    # z-statistic always Inf, *unless* we have residual df (which we have for some models)
    if (identical(statistic, "z-statistic")) {
      return(Inf)
    }
    # Chi2-statistic usually have 1 df
    if (identical(statistic, "chi-squared statistic")) {
      return(1)
    }
    # Wald t-statistic
    dof <- .get_residual_df(x, verbose)

  # ml1 - only for certain mixed models -----
  } else if (type == "ml1") {
    dof <- .degrees_of_freedom_ml1(x)

  # between-within - only for certain mixed models -----
  } else if (type == "betwithin") {
    dof <- .degrees_of_freedom_betwithin(x)

  # remaining option is model-based df, i.e. number of estimated parameters
  } else {
    dof <- .model_df(x)
  }

  if (!is.null(dof) && length(dof) > 0 && all(dof == 0) && isTRUE(verbose)) {
    warning("Model has zero degrees of freedom!", call. = FALSE)
  }

  if (is.null(dof) && isTRUE(verbose)) {
    warning("Could not extract degrees of freedom.", call. = FALSE)
  }

  dof
}


#' @export
get_df.model_fit <- function(x, type = "residual", verbose = TRUE, ...) {
  get_df(x$fit, type = type, verbose = verbose, ...)
}


#' @export
get_df.emmGrid <- function(x, ...) {
  if (!is.null(x@misc$is_boot) && x@misc$is_boot) {
    return(.boot_em_df(x))
  }
  unique(summary(x)$df)
}


#' @export
get_df.emm_list <- function(x, ...) {
  if (!is.null(x[[1]]@misc$is_boot) && x[[1]]@misc$is_boot) {
    return(.boot_em_df(x))
  }
  s <- summary(x)
  unname(unlist(lapply(s, function(i) {
    if (is.null(i$df)) {
      rep(Inf, nrow(i))
    } else {
      i$df
    }
  })))
}


#' @export
get_df.coeftest <- function(x, ...) {
  attributes(x)$df
}




# methods for models w/o df.residual() method --------------

#' @export
get_df.rlm <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal", "wald", "analytical"))
  statistic <- find_statistic(x)
  # Wald-z or normal
  if (type == "normal" || (type == "wald" && identical(statistic, "z-statistic"))) {
    return(Inf)
  # Wald Chi-squared
  } else if (type == "wald" && identical(statistic, "chi-squared statistic")) {
    return(1)
  # Residual or Wald-t
  } else if (type %in% c("residual", "wald", "analytical")) {
    .get_residual_df(x)
  } else {
    .model_df(x)
  }
}

#' @export
get_df.bigglm <- get_df.rlm

#' @export
get_df.biglm <- get_df.rlm

#' @export
get_df.complmrob <- get_df.rlm

#' @export
get_df.gls <- get_df.rlm

#' @export
get_df.garch <- get_df.rlm

#' @export
get_df.mhurdle <- get_df.rlm

#' @export
get_df.nlrq <- get_df.rlm

#' @export
get_df.truncreg <- get_df.rlm




# Mixed models - special treatment --------------

#' @export
get_df.lmerMod <- function(x, type = "residual", ...) {
  type <- match.arg(
    tolower(type),
    choices = c("residual", "model", "analytical", "satterthwaite", "kenward",
                "kenward-roger", "kr", "normal", "wald", "ml1", "betwithin")
  )

  if (type == "satterthwaite") {
    .degrees_of_freedom_satterthwaite(x)
  } else if (type %in% c("kr", "kenward", "kenward-roger")) {
    .degrees_of_freedom_kr(x)
  } else {
    get_df.default(x, type = type, ...)
  }
}

#' @export
get_df.lmerModTest <- get_df.lmerMod

#' @export
get_df.lme <- get_df.lmerMod




# Other models ------------------


#' @export
get_df.logitor <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal", "wald", "analytical"))
  if (type == "model") {
    .model_df(x)
  } else if (type == "normal") {
    return(Inf)
  } else {
    get_df.default(x$fit, type = type, ...)
  }
}

#' @export
get_df.poissonirr <- get_df.logitor

#' @export
get_df.negbinirr <- get_df.logitor

#' @export
get_df.poissonmfx <- get_df.logitor

#' @export
get_df.logitmfx <- get_df.logitor

#' @export
get_df.negbinmfx <- get_df.logitor

#' @export
get_df.probitmfx <- get_df.logitor

#' @export
get_df.betaor <- get_df.logitor

#' @export
get_df.betamfx <- get_df.logitor


#' @export
get_df.mira <- function(x, type = "residual", verbose = TRUE, ...) {
  # installed?
  check_if_installed("mice")
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal"))
  get_df(mice::pool(x), type, verbose = verbose, ...)
}


#' @export
get_df.mipo <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal"))
  if (type == "model") {
    .model_df(x)
  } else if (type == "normal") {
    return(Inf)
  } else {
    as.vector(summary(x)$df)
  }
}




# not yet supported --------------------

#' @export
get_df.mediate <- function(x, ...) {
  NULL
}




# Analytical approach ------------------------------


.get_residual_df <- function(x, verbose = TRUE) {
  dof <- .degrees_of_freedom_residual(x, verbose = verbose)
  if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
    dof <- .degrees_of_freedom_analytical(x)
  }
  dof
}


#' @keywords internal
.degrees_of_freedom_analytical <- function(model) {
  nparam <- .model_df(model)
  n <- n_obs(model)

  if (is.null(n)) {
    return(Inf)
  }

  return(n - nparam)
}




# Model approach (model-based / logLik df) ------------------------------


.model_df <- function(x) {
  dof <- tryCatch(attr(stats::logLik(x), "df"), error = function(e) NULL)

  if (is.null(dof) || all(is.infinite(dof)) || all(is.na(dof))) {
    r <- tryCatch(x$rank, error = function(e) NULL)
    if (!is.null(r)) {
      dof <- r + 1
    } else {
      n <- n_parameters(x)
      extra <- 0
      mi <- model_info(x, verbose = FALSE)

      if (mi$is_linear || mi$is_negbin) {
        extra <- extra + 1
      }

      dof <- n + extra
    }
  }

  dof
}


.boot_em_df <- function(model) {
  est <- get_parameters(model, summary = FALSE)
  rep(NA, ncol(est))
}
