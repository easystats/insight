#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract residual or model-based degrees of freedom
#'   from regression models.
#'
#' @param x A statistical model.
#' @param type Type of approximation for the degrees of freedom. Can be one of
#' the following:
#'
#'   + `"residual"` (aka `"analytical"`) returns the residual degrees of
#'     freedom, which usually is what [`stats::df.residual()`] returns. If a
#'     model object has no method to extract residual degrees of freedom, these
#'     are calculated as `n-p`, i.e. the number of observations minus the number
#'     of estimated parameters. If residual degrees of freedom cannot be extracted
#'     by either approach, returns `Inf`.
#'   + `"wald"` returns residual (aka analytical) degrees of freedom for models
#'     with t-statistic, `1` for models with Chi-squared statistic, and `Inf` for
#'     all other models. Also returns `Inf` if residual degrees of freedom cannot
#'     be extracted.
#'   + `"normal"` always returns `Inf`.
#'   + `"model"` returns model-based degrees of freedom, i.e. the number of
#'     (estimated) parameters.
#'   + For mixed models, can also be `"ml1"` (or `"m-l-1"`, approximation of
#'     degrees of freedom based on a "m-l-1" heuristic as suggested by _Elff et
#'     al. 2019_) or `"between-within"` (or `"betwithin"`).
#'   + For mixed models of class `merMod`, `type` can also be `"satterthwaite"`
#'     or `"kenward-roger"` (or `"kenward"`). See 'Details'.
#'
#' Usually, when degrees of freedom are required to calculate p-values or
#' confidence intervals, `type = "wald"` is likely to be the best choice in
#' most cases.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @details
#' **Degrees of freedom for mixed models**
#'
#' Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statistics (see _Li and Redden 2015_).
#'
#' *m-l-1 degrees of freedom*
#'
#' The _m-l-1_ heuristic is an approach that uses a t-distribution with fewer
#' degrees of freedom. In particular for repeated measure designs (longitudinal
#' data analysis), the m-l-1 heuristic is likely to be more accurate than simply
#' using the residual or infinite degrees of freedom, because `get_df(type = "ml1")`
#' returns different degrees of freedom for within-cluster and between-cluster
#' effects. Note that the "m-l-1" heuristic is not applicable (or at least less
#' accurate) for complex multilevel designs, e.g. with cross-classified clusters.
#' In such cases, more accurate approaches like the Kenward-Roger approximation
#' is recommended. However, the "m-l-1" heuristic also applies to generalized
#' mixed models, while approaches like Kenward-Roger or Satterthwaite are limited
#' to linear mixed models only.
#'
#' *Between-within degrees of freedom*
#'
#' The Between-within denominator degrees of freedom approximation is, similar
#' to the "m-l-1" heuristic, recommended in particular for (generalized) linear
#' mixed models with repeated measurements (longitudinal design).
#' `get_df(type = "betwithin")` implements a heuristic based on the between-within
#' approach, i.e. this type returns different degrees of freedom for within-cluster
#' and between-cluster effects. Note that this implementation does not return
#' exactly the same results as shown in _Li and Redden 2015_, but similar.
#'
#' *Satterthwaite and Kenward-Rogers degrees of freedom*
#'
#' Unlike simpler approximation heuristics like the "m-l-1" rule (`type = "ml1"`),
#' the Satterthwaite or Kenward-Rogers approximation is also applicable in more
#' complex multilevel designs. However, the "m-l-1" or "between-within" heuristics
#' also apply to generalized mixed models, while approaches like Kenward-Roger
#' or Satterthwaite are limited to linear mixed models only.
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
  # check for valid model-object
  if (missing(x) || is.null(x)) {
    format_error(
      "You must provide a model-object. Argument cannot be missing or `NULL`."
    )
  }

  # check valid options
  type <- match.arg(
    tolower(type),
    choices = c(
      "residual", "model", "analytical", "wald", "normal", "ml1", "betwithin",
      "between-within", "profile", "boot", "uniroot", "likelihood", "m-l-1", "any"
    )
  )

  # check if user already passed "statistic" argument, to
  # avoid multiple calls to "find_statistic()"
  dots <- list(...)
  statistic <- dots$statistic
  if (is.null(statistic)) {
    statistic <- find_statistic(x)
  }

  # handle aliases, resp. mixing type and ci_method
  type <- .check_df_type(type)

  if (type == "normal") { # nolint
    # Wald normal approximation - always Inf -----
    return(Inf)
  } else if (type == "residual") {
    # residual/analytical df, falls back to Inf if we have no residual df method -----
    dof <- .get_residual_df(x, verbose)
  } else if (type == "wald") {
    # Wald df - always Inf for z-statistic, 1 for Chi2-statistic, else residual df -----

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
  } else if (type %in% c("ml1", "m-l-1")) {
    dof <- .degrees_of_freedom_ml1(x)

    # between-within - only for certain mixed models -----
  } else if (type %in% c("betwithin", "between-within")) {
    dof <- .degrees_of_freedom_betwithin(x)

    # remaining option is model-based df, i.e. number of estimated parameters
  } else {
    dof <- .model_df(x)
  }

  if (!is.null(dof) && length(dof) > 0 && all(dof == 0) && isTRUE(verbose)) {
    format_warning("Model has zero degrees of freedom!")
  }

  if (is.null(dof) && isTRUE(verbose)) {
    format_warning("Could not extract degrees of freedom.")
  }

  dof
}


# models that rely on the default method ---------------
# ------------------------------------------------------

#' @export
get_df.model_fit <- function(x, type = "residual", verbose = TRUE, ...) {
  get_df(x$fit, type = type, verbose = verbose, ...)
}

#' @export
get_df.logitor <- function(x, type = "residual", verbose = TRUE, ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal", "wald", "analytical", "any"))
  get_df.default(x$fit, type = type, verbose = verbose, ...)
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


# models, where we want residual df, even for "wald" and "z-statistic" ---------
# ------------------------------------------------------------------------------

#' @export
get_df.rlm <- function(x, type = "residual", verbose = TRUE, ...) {
  if (identical(type, "model")) {
    .model_df(x)
  } else if (identical(type, "normal")) {
    Inf
  } else {
    .degrees_of_freedom_residual(x)
  }
}

#' @export
get_df.mipo <- get_df.rlm

#' @export
get_df.mhurdle <- get_df.rlm

#' @export
get_df.nlrq <- get_df.rlm

#' @export
get_df.complmrob <- get_df.rlm

#' @export
get_df.plm <- get_df.rlm

#' @export
get_df.multinom <- get_df.rlm

#' @export
get_df.garch <- get_df.rlm

#' @export
get_df.vgam <- get_df.rlm

#' @export
get_df.truncreg <- get_df.rlm

#' @export
get_df.systemfit <- get_df.rlm


# models, where we only have one type of residuals ---------
# ----------------------------------------------------------

#' @export
get_df.svy2lme <- function(x, type = "residual", verbose = TRUE, ...) {
  if (identical(type, "model")) {
    .model_df(x)
  } else {
    Inf
  }
}

#' @export
get_df.mmrm <- function(x, type = "residual", verbose = TRUE, ...) {
  if (identical(type, "model")) {
    .model_df(x)
  } else {
    summary_table <- stats::coef(summary(x))
    unname(summary_table[, "df"])
  }
}

#' @export
get_df.mmrm_fit <- get_df.mmrm

#' @export
get_df.mmrm_tmb <- get_df.mmrm


# special "model" classes ---------
# ---------------------------------

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
  unlist(lapply(s, function(i) {
    if (is.null(i$df)) {
      rep(Inf, nrow(i))
    } else {
      i$df
    }
  }), use.names = FALSE)
}

#' @export
get_df.mira <- function(x, type = "residual", verbose = TRUE, ...) {
  check_if_installed("mice")
  type <- match.arg(tolower(type), choices = c("residual", "model", "normal", "wald", "any", "analytical"))
  get_df(mice::pool(x), type, verbose = verbose, ...)
}

#' @export
get_df.coeftest <- function(x, ...) {
  attributes(x)$df
}

#' @export
get_df.glht <- function(x, ...) {
  x$df
}

#' @export
get_df.selection <- function(x, ...) {
  s <- summary(x)
  s$param$df
}

#' @export
get_df.serp <- function(x, type = "normal", ...) {
  if (identical(type, "model")) {
    .model_df(x)
  } else if (identical(type, "residual")) {
    x$rdf
  } else {
    Inf
  }
}


# Mixed models - special treatment --------------
# -----------------------------------------------

#' @export
get_df.lmerMod <- function(x, type = "residual", ...) {
  type <- match.arg(
    tolower(type),
    choices = c(
      "residual", "model", "analytical", "satterthwaite", "kenward",
      "kenward-roger", "kr", "normal", "wald", "ml1", "m-l-1", "betwithin",
      "between-within", "any"
    )
  )

  # hidden gem - required for get_predicted_ci(), where we have per-observation DF
  dots <- list(...)

  if (type %in% c("satterthwaite", "kr", "kenward", "kenward-roger") && isTRUE(dots$df_per_obs)) {
    .satterthwaite_kr_df_per_obs(x, type, dots$data)
  } else if (type == "satterthwaite") {
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
# -------------------------------

#' @export
get_df.phylolm <- function(x, type = "residual", ...) {
  if (identical(type, "model")) {
    stats::logLik(x)$df
  } else {
    get_df.default(x, type = type)
  }
}

#' @export
get_df.phyloglm <- get_df.phylolm

#' @export
get_df.fixest <- function(x, type = "residual", ...) {
  # fixest degrees of freedom can be tricky. best to use the function by the
  # package.
  check_if_installed("fixest")
  if (is.null(type)) {
    type <- "residual"
  }
  type <- match.arg(
    tolower(type),
    choices = c("wald", "residual", "normal", "any", "analytical")
  )
  type <- switch(type,
    any = ,
    wald = "t",
    analytical = ,
    residual = "resid",
    type
  )
  if (type == "normal") {
    return(Inf)
  }
  fixest::degrees_freedom(x, type = type)
}

#' @export
get_df.fixest_multi <- function(x, ...) {
  out <- do.call(rbind, lapply(x, get_df, ...))

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(x)

  # add response column
  out$Response <- id_columns$Response
  out$Group <- id_columns$Group

  row.names(out) <- NULL
  out
}

#' @export
get_df.nestedLogit <- function(x, type = NULL, component = "all", verbose = TRUE, ...) {
  if (is.null(type)) {
    type <- "wald"
  }
  if (tolower(type) == "residual") {
    cf <- as.data.frame(stats::coef(x))
    dof <- rep(vapply(x$models, stats::df.residual, numeric(1)), each = nrow(cf))
    if (!is.null(component) && !identical(component, "all")) {
      comp <- intersect(names(dof), component)
      if (length(comp)) {
        dof <- dof[comp]
      } else {
        if (verbose) {
          format_alert(paste0(
            "No matching model found. Possible values for `component` are ",
            toString(paste0("'", names(x$models), "'")),
            "."
          ))
        }
        dof <- Inf
      }
    }
  } else {
    dof <- Inf
  }
  dof
}


# not yet supported --------------------
# --------------------------------------

#' @export
get_df.mediate <- function(x, ...) {
  NULL
}


# Analytical approach ------------------------------
# --------------------------------------------------

.get_residual_df <- function(x, verbose = TRUE) {
  dof <- .degrees_of_freedom_residual(x, verbose = verbose)
  if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
    dof <- .degrees_of_freedom_analytical(x)
  }
  dof
}

#' @keywords internal
.degrees_of_freedom_analytical <- function(x, kenward = TRUE, model_n_params = TRUE) {
  if (isTRUE(model_n_params)) {
    nparam <- .model_df(x)
  } else {
    nparam <- n_parameters(x)
  }

  n <- n_obs(x)
  if (is.null(n)) {
    return(Inf)
  }

  if (isTRUE(kenward) && inherits(x, "lmerMod")) {
    dof <- as.numeric(.degrees_of_freedom_kr(x))
  } else {
    dof <- n - nparam
  }

  dof
}


# Model approach (model-based / logLik df) ------------------------------
# -----------------------------------------------------------------------

.model_df <- function(x) {
  # logLik() for plm calls get_df(), so we would have a recursion here.
  # therefore, we need to check for plm first
  if (inherits(x, "plm")) {
    dof <- NULL
  } else {
    dof <- .safe(attr(stats::logLik(x), "df"))
  }

  if (is.null(dof) || all(is.infinite(dof)) || all(is.na(dof))) {
    r <- .safe(x$rank)
    if (is.null(r)) {
      n <- n_parameters(x)
      extra <- 0
      mi <- model_info(x, verbose = FALSE)

      if (mi$is_linear || mi$is_negbin) {
        extra <- extra + 1
      }

      dof <- n + extra
    } else {
      dof <- r + 1
    }
  }

  dof
}


# tools ----------------
# ----------------------

.boot_em_df <- function(model) {
  est <- get_parameters(model, summary = FALSE)
  rep(NA, ncol(est))
}

.check_df_type <- function(type) {
  # handle mixing of ci_method and type arguments
  if (tolower(type) %in% c("profile", "uniroot", "quantile", "likelihood", "eti", "hdi", "bci", "boot", "spi", "nokr", "any")) {
    type <- "wald"
  } else if (tolower(type) == "analytical") {
    type <- "residual"
  }
  type
}

.get_fixest_multi_columns <- function(model) {
  # add response and group columns
  s <- summary(model)
  l <- lengths(lapply(s, stats::coef))
  parts <- strsplit(names(l), ";", fixed = TRUE)

  id_columns <- Map(function(i, j) {
    if (length(j) == 1 && startsWith(j, "rhs")) {
      data.frame(
        Group = rep(trim_ws(sub("rhs:", "", j, fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    } else if (length(j) == 1 && startsWith(j, "lhs")) {
      data.frame(
        Response = rep(trim_ws(sub("lhs:", "", j, fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Response = rep(trim_ws(sub("lhs:", "", j[1], fixed = TRUE)), i),
        Group = rep(trim_ws(sub("rhs:", "", j[2], fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    }
  }, unname(l), parts)

  do.call(rbind, id_columns)
}
