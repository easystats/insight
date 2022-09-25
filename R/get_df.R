#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract residual or model-based degrees of freedom
#'   from regression models.
#'
#' @param x A statistical model.
#' @param type Can be `"residual"`, `"wald"`, `"normal"`, `"analytical"`, or
#'   `"model"`.
#'
#' - `"residual"` tries to extract residual degrees of freedoms. If residual
#'   degrees of freedom cannot be extracted, returns analytical degrees of
#'   freedom, i.e. `n-k` (number of observations minus number of parameters).
#' - `"wald"` for models with z-statistic, returns `"Inf"`. Else, tries to
#'   extract residual degrees of freedoms. If residual degrees of freedom
#'   cannot be extracted, returns `"Inf"`.
#' - `"analytical"` for models with z-statistic, returns `"Inf"`. Else, returns
#'   analytical degrees of freedom, i.e. `n-k` (number of observations minus
#'   number of parameters).
#' - `"normal"` always returns `"Inf"`.
#' - `"model"` returns model-based degrees of freedom, i.e. the number of
#'   (estimated) parameters.
#'
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
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
  type <- match.arg(tolower(type), choices = c("residual", "model", "analytical", "wald", "normal"))

  # check if user already passed "statistic" argument, to 
  # avoid multiple calls to "find_statistic()"
  dots <- list(...)
  statistic <- dots$statistic
  if (is.null(statistic)) {
    statistic <- find_statistic(x)
  }

  # Wald normal approximation - always Inf -----
  if (type == "normal") {
    return(Inf)

  # residual df, falls back to analytical if we have no residual df method -----
  } else if (type == "residual") {
    dof <- .degrees_of_freedom_residual(x, verbose = verbose)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(x)
    }

  # Wald df - always Inf for z-statistic, else residual df -----
  } else if (type == "wald") {
    # z-statistic always Inf, *unless* we have residual df (which we have for some models)
    if (identical(statistic, "z-statistic")) {
      return(Inf)
    }
    dof <- .degrees_of_freedom_residual(x, verbose = verbose)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      return(Inf)
    }

  # analytical df - always Inf for z-statistic, else n-k -----
  } else if (type == "analytical") {
    # z-statistic always Inf, *unless* we have residual df (which we have for some models)
    if (identical(statistic, "z-statistic")) {
      return(Inf)
    }
    dof <- .degrees_of_freedom_analytical(x)

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
  stat <- find_statistic(x)
  if (type == "normal" || (type == "wald" && stat == "z-statistic")) {
    return(Inf)
  } else if (type %in% c("residual", "wald")) {
    .degrees_of_freedom_analytical(x)
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
  dots <- list(...)
  type <- match.arg(
    tolower(type),
    choices = c("residual", "model", "analytical", "satterthwaite", "kenward", "kenward-roger", "normal", "wald")
  )
  # fix name for lmerTest
  if (type == "kenward") {
    type <- "kenward-roger"
  }
  if (type == "satterthwaite") {
    .degrees_of_freedom_satterthwaite(x)
  } else if (type == "kenward-roger") {
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
    if (!is.null(x$rank)) {
      dof <- x$rank + 1
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
