#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract residual or model-based degrees of freedom
#'   from regression models.
#'
#' @param x A statistical model.
#' @param type Can be `"residual"`, `"model"` or `"analytical"`. `"residual"`
#' tries to extract residual degrees of freedoms. If residual degrees of freedom
#' could not be extracted, returns analytical degrees of freedom, i.e. `n-k`
#' (number of observations minus number of parameters). `"model"` returns
#' model-based degrees of freedom, i.e. the number of (estimated) parameters.
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
  type <- match.arg(tolower(type), choices = c("residual", "model", "analytical"))

  if (type == "residual") {
    dof <- .degrees_of_freedom_residual(x, verbose = verbose)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(x)
    }
  } else if (type == "analytical") {
    dof <- .degrees_of_freedom_analytical(x)
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
get_df.ivFixed <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    as.vector(x$df)
  }
}

#' @export
get_df.ivprobit <- get_df.ivFixed


#' @export
get_df.multinom <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    n_obs(x) - x$edf
  }
}

#' @export
get_df.nnet <- get_df.multinom


#' @export
get_df.summary.lm <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    x$fstatistic[3]
  }
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


#' @export
get_df.lqmm <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    cs <- summary(x)
    tryCatch(
      {
        if (!is.null(cs$rdf)) {
          cs$rdf
        } else {
          attr(cs$B, "R") - 1
        }
      },
      error = function(e) {
        NULL
      }
    )
  }
}


#' @export
get_df.lqm <- get_df.lqmm


#' @export
get_df.cgam <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    x$resid_df_obs
  }
}


#' @export
get_df.glht <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    x$df
  }
}

#' @export
get_df.BBmm <- get_df.glht

#' @export
get_df.BBreg <- get_df.glht


#' @export
get_df.rlm <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "residual") {
    .degrees_of_freedom_analytical(x)
  } else {
    .model_df(x)
  }
}


#' @export
get_df.plm <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "residual") {
    x$df.residual
  } else {
    .model_df(x)
  }
}


#' @export
get_df.selection <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    s <- summary(x)
    s$param$df
  }
}


#' @export
get_df.logitor <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    get_df.default(x$fit, ...)
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
get_df.merModList <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    s <- suppressWarnings(summary(x))
    s$fe$df
  }
}


#' @export
get_df.mira <- function(x, type = "residual", ...) {
  # installed?
  check_if_installed("mice")
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  get_df(mice::pool(x), type, ...)
}


#' @export
get_df.mipo <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    as.vector(summary(x)$df)
  }
}


#' @export
get_df.vgam <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    params <- get_parameters(x)
    out <- stats::setNames(rep(NA, nrow(params)), params$Parameter)
    out[names(x@nl.df)] <- x@nl.df
    out
  }
}


#' @export
get_df.rqs <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    tryCatch(
      {
        s <- suppressWarnings(summary(x, covariance = TRUE))
        cs <- lapply(s, function(i) i$rdf)
        unique(unlist(cs))
      },
      error = function(e) {
        NULL
      }
    )
  }
}


#' @export
get_df.systemfit <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  df <- c()
  s <- summary(x)$eq
  params <- find_parameters(x)
  f <- find_formula(x)
  system_names <- names(f)

  for (i in 1:length(system_names)) {
    dfs <- rep(s[[i]]$df[2], length(params[[i]]))
    df_names <- rep(names(params[i]), length(params[[i]]))
    df <- c(df, stats::setNames(dfs, df_names))
  }

  df
}






# Analytical approach ------------------------------


#' @keywords internal
.degrees_of_freedom_analytical <- function(model) {
  nparam <- n_parameters(model)
  n <- n_obs(model)

  if (is.null(n)) {
    return(Inf)
  }

  return(n - nparam)
}




# Model approach (Residual df) ------------------------------


#' @keywords internal
.degrees_of_freedom_residual <- function(model, verbose = TRUE) {
  if (.is_bayesian_model(model) && !inherits(model, c("bayesx", "blmerMod", "bglmerMod"))) {
    if (requireNamespace("bayestestR", quietly = TRUE)) {
      model <- bayestestR::bayesian_as_frequentist(model)
    } else {
      if (isTRUE(verbose)) {
        warning("Can't extract degrees of freedom from Bayesian model.", call. = FALSE)
      }
      return(NULL)
    }
  }

  # 1st try
  dof <- try(stats::df.residual(model), silent = TRUE)

  # 2nd try
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    junk <- utils::capture.output(dof = try(summary(model)$df[2], silent = TRUE))
  }

  # 3rd try, nlme
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    dof <- try(unname(model$fixDF$X), silent = TRUE)
  }

  # last try
  if (inherits(dof, "try-error")) {
    dof <- NULL
  }

  dof
}




# Model approach (model-based / logLik df) ------------------------------


.model_df <- function(x) {
  dof <- tryCatch(
    {
      attr(stats::logLik(x), "df")
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(dof) || all(is.infinite(dof)) || all(is.na(dof))) {
    if (!is.null(x$rank)) {
      dof <- x$rank + 1
    } else {
      n <- n_parameters(x)
      extra <- 0
      mi <- model_info(x)

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
