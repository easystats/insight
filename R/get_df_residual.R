# Model approach (Residual df) ------------------------------

#' @keywords internal
.degrees_of_freedom_residual <- function(x, ...) {
  UseMethod(".degrees_of_freedom_residual")
}


#' @keywords internal
.degrees_of_freedom_residual.default <- function(x, verbose = TRUE, ...) {
  if (.is_bayesian_model(x, exclude = c("bmerMod", "bayesx", "blmerMod", "bglmerMod"))) {
    if (check_if_installed("bayestestR", quietly = TRUE)) {
      x <- .safe(bayestestR::bayesian_as_frequentist(x))
      if (is.null(x)) {
        if (isTRUE(verbose)) {
          format_warning("Can't extract degrees of freedom from Bayesian model.")
        }
        return(NULL)
      }
    } else {
      if (isTRUE(verbose)) {
        format_warning("Can't extract degrees of freedom from Bayesian model.")
      }
      return(NULL)
    }
  }

  # 1st try
  dof <- try(stats::df.residual(x), silent = TRUE)

  # 2nd try
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    junk <- utils::capture.output(dof = try(summary(x)$df[2], silent = TRUE))
  }

  # last try
  if (inherits(dof, "try-error")) {
    dof <- NULL
  }

  dof
}

#' @keywords internal
.degrees_of_freedom_residual.lme <- function(x, verbose = TRUE, ...) {
  dof <- try(unname(x$fixDF$X), silent = TRUE)
  # last try
  if (inherits(dof, "try-error")) {
    dof <- NULL
  }
  dof
}


# residual DF based on n-k -----------
# ------------------------------------

#' @keywords internal
.degrees_of_freedom_residual.gls <- function(x, verbose = TRUE, ...) {
  # we don't call ".degrees_of_freedom_analytical()" here, because that
  # function relies on `.model_df()` to estimate the number of parameters,
  # which returns results that are not in line with the "summary()" for gls
  .degrees_of_freedom_analytical(x, kenward = FALSE, model_n_params = FALSE)
}

#' @keywords internal
.degrees_of_freedom_residual.rlm <- .degrees_of_freedom_residual.gls

#' @keywords internal
.degrees_of_freedom_residual.mhurdle <- .degrees_of_freedom_residual.gls

#' @keywords internal
.degrees_of_freedom_residual.truncreg <- .degrees_of_freedom_residual.gls

#' @keywords internal
.degrees_of_freedom_residual.garch <- .degrees_of_freedom_residual.gls

#' @keywords internal
.degrees_of_freedom_residual.complmrob <- .degrees_of_freedom_residual.gls

#' @keywords internal
.degrees_of_freedom_residual.biglm <- function(x, verbose = TRUE, ...) {
  if (!is.null(x$df.resid)) {
    x$df.resid
  } else {
    .degrees_of_freedom_analytical(x, kenward = FALSE, model_n_params = FALSE)
  }
}

#' @keywords internal
.degrees_of_freedom_residual.bigglm <- .degrees_of_freedom_residual.biglm


# residual DF taken from model objects -----------
# ------------------------------------------------

#' @keywords internal
.degrees_of_freedom_residual.glimML <- function(x, verbose = TRUE, ...) {
  check_if_installed("aod")
  aod::df.residual(x)
}

#' @keywords internal
.degrees_of_freedom_residual.ivFixed <- function(x, verbose = TRUE, ...) {
  as.vector(x$df)
}

#' @keywords internal
.degrees_of_freedom_residual.ivprobit <- .degrees_of_freedom_residual.ivFixed

#' @keywords internal
.degrees_of_freedom_residual.multinom <- function(x, verbose = TRUE, ...) {
  n_obs(x) - x$edf
}

#' @keywords internal
.degrees_of_freedom_residual.nnet <- .degrees_of_freedom_residual.multinom

#' @keywords internal
.degrees_of_freedom_residual.rqss <- .degrees_of_freedom_residual.multinom

#' @keywords internal
.degrees_of_freedom_residual.fixest <- function(x, verbose = TRUE, ...) {
  check_if_installed("fixest")
  fixest::degrees_freedom(x, type = "resid")
}

#' @keywords internal
.degrees_of_freedom_residual.summary.lm <- function(x, verbose = TRUE, ...) {
  x$fstatistic[3]
}

#' @keywords internal
.degrees_of_freedom_residual.lqmm <- function(x, verbose = TRUE, ...) {
  cs <- summary(x)
  tryCatch(
    if (is.null(cs$rdf)) {
      attr(cs$B, "R") - 1
    } else {
      cs$rdf
    },
    error = function(e) {
      NULL
    }
  )
}

#' @keywords internal
.degrees_of_freedom_residual.lqm <- .degrees_of_freedom_residual.lqmm

#' @keywords internal
.degrees_of_freedom_residual.cgam <- function(x, verbose = TRUE, ...) {
  # x$resid_df_obs
  # new in cgam 1.18
  stats::df.residual(x)
}

#' @keywords internal
.degrees_of_freedom_residual.cgamm <- function(x, verbose = TRUE, ...) {
  x$resid_df_obs
}

#' @keywords internal
.degrees_of_freedom_residual.mipo <- function(x, verbose = TRUE, ...) {
  as.vector(summary(x)$df)
}

#' @keywords internal
.degrees_of_freedom_residual.hglm <- function(x, verbose = TRUE, ...) {
  x$dfReFe
}

#' @keywords internal
.degrees_of_freedom_residual.serp <- function(x, verbose = TRUE, ...) {
  x$rdf
}

#' @keywords internal
.degrees_of_freedom_residual.glht <- function(x, verbose = TRUE, ...) {
  x$df
}

#' @keywords internal
.degrees_of_freedom_residual.BBmm <- .degrees_of_freedom_residual.glht

#' @keywords internal
.degrees_of_freedom_residual.BBreg <- .degrees_of_freedom_residual.glht

#' @keywords internal
.degrees_of_freedom_residual.rq <- function(x, verbose = TRUE, ...) {
  tryCatch(
    {
      s <- suppressWarnings(summary(x, covariance = TRUE))
      cs <- lapply(s, function(i) i$rdf)
      unique(unlist(cs, use.names = FALSE))
    },
    error = function(e) {
      NULL
    }
  )
}

#' @keywords internal
.degrees_of_freedom_residual.rqs <- .degrees_of_freedom_residual.rq

#' @keywords internal
.degrees_of_freedom_residual.bfsl <- function(x, verbose = TRUE, ...) {
  x$df.residual
}

#' @keywords internal
.degrees_of_freedom_residual.plm <- function(x, verbose = TRUE, ...) {
  x$df.residual
}

#' @keywords internal
.degrees_of_freedom_residual.merModList <- function(x, verbose = TRUE, ...) {
  s <- suppressWarnings(summary(x))
  s$fe$df
}

#' @keywords internal
.degrees_of_freedom_residual.vgam <- function(x, verbose = TRUE, ...) {
  params <- get_parameters(x)
  out <- stats::setNames(rep(NA, nrow(params)), params$Parameter)
  out[names(x@nl.df)] <- x@nl.df
  out
}

#' @keywords internal
.degrees_of_freedom_residual.systemfit <- function(x, verbose = TRUE, ...) {
  dof <- NULL
  s <- summary(x)$eq
  params <- find_parameters(x)
  f <- find_formula(x, verbose = FALSE)
  system_names <- names(f)

  for (i in seq_along(system_names)) {
    dfs <- rep(s[[i]]$df[2], length(params[[i]]))
    df_names <- rep(names(params[i]), length(params[[i]]))
    dof <- c(dof, stats::setNames(dfs, df_names))
  }

  dof
}


# helper ----------------------

.dof_fit_gam <- function(model, dof) {
  params <- find_parameters(model)
  if (!is.null(params$conditional)) {
    dof <- rep(dof, length(params$conditional))
  }
  if (!is.null(params$smooth_terms)) {
    s <- summary(model)
    dof <- c(dof, s$s.table[, "Ref.df"])
  }
  dof
}
