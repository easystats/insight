# Model approach (Residual df) ------------------------------

#' @keywords internal
.degrees_of_freedom_residual <- function(x, ...) {
  UseMethod(".degrees_of_freedom_residual")
}


#' @keywords internal
.degrees_of_freedom_residual.default <- function(x, verbose = TRUE, ...) {
  if (.is_bayesian_model(x) && !inherits(x, c("bayesx", "blmerMod", "bglmerMod"))) {
    if (check_if_installed("bayestestR", quietly = TRUE)) {
      x <- tryCatch(bayestestR::bayesian_as_frequentist(x),
                    error = function(e) NULL)
      if (is.null(x)) {
        format_warning("Can't extract degrees of freedom from Bayesian model.")
      }
      return(NULL)
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

#' @keywords internal
.degrees_of_freedom_residual.gls <- function(x, verbose = TRUE, ...) {
  nparam <- n_parameters(x)
  n <- n_obs(x)

  if (is.null(n) || is.null(nparam)) {
    return(Inf)
  }

  return(n - nparam)
}

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
.degrees_of_freedom_residual.glht <- function(x, verbose = TRUE, ...) {
  x$df
}

#' @export
.degrees_of_freedom_residual.BBmm <- .degrees_of_freedom_residual.glht

#' @export
.degrees_of_freedom_residual.BBreg <- .degrees_of_freedom_residual.glht

#' @keywords internal
.degrees_of_freedom_residual.rq <- function(x, verbose = TRUE, ...) {
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

#' @keywords internal
.degrees_of_freedom_residual.rqs <- function(x, verbose = TRUE, ...) {
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

#' @keywords internal
.degrees_of_freedom_residual.rqss <- function(x, verbose = TRUE, ...) {
  n_obs(x) - x$edf
}

#' @keywords internal
.degrees_of_freedom_residual.bfsl <- function(x, verbose = TRUE, ...) {
  x$df.residual
}

#' @keywords internal
.degrees_of_freedom_residual.plm <- function(x, verbose = TRUE, ...) {
  x$df.residual
}

#' @keywords internal
.degrees_of_freedom_residual.selection <- function(x, verbose = TRUE, ...) {
  s <- summary(x)
  s$param$df
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
  df <- c()
  s <- summary(x)$eq
  params <- find_parameters(x)
  f <- find_formula(x)
  system_names <- names(f)

  for (i in seq_along(system_names)) {
    dfs <- rep(s[[i]]$df[2], length(params[[i]]))
    df_names <- rep(names(params[i]), length(params[[i]]))
    df <- c(df, stats::setNames(dfs, df_names))
  }

  df
}
