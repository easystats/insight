#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract residual or model-based degrees of freedom from regression models.
#'
#' @param x A statistical model.
#' @param type Can be \code{"residual"} or \code{"model"}. \code{"residual"}
#' tries to extract residual degrees of freedoms. If residual degrees of freedom
#' could not be extracted, returns \code{n-k} (number of observations minus
#' number of parameters). \code{"model"} returns model-based degrees of freedom,
#' i.e. the number of (estimated) parameters.
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
  type <- match.arg(tolower(type), choices = c("residual", "model"))

  if (type == "residual") {
    dof <- .degrees_of_freedom_fit(x, verbose = verbose)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(x)
    }
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
get_df.ivFixed <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    as.vector(x$df)
  }
}


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
  unique(summary(x)$df)
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
get_df.glht <- function(x, type = "residual", ...) {
  type <- match.arg(tolower(type), choices = c("residual", "model"))
  if (type == "model") {
    .model_df(x)
  } else {
    x$df
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

#' @importFrom stats df.residual
#' @importFrom utils capture.output
#' @keywords internal
.degrees_of_freedom_fit <- function(model, verbose = TRUE) {
  info <- model_info(model, verbose = FALSE)

  if (!is.null(info) && is.list(info) && info$is_bayesian && !inherits(model, c("bayesx", "blmerMod", "bglmerMod"))) {
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
  if (inherits(dof, "try-error") || is.null(dof)) {
    junk <- utils::capture.output(dof = try(summary(model)$df[2], silent = TRUE))
  }

  # 3rd try, nlme
  if (inherits(dof, "try-error") || is.null(dof)) {
    dof <- try(unname(model$fixDF$X), silent = TRUE)
  }

  # last try
  if (inherits(dof, "try-error")) {
    dof <- NULL
  }

  dof
}



#' @importFrom stats logLik
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
    n <- n_parameters(x)
    dof <- ifelse(model_info(x)$is_linear, n + 1, n)
  }

  dof
}
