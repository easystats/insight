#' @title Extract degrees of freedom
#' @name get_df
#'
#' @description Estimate or extract (residual) degrees of freedom from regression models.
#'
#' @param x A statistical model.
#' @param method Can be \code{"residual"}, in which case they are directly taken from the model if available (for Bayesian models, the goal (looking for help to make it happen) would be to refit the model as a frequentist one before extracting the degrees of freedom), \code{"analytical"} (degrees of freedom are estimated based on the model type), or \code{"any"}, which tries to extract degrees of freedom by any of those methods, whichever succeeds.
#' @param ... Currently not used.
#'
#' @details Methods for calculating degrees of freedom:
#' \itemize{
#' \item \code{"residual"} tries to extract residual degrees of freedoms, and returns \code{Inf} if residual degrees of freedom could not be extracted.
#' \item \code{"analytical"} degrees of freedom are \code{n-k} (number of observations minus number of parameters).
#' \item \code{"any"} first tries to extract residual degrees of freedom, and if these are not available, extracts analytical degrees of freedom.
#' }
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' get_df(model)
#'
#' model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
#' get_df(model)
#' @export
get_df <- function(model, ...) {
  UseMethod("get_df")
}


#' @rdname get_df
#' @export
get_df.default <- function(model, method = "any", ...) {
  method <- tolower(method)
  method <- match.arg(method, c("analytical", "any", "residual"))

  if (method == "any") {
    dof <- .degrees_of_freedom_fit(model, verbose = FALSE)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(model)
    }
  } else if (method == "analytical") {
    dof <- .degrees_of_freedom_analytical(model)
  } else {
    dof <- .degrees_of_freedom_fit(model)
  }

  if (!is.null(dof) && length(dof) > 0 && all(dof == 0)) {
    warning("Model has zero degrees of freedom!", call. = FALSE)
  }

  dof
}


#' @export
get_df.ivFixed <- function(x, ...) {
  as.vector(x$df)
}


#' @export
get_df.summary.lm <- function(x, ...) {
  x$fstatistic[3]
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
get_df.lqmm <- function(x, ...) {
  cs <- summary(model)
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


#' @export
get_df.lqm <- get_df.lqmm


#' @export
get_df.glht <- function(x, ...) {
  x$df
}


#' @export
get_df.logitor <- function(x, ...) {
  get_df.default(x$fit, ...)
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
  if (inherits(dof, "try-error") || is.null(dof)) {
    dof <- Inf
    if (verbose) {
      warning("Could not extract degrees of freedom.", call. = FALSE)
    }
  }


  dof
}
