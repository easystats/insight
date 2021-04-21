#' @title Get residual standard deviation from models
#'
#' @description Returns \code{sigma}, which corresponds the estimated standard deviation of the residuals. This function extends the \code{sigma()} base R generic for models that don't have implemented it. It also computes the confidence interval (CI), which is stored as an attribute.
#'
#' Sigma is a key-component of regression models, and part of the so-called auxiliary parameters that are estimated. Indeed, linear models for instance assume that the residuals comes from a normal distribution with mean 0 and standard deviation \code{sigma}. See the details section below for more information about its interpretation and calculation.
#'
#' @name get_sigma
#'
#' @param x A model.
#' @param ci The CI width.
#' @inheritParams find_parameters
#'
#' @return The residual standard deviation (sigma), or \code{NULL} if this information could not be accessed.
#'
#' @details
#'   \subsection{Interpretation of Sigma}{
#'   The residual standard deviation, \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}},
#'   indicates that the predicted outcome will be within +/- \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}}
#'   units of the linear predictor for approximately 68\% of the data points
#'   (\cite{Gelman, Hill & Vehtari 2020, p.84}). In other words, the residual
#'   standard deviation indicates the accuracy for a model to predict scores,
#'   thus it can be thought of as \dQuote{a measure of the average distance
#'   each observation falls from its prediction from the model}
#'   (\cite{Gelman, Hill & Vehtari 2020, p.168}). \ifelse{html}{\out{&sigma;}}{\eqn{\sigma}}
#'   can be considered as a measure of the unexplained variation in the data,
#'   or of the precision of inferences about regression coefficients.
#'   }
#'   \subsection{Calculation of Sigma}{
#'   By default, \code{get_sigma()} tries to extract sigma by calling
#'   \code{stats::sigma()}. If the model-object has no \code{sigma()} method,
#'   the next step is calculating sigma as square-root of the model-deviance
#'   divided by the residual degrees of freedom. Finally, if even this approach
#'   fails, and \code{x} is a mixed model, the residual standard deviation is
#'   accessed using the square-root from \code{get_variance_residual()}.
#'   }
#'
#' @references Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other Stories. Cambridge University Press.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_sigma(m)
#' @export
get_sigma <- function(x, ci = 0.95, verbose = TRUE) {
  s <- .get_sigma(x, verbose = verbose)

  # Confidence interval for sigma
  ci <- tryCatch(
    {
      .get_sigma_ci(x, ci = ci)
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(ci)) {
    attributes(s) <- c(attributes(s), ci)
  }

  s
}


# Retrieve sigma ----------------------------------------------------------


.get_sigma <- function(x, ...) {
  UseMethod(".get_sigma")
}


# special handling ---------------

.get_sigma.model_fit <- function(x, verbose = TRUE, ...) {
  .get_sigma(x$fit, verbose = verbose)
}

.get_sigma.lrm <- function(x, verbose = TRUE, ...) {
  s <- stats::sigma(x)
  s[length(s)]
}

.get_sigma.merModList <- function(x, verbose = TRUE, ...) {
  s <- suppressWarnings(summary(x))
  s$residError
}

.get_sigma.summary.lm <- function(x, verbose = TRUE, ...) {
  x$sigma
}

.get_sigma.cpglmm <- function(x, verbose = TRUE, ...) {
  tryCatch(
    {
      stats::deviance(x)[["sigmaML"]]
    },
    error = function(e) {
      NULL
    }
  )
}

# default handling ---------------

.get_sigma.default <- function(x, verbose = TRUE, ...) {
  if (inherits(x, c("mipo", "mira", "riskRegression"))) {
    return(NULL)
  }

  # default sigma ---------------
  s <- tryCatch(
    {
      stats::sigma(x)
    },
    error = function(e) {
      NULL
    }
  )

  # compute sigma manually ---------------
  if (.is_empty_object(s)) {
    s <- tryCatch(
      {
        sqrt(get_deviance(x, verbose = verbose) / get_df(x, type = "residual", verbose = verbose))
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (.is_empty_object(s)) {
    info <- model_info(x)
    if (!is.null(info) && info$is_mixed) {
      s <- tryCatch(
        {
          sqrt(get_variance_residual(x, verbose = FALSE))
        },
        error = function(e) {
          NULL
        }
      )
    }
  }

  if (.is_empty_object(s) && inherits(x, "brmsfit")) {
    s <- tryCatch(
      {
        dat <- as.data.frame(x)
        sigma_column <- grep("sigma", colnames(dat), fixed = TRUE)
        if (length(sigma_column)) {
          mean(dat[[sigma_column]][1])
        } else {
          NULL
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (.is_empty_object(s)) {
    return(NULL)
  }
  class(s) <- c("insight_aux", class(s))
  s
}







# Methods -----------------------------------------------------------------

.get_sigma_ci <- function(x, ci = 0.95, ...) {

  # TODO: What does it work for Bayesian models?

  if (is.null(ci) || is.na(ci)) {
    return(NULL)
  }

  alpha <- 1 - ci
  dev <- get_deviance(x)
  n <- n_obs(x)
  low <- dev / stats::qchisq(1 - alpha / 2, n)
  high <- dev / stats::qchisq(alpha / 2, n)
  list(CI_low = sqrt(low), CI_high = sqrt(high))
}



#' @export
as.numeric.insight_aux <- function(x, ...) {
  if (is.null(x) || is.na(x) || is.infinite(x)) {
    return(NULL)
  } else {
    mean(x, na.rm = TRUE)
  }
}
