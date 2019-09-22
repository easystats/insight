#' @title Get variance-covariance matrix from models
#'
#' @description Returns the variance-covariance, as retrieved by
#'   \code{stats::vcov()}, but works for more model objects that probably
#'   don't provide a \code{vcov()}-method.
#' @name get_varcov
#'
#' @param x A model.
#' @param component Should the complete variance-covariance matrix of the model
#'   be returned, or only for the count or zero-inlfated model? Applies to models
#'   with zero-inflated component. \code{component} may be one of
#'   \code{"conditional"}, \code{"zi"}, \code{"zero-inflated"} or \code{"all"}.
#'   May be abbreviated.
#' @param ... Currently not used.
#'
#' @note \code{get_varcov()} tries to return the nearest positive definite matrix
#'   in case of a negative variance-covariance matrix.
#'
#' @return The variance-covariance matrix, as \code{matrix}-object.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_varcov(m)
#' @importFrom stats vcov
#' @export
get_varcov <- function(x, ...) {
  UseMethod("get_varcov")
}


# Default models ----------------------------------------------------


#' @export
get_varcov.default <- function(x, ...) {
  vc <- suppressWarnings(stats::vcov(x))

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}





# Zero-Inflated models ----------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.hurdle <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(object = x, model = "count"),
    "zi" = ,
    "zero_inflated" = stats::vcov(object = x, model = "zero"),
    stats::vcov(object = x)
  )

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}

#' @export
get_varcov.zeroinfl <- get_varcov.hurdle

#' @export
get_varcov.zerocount <- get_varcov.hurdle





# Zero-Inflated mixed models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.MixMod <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(x, parm = "fixed-effects"),
    "zi" = ,
    "zero_inflated" = stats::vcov(x, parm = "zero_part"),
    stats::vcov(x)
  )

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @rdname get_varcov
#' @export
get_varcov.glmmTMB <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)

  vc <- switch(
    component,
    "conditional" = stats::vcov(x)[["cond"]],
    "zi" = ,
    "zero_inflated" = stats::vcov(x)[["zi"]],
    stats::vcov(x, full = TRUE)
  )

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}






# Bayesian models ------------------------------------------------


#' @rdname get_varcov
#' @export
get_varcov.brmsfit <- function(x, component = c("conditional", "zero_inflated", "zi", "all"), ...) {
  component <- match.arg(component)
  params <- find_parameters(x, effects = "fixed", component = component, flatten = TRUE)
  params <- gsub("^b_", "", params)

  vc <- stats::vcov(x)[params, params]

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}






# Other models with special handling -----------------------------------------


#' @export
get_varcov.rq <- function(x, ...) {
  s <- summary(x, covariance = TRUE)
  vc <- as.matrix(s$cov)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.crq <- get_varcov.rq

#' @export
get_varcov.nlrq <- get_varcov.rq


#' @export
get_varcov.mixed <- function(x, ...) {
  vc <- as.matrix(stats::vcov(x$full_model))

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.gamm <- function(x, ...) {
  vc <- stats::vcov(x$gam)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.list <- function(x, ...) {
  if ("gam" %in% names(x)) {
    vc <- stats::vcov(x$gam)

    if (.is_negativ_matrix(vc)) {
      vc <- .fix_negative_matrix(vc)
    }

    as.matrix(vc)
  }
}


#' @export
get_varcov.BBmm <- function(x, ...) {
  vc <- x$fixed.vcov

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.BBreg <- function(x, ...) {
  vc <- x$vcov

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.feis <- function(x, ...) {
  vc <- x$vcov

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.glimML <- function(x, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }
  vc <- aod::vcov(x)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.vglm <- function(x, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' required for this function to work. Please install it.")
  }
  vc <- VGAM::vcov(x)

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.vgam <- get_varcov.vglm


#' @export
get_varcov.geeglm <- function(x, ...) {
  vc <- summary(x)$cov.unscaled

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.tobit <- function(x, ...) {
  coef_names <- find_parameters(x, flatten = TRUE)
  vc <- stats::vcov(x)[coef_names, coef_names]

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.lmRob <- function(x, ...) {
  vc <- x$cov

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.glmRob <- get_varcov.lmRob



#' @export
get_varcov.gee <- function(x, ...) {
  vc <- x$naive.variance

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }

  as.matrix(vc)
}


#' @export
get_varcov.LORgee <- get_varcov.gee







# helper-functions -----------------------------------------------------


.is_negativ_matrix <- function(x) {
  if (is.matrix(x) && (nrow(x) == ncol(x))) {
    eigenvalues <- eigen(x, only.values = TRUE)$values
    eigenvalues[abs(eigenvalues) < 1e-07] <- 0
    rv <- any(eigenvalues <= 0)
  } else {
    rv <- FALSE
  }

  rv
}


.fix_negative_matrix <- function(m) {
  if (requireNamespace("Matrix", quietly = TRUE)) {
    as.matrix(Matrix::nearPD(m)$mat)
  } else {
    m
  }
}
