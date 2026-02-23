.get_simulated_gaussian <- function(x, iterations, fitted_values, is_glm) {
  n <- length(fitted_values)
  ntot <- n * iterations

  vars <- stats::deviance(x) / stats::df.residual(x)
  w <- .safe(x$weights)
  if (is_glm) {
    if (!is.null(x$prior.weights) && length(x$prior.weights) == n) {
      vars <- vars / x$prior.weights
    }
  } else if (!(is.null(w) || (length(w) == 1L && w == 1)) && length(w) == n) {
    vars <- vars / w
  }
  as.vector(fitted_values) + stats::rnorm(ntot, sd = sqrt(vars))
}


.get_simulated_binomial <- function(x, iterations, fitted_values, data) {
  n <- length(fitted_values)
  ntot <- n * iterations
  wts <- x$prior.weights
  m <- x$model

  if (any(wts %% 1 != 0)) {
    format_error("Cannot simulate from non-integer prior weights.")
  }

  # when we have no prior weights, we must ensure it's of the same length
  # as the number of iterations
  if (all(wts == 1) && n != length(wts)) {
    wts <- rep(1, n)
  }
  # check length, cannot use prior weights when not the same
  if (!(all(wts == 1)) && n != length(wts)) {
    format_error("Cannot simulate with `prior.weights` for a data grid.")
  }

  if (!is.null(m)) {
    y <- stats::model.response(m)
    if (is.factor(y)) {
      sims <- factor(
        1 + stats::rbinom(ntot, size = 1, prob = fitted_values),
        labels = levels(y)
      )
      split(sims, rep(seq_len(iterations), each = n))
    } else if (is.matrix(y) && ncol(y) == 2) {
      resp <- find_response(x, combine = FALSE)
      sims <- vector("list", iterations)
      for (i in seq_len(iterations)) {
        sim_column <- stats::rbinom(n, size = wts, prob = fitted_values)
        sim_matrix <- cbind(sim_column, wts - sim_column)
        if (length(resp) == ncol(sim_matrix)) {
          colnames(sim_matrix) <- resp
        } else {
          colnames(sim_matrix) <- colnames(y)
        }
        sims[[i]] <- sim_matrix
      }
      sims
    } else {
      stats::rbinom(ntot, size = wts, prob = fitted_values) / wts
    }
  } else {
    stats::rbinom(ntot, size = wts, prob = fitted_values) / wts
  }
}


.get_simulated_poisson <- function(x, iterations, fitted_values) {
  n <- length(fitted_values)
  ntot <- n * iterations
  wts <- x$prior.weights

  if (any(wts != 1)) {
    format_alert("Ignoring prior weights.")
  }

  stats::rpois(ntot, fitted_values)
}


.get_simulated_gamma <- function(x, iterations, fitted_values) {
  check_if_installed("MASS")

  n <- length(fitted_values)
  ntot <- n * iterations
  wts <- x$prior.weights

  if (any(wts != 1)) {
    format_alert("Using weights as shape parameters.")
  }
  shape <- MASS::gamma.shape(x)$alpha * wts
  stats::rgamma(ntot, shape = shape, rate = shape / fitted_values)
}


.get_simulated_negbin <- function(x, iterations, fitted_values) {
  check_if_installed("MASS")

  n <- length(fitted_values)
  ntot <- n * iterations
  wts <- x$prior.weights

  if (any(wts != 1)) {
    format_alert("Ignoring prior weights.")
  }

  # try to extract theta
  if (inherits(x, "gam")) {
    f <- get_family(m)
    theta <- .safe(f$getTheta())
  } else {
    theta <- 1
  }

  MASS::rnegbin(ntot, mu = as.vector(fitted_values), theta = theta)
}
