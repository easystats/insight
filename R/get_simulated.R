#' @title Simulate values from fitted models
#'
#' @description Simulate responses from fitted statistical models.
#'
#' @name get_simulated
#'
#' @param x A model.
#' @param data An optional data frame in which to evaluate predictions before
#'   simulation. This can be a data grid created with [get_datagrid()].
#' @param iterations Number of response vectors to simulate.
#' @param seed An optional integer random seed.
#' @param re.form For `glmmTMB` models, random effects formula passed to
#'   simulation (`NULL`, `NA` or `~0`).
#' @param ... Additional arguments (currently unused for `lm`).
#'
#' @return A data frame with one column per simulation (`iter_1`, `iter_2`, ...).
#'   The attribute `seed` contains information about the RNG state used.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' # Simulations on the original data
#' get_simulated(m, iterations = 2, seed = 123)
#'
#' # Simulations on a data grid
#' dg <- get_datagrid(m, wt = c(2, 3), cyl = c(4, 6))
#' get_simulated(dg, m, iterations = 2, seed = 123)
#'
#' @export
get_simulated <- function(x, ...) {
  UseMethod("get_simulated")
}


#' @rdname get_simulated
#' @export
get_simulated.lm <- function(x, data = NULL, iterations = 1, seed = NULL, ...) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stats::runif(1) # initialize the RNG if necessary
  }

  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  if (is.null(data)) {
    data <- get_data(x, verbose = FALSE)
  }

  isGlm <- inherits(x, "glm")
  if (isGlm) {
    fam <- x$family$family
  } else {
    fam <- "gaussian"
  }
  if (is.null(data)) {
    fitted_values <- stats::fitted(x) # == napredict(*, x$fitted)
  } else {
    fitted_values <- stats::predict(x, newdata = data, ...)
  }
  isMlm <- identical(fam, "gaussian") && is.matrix(fitted_values)
  if (isMlm) {
    row_names <- dimnames(fitted_values)
  } else {
    row_names <- names(fitted_values)
  }

  if (isMlm) {
    format_error("`simulate()` is not yet implemented for multivariate models.")
  }

  n <- length(fitted_values)
  ntot <- n * iterations

  val <- switch(
    fam,
    "gaussian" = {
      vars <- stats::deviance(x) / stats::df.residual(x)
      if (isGlm) {
        if (!is.null(x$prior.weights) && length(x$prior.weights) == n) {
          vars <- vars / x$prior.weights
        }
      } else if (
        !(is.null(w <- x$weights) || (length(w) == 1L && w == 1)) && length(w) == n
      ) {
        vars <- vars / w
      }
      fitted_values + stats::rnorm(ntot, sd = sqrt(vars))
    },
    if (!is.null(x$family$simulate)) {
      x$family$simulate(x, iterations)
    } else {
      format_error(paste0("Family '", fam, "' not implemented."))
    }
  )

  if (!is.list(val)) {
    dim(val) <- c(n, iterations)
    val <- as.data.frame(val)
  } else {
    class(val) <- "data.frame"
  }

  names(val) <- paste0("iter_", seq_len(iterations))
  if (!is.null(row_names)) {
    row.names(val) <- row_names
  }

  attr(val, "seed") <- RNGstate
  val
}


#' @rdname get_simulated
#' @export
get_simulated.betareg <- function(x, data = NULL, iterations = 1, seed = NULL, ...) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stats::runif(1)
  }

  check_if_installed("betareg")

  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  if (is.null(data)) {
    data <- get_data(x, verbose = FALSE)
  }

  p <- if (is.null(data)) {
    stats::predict(x, type = "parameter")
  } else {
    stats::predict(x, newdata = data, type = "parameter")
  }
  p <- as.data.frame(p)

  n <- nrow(p)
  row_names <- rownames(p)
  dist <- if (is.null(x$dist)) "beta" else x$dist

  s <- switch(
    dist,
    "beta" = replicate(iterations, betareg::rbetar(n, mu = p$mu, phi = p$phi)),
    "xbeta" = replicate(
      iterations,
      betareg::rxbeta(n, mu = p$mu, phi = p$phi, nu = p$nu)
    ),
    "xbetax" = replicate(
      iterations,
      betareg::rxbetax(n, mu = p$mu, phi = p$phi, nu = p$nu)
    ),
    format_error(sprintf("Distribution '%s' is not supported for simulation.", dist))
  )

  s <- as.data.frame(s)
  names(s) <- paste0("iter_", seq_len(iterations))
  if (!is.null(row_names)) {
    row.names(s) <- row_names
  }

  attr(s, "seed") <- RNGstate
  s
}


#' @rdname get_simulated
#' @export
get_simulated.glmmTMB <- function(
  x,
  data = NULL,
  iterations = 1,
  seed = NULL,
  re.form = NULL,
  ...
) {
  check_if_installed("glmmTMB")
  no_sim <- get("noSim", envir = asNamespace("glmmTMB"))
  if (no_sim(x$modelInfo$family$family)) {
    format_error("Simulation code has not been implemented for this family.")
  }

  if (!is.null(data)) {
    format_error("`data` is currently not supported for `get_simulated.glmmTMB()`.")
  }

  pop_pred <- !is.null(re.form) && ((re.form == ~0) || identical(re.form, NA))
  if (!is.null(re.form)) {
    format_error("Only `re.form = NULL` is currently implemented.")
  }

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stats::runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  family <- x$modelInfo$family$family
  ret <- replicate(
    iterations,
    x$obj$simulate(par = x$fit$parfull)$yobs,
    simplify = FALSE
  )

  binomial_type <- get("binomialType", envir = asNamespace("glmmTMB"))
  if (binomial_type(family)) {
    size <- x$obj$env$data$size
    ret <- lapply(ret, function(y) cbind(y, size - y, deparse.level = 0))
    class(ret) <- "data.frame"
    rownames(ret) <- as.character(seq_len(nrow(ret[[1]])))
  } else {
    ret <- as.data.frame(ret)
  }

  names(ret) <- paste0("iter_", seq_len(iterations))
  attr(ret, "seed") <- RNGstate
  ret
}


#' @rdname get_simulated
#' @export
get_simulated.merMod <- function(
  x,
  data = NULL,
  iterations = 1,
  seed = NULL,
  use.u = FALSE,
  re.form = NA,
  newparams = NULL,
  family = NULL,
  cluster.rand = rnorm,
  allow.new.levels = FALSE,
  na.action = na.pass,
  ...
) {
  simulate_fun <- get(".simulateFun", envir = asNamespace("lme4"))

  val <- simulate_fun(
    object = x,
    nsim = iterations,
    seed = seed,
    use.u = use.u,
    re.form = re.form,
    newdata = data,
    newparams = newparams,
    family = family,
    cluster.rand = cluster.rand,
    allow.new.levels = allow.new.levels,
    na.action = na.action,
    ...
  )

  names(val) <- paste0("iter_", seq_len(iterations))
  val
}


#' @export
get_simulated.lmerMod <- get_simulated.merMod


#' @rdname get_simulated
#' @export
get_simulated.default <- function(x, ...) {
  format_error(sprintf(
    "`get_simulated()` has not yet been implemented for models of class `%s`.",
    class(x)[1]
  ))
}


#' @rdname get_simulated
#' @export
get_simulated.data.frame <- function(x, data = NULL, ...) {
  # This makes it pipe friendly; data %>% get_simulated(model)
  if (is.null(data)) {
    format_error("Please provide a model to base the simulations on.")
  } else {
    get_simulated(data, x, ...)
  }
}


#' @export
get_simulated.Gam <- get_simulated.default
#' @export
get_simulated.MixMod <- get_simulated.default
#' @export
get_simulated.afex_aov <- get_simulated.default
#' @export
get_simulated.bife <- get_simulated.default
#' @export
get_simulated.bracl <- get_simulated.default
#' @export
get_simulated.brmsfit <- get_simulated.default
#' @export
get_simulated.clm <- get_simulated.default
#' @export
get_simulated.coxme <- get_simulated.default
#' @export
get_simulated.coxph <- get_simulated.default
#' @export
get_simulated.crr <- get_simulated.default
#' @export
get_simulated.fa <- get_simulated.default
#' @export
get_simulated.faMain <- get_simulated.default
#' @export
get_simulated.fixest <- get_simulated.default
#' @export
get_simulated.gam <- get_simulated.default
#' @export
get_simulated.gamlss <- get_simulated.default
#' @export
get_simulated.gamm <- get_simulated.default
#' @export
get_simulated.glm <- get_simulated.default
#' @export
get_simulated.glmgee <- get_simulated.default
#' @export
get_simulated.hglm <- get_simulated.default
#' @export
get_simulated.htest <- get_simulated.default
#' @export
get_simulated.hurdle <- get_simulated.default
#' @export
get_simulated.list <- get_simulated.default
#' @export
get_simulated.lrm <- get_simulated.default
#' @export
get_simulated.multinom <- get_simulated.default
#' @export
get_simulated.phylolm <- get_simulated.default
#' @export
get_simulated.polr <- get_simulated.default
#' @export
get_simulated.prcomp <- get_simulated.default
#' @export
get_simulated.principal <- get_simulated.default
#' @export
get_simulated.rlm <- get_simulated.default
#' @export
get_simulated.rma <- get_simulated.default
#' @export
get_simulated.sdmTMB <- get_simulated.default
#' @export
get_simulated.stanreg <- get_simulated.default
#' @export
get_simulated.survreg <- get_simulated.default
#' @export
get_simulated.zeroinfl <- get_simulated.default
