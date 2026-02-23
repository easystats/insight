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
#' @param include_data Logical, if `TRUE`, `data` is returned alongside with
#' the simulated draws. If `data = NULL`, the data used to fit the model is
#' included.
#' @param seed An optional integer random seed.
#' @param centrality Function, indicating how to summarize aggregated simulated
#' values when `data` is a data grid. Only applies to models from package
#' `glmmTMB`, because a special handling for the `data` argument is required
#' here. `simulate.glmmTMB()` always returns simulated draws for the full
#' data that was used to fit the model. If a data grid is passed via `data`,
#' `get_simulated.glmmTMB()` internally, first, filters the results using
#' `datawizard::data_match()` with the model data and `data`. This may return
#' more than one row of simulated draws per group defined in `data`, thus, in
#' a second step, the filtered data are aggregated by the groups defined in
#' `data`. Resulting estimates of simulated values are summarized using
#' `centrylity`, which, by default is the mode for categorical, ordinal or
#' count response variables, or the mean otherwise.
#' @param re.form For `glmmTMB` and `merMod` models, random effects formula
#' passed to simulation (`NULL`, `NA` or `~0`).
#' @param use.u For `merMod` models, logical indicating whether to condition
#' on the current conditional modes of the random effects when simulating.
#' @param newparams For `merMod` models, optional list or vector of alternative
#' parameter values (e.g., fixed- and random-effect parameters) to be used for
#' simulation instead of those from the fitted model.
#' @param family For `merMod` models, optional `family` object or function to
#' override the model's family when simulating responses.
#' @param cluster.rand For `merMod` models, function used to generate random
#' draws for the random effects (e.g., `rnorm`).
#' @param allow.new.levels For `merMod` models, logical indicating whether new
#' levels of grouping variables are allowed in `data` when simulating.
#' @param na.action For `merMod` models, function specifying how to handle
#' missing values in `data` before simulation (e.g., `"na.pass"`).
#' @param ... Additional arguments passed to the underlying prediction or
#' simulation methods.
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
get_simulated.lm <- function(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  ...
) {
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
    focal <- find_variables(x, flatten = TRUE)
  } else {
    focal <- colnames(data)
  }

  is_glm <- inherits(x, "glm")
  if (is_glm) {
    model_family <- x$family$family
  } else {
    model_family <- "gaussian"
  }

  ## TODO: check if we can use this in general, or need other types for other models

  # type = "response" required for binomial, poisson etc. glm later
  # confirmed that response can be used for: binomial, poisson, Gamma
  fitted_values <- stats::predict(x, newdata = data, type = "response", ...)

  is_multivariate <- identical(model_family, "gaussian") && is.matrix(fitted_values)
  if (is_multivariate) {
    row_names <- dimnames(fitted_values)
  } else {
    row_names <- names(fitted_values)
  }

  if (is_multivariate) {
    format_error("`simulate()` is not yet implemented for multivariate models.")
  }

  val <- switch(
    model_family,
    gaussian = .get_simulated_gaussian(x, iterations, fitted_values, is_glm),
    binomial = .get_simulated_binomial(x, iterations, fitted_values, data),
    poisson = .get_simulated_poisson(x, iterations, fitted_values),
    Gamma = .get_simulated_gamma(x, iterations, fitted_values),
    if (!is.null(x$family$simulate)) {
      x$family$simulate(x, iterations)
    } else {
      format_error(paste0("Family '", model_family, "' not implemented."))
    }
  )

  if (!is.list(val)) {
    dim(val) <- c(length(fitted_values), iterations)
  }

  val <- as.data.frame(val)

  # for glm with "cbind()" response, we need special handling here
  resp <- find_response(x, combine = FALSE)
  if (length(resp) == 2) {
    colnames(val) <- paste(
      rep(paste0("iter_", seq_len(iterations)), each = 2),
      resp,
      sep = "_"
    )
  } else {
    colnames(val) <- paste0("iter_", seq_len(iterations))
  }

  if (include_data) {
    # keep only focal terms
    data <- data[intersect(focal, colnames(data))]
    val <- cbind(data, val)
  }

  if (!is.null(row_names)) {
    row.names(val) <- row_names
  }

  attr(val, "seed") <- RNGstate
  val
}


#' @export
get_simulated.glm <- get_simulated.lm


#' @export
get_simulated.betareg <- function(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  ...
) {
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

  predictions <- suppressWarnings(stats::predict(x, newdata = data, type = "parameter"))
  predictions <- as.data.frame(predictions)

  n <- nrow(predictions)
  row_names <- rownames(predictions)

  if (is.null(x$dist)) {
    dist <- "beta"
  } else {
    dist <- x$dist
  }

  sims <- switch(
    dist,
    beta = replicate(
      iterations,
      betareg::rbetar(n, mu = predictions$mu, phi = predictions$phi)
    ),
    xbeta = replicate(
      iterations,
      betareg::rxbeta(n, mu = predictions$mu, phi = predictions$phi, nu = predictions$nu)
    ),
    xbetax = replicate(
      iterations,
      betareg::rxbetax(n, mu = predictions$mu, phi = predictions$phi, nu = predictions$nu)
    ),
    format_error(sprintf("Distribution '%s' is not supported for simulation.", dist))
  )

  sims <- as.data.frame(sims)
  names(sims) <- paste0("iter_", seq_len(iterations))
  if (!is.null(row_names)) {
    row.names(sims) <- row_names
  }

  attr(sims, "seed") <- RNGstate
  sims
}


#' @rdname get_simulated
#' @export
get_simulated.glmmTMB <- function(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  centrality = NULL,
  re.form = NULL,
  ...
) {
  check_if_installed("glmmTMB")
  no_sim <- get("noSim", envir = asNamespace("glmmTMB"))
  if (no_sim(x$modelInfo$family$family)) {
    format_error("Simulation code has not been implemented for this family.")
  }

  # extract data
  model_data <- get_data(x, verbose = FALSE)

  # check if all variables in data grid are also present in data
  if (!is.null(data) && !all(colnames(data) %in% colnames(model_data))) {
    format_error("Not all variables in `data` were found in the model.")
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

  # do we need to filter?
  if (!is.null(data) && nrow(data) != nrow(model_data)) {
    check_if_installed("datawizard")
    # find focal terms from data grid
    focal <- attributes(data)$by
    # remove random effects from data grid
    re <- find_random(x, split_nested = TRUE, flatten = TRUE)
    data[re] <- NULL
    # if we don't have a data grid, use column names instead
    if (is.null(focal)) {
      focal <- colnames(data)
    }
    # add simulations to model data
    model_data <- cbind(model_data, ret)
    # next, filter data
    filtered_data <- datawizard::data_match(model_data, data[focal])
    # sanity check - did filtering work?
    if (nrow(filtered_data) == 0) {
      format_error(
        "Simulating predictions only works when values in the data grid are also present in the data that was used to fit the model."
      )
    }
    # define function how to summarize aggregated simulations. by default,
    # we use the mode for categorical, ordinal and count data, and the mea
    # for everything else
    if (is.null(centrality)) {
      # for categorical outcomes, we aggregrate using the mode, not the mean
      model_info <- model_info(x)
      use_mode <- any(unlist(
        model_info[c(
          "is_binomial",
          "is_ordinal",
          "is_multinomial",
          "is_poisson",
          "is_count",
          "is_categorical"
        )],
        use.names = FALSE
      ))

      if (use_mode) {
        centrality <- .mode_value
      } else {
        centrality <- mean
      }
    }
    # aggreate and "average" simulations
    ret <- stats::aggregate(
      filtered_data[colnames(ret)],
      by = filtered_data[focal],
      centrality,
      na.rm = TRUE
    )
    # by default, we have the data bound to the iterations, so we might
    # need to remove them here
    if (!include_data) {
      keep <- grep("iter_\\d+", colnames(ret), value = TRUE)
      ret <- ret[keep]
    }
  } else if (include_data) {
    # find predictors
    focal <- intersect(find_variables(x, flatten = TRUE), colnames(model_data))
    ret <- cbind(model_data[focal], ret)
  }

  attr(ret, "seed") <- RNGstate
  ret
}


#' @rdname get_simulated
#' @export
get_simulated.merMod <- function(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
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
  check_if_installed("lme4")
  simulate_fun <- get(".simulateFun", envir = asNamespace("lme4"))

  # we cannot pass those arguments directly to "simulate_fun", because
  # that function internally checks that at least of those argument is
  # missing - this would never be TRUE if we include those arguments in
  # the function call. thus, we handle the defaults separately
  add_reform <- missing(use.u) && !missing(re.form)
  add_use_u <- !missing(use.u) && missing(re.form)

  args <- list(
    object = x,
    nsim = iterations,
    seed = seed,
    newdata = data,
    newparams = newparams,
    family = family,
    cluster.rand = cluster.rand,
    allow.new.levels = allow.new.levels,
    na.action = na.action
  )

  if (add_reform) {
    args$re.form <- re.form
  } else if (add_use_u) {
    args$use.u <- use.u
  }

  val <- do.call(simulate_fun, c(args, list(...)))

  names(val) <- paste0("iter_", seq_len(iterations))

  if (include_data) {
    if (is.null(data)) {
      data <- get_data(x, verbose = FALSE)
    } else {
      focal <- colnames(data)
    }
    # keep only focal terms
    data <- data[intersect(focal, colnames(data))]
    val <- cbind(data, val)
  }

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
get_simulated.data.frame <- function(x, data = NULL, include_data = FALSE, ...) {
  # This makes it pipe friendly; data %>% get_simulated(model)
  if (is.null(data)) {
    format_error("Please provide a model to base the simulations on.")
  } else {
    get_simulated(data, x, include_data = include_data, ...)
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
