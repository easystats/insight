#' @title Get model parameters from Bayesian models
#' @name get_parameters.BGGM
#'
#' @description Returns the coefficients (or posterior samples for Bayesian
#'    models) from a model.
#'
#' @param iterations Number of posterior draws.
#' @param progress Display progress.
#' @param summary Logical, indicates whether the full posterior samples
#'   (`summary = FALSE`)) or the summarized centrality indices of
#'   the posterior samples (`summary = TRUE`)) should be returned as
#'   estimates.
#' @param centrality Only for models with posterior samples, and when
#'   `summary = TRUE`. In this case, `centrality = "mean"` would
#'   calculate means of posterior samples for each parameter, while
#'   `centrality = "median"` would use the more robust median value as
#'   measure of central tendency.
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters.BGGM
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return The posterior samples from the requested parameters as data frame.
#'   If `summary = TRUE`, returns a data frame with two columns: the
#'   parameter names and the related point estimates (based on `centrality`).
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' `effects` and `component` can be used.
#'
#' @inheritSection find_predictors Model components
#'
#' @section BFBayesFactor Models:
#' Note that for `BFBayesFactor` models (from the **BayesFactor** package),
#' posteriors are only extracted from the first numerator model (i.e.,
#' `model[1]`). If you want to apply some function `foo()` to another
#' model stored in the `BFBayesFactor` object, index it directly, e.g.
#' `foo(model[2])`, `foo(1/model[5])`, etc.
#' See also [bayestestR::weighted_posteriors()].
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.BGGM <- function(x,
                                component = "correlation",
                                summary = FALSE,
                                centrality = "mean",
                                ...) {
  # check_if_installed("BGGM")
  #
  # out <- as.data.frame(BGGM::posterior_samples(x))
  out <- as.data.frame(.bggm_posterior_samples(x))
  intercepts <- endsWith(colnames(out), "_(Intercept)")
  correlations <- grepl("(.*)--(.*)", colnames(out))
  conditional <- !intercepts & !correlations

  component <- validate_argument(
    component,
    c("correlation", "conditional", "intercept", "all")
  )
  out <- switch(component,
    conditional = out[, conditional, drop = FALSE],
    correlation = out[, correlations, drop = FALSE],
    intercept = out[, intercepts, drop = FALSE],
    out
  )
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.MCMCglmm <- function(x,
                                    effects = "fixed",
                                    summary = FALSE,
                                    centrality = "mean",
                                    ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))

  nF <- x$Fixed$nfl
  fixed <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  random <- as.data.frame(x$VCV[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE])
  all_params <- cbind(fixed, random)

  out <- if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all_params
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters.BGGM
#' @export
get_parameters.BFBayesFactor <- function(x,
                                         effects = "all",
                                         component = "all",
                                         iterations = 4000,
                                         progress = FALSE,
                                         verbose = TRUE,
                                         summary = FALSE,
                                         centrality = "mean",
                                         ...) {
  check_if_installed("BayesFactor")

  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(component, c("all", "extra"))
  bf_type <- .classify_BFBayesFactor(x)

  # check if valid model was indexed...

  if ((length(x@numerator) > 1 ||
    !xor(
      x@denominator@shortName == "Intercept only",
      grepl("^(Null|Indep)", x@denominator@shortName)
    )) && verbose) {
    format_alert(
      "Multiple `BFBayesFactor` models detected - posteriors are extracted from the first numerator model.",
      'See help("get_parameters", package = "insight").'
    )
  }


  params <- find_parameters(x, effects = effects, component = component, flatten = TRUE, ...)

  if (bf_type %in% c("correlation", "ttest1", "ttest2", "meta", "linear")) {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1, ...)
      ))

    out <- switch(bf_type,
      correlation = data.frame(rho = as.numeric(posteriors$rho)),
      ttest1 = data.frame(Difference = as.numeric(posteriors[, 1]) - x@numerator[[1]]@prior$mu),
      ttest2 = data.frame(Difference = as.numeric(posteriors[, 2]) - x@numerator[[1]]@prior$mu),
      meta = data.frame(Effect = as.numeric(posteriors$delta)),
      linear = .get_bf_posteriors(posteriors, params),
      NULL
    )
  } else if (bf_type == "proptest") {
    posteriors <- as.data.frame(as.matrix(suppressMessages(
      BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1)
    )[, "p"]))
    colnames(posteriors) <- "p"
    out <- posteriors
  } else if (bf_type == "xtable") {
    model_data <- get_data(x, verbose = verbose)
    N <- sum(model_data)
    cells <- prod(dim(model_data))
    posts <- as.data.frame(as.matrix(suppressMessages(
      BayesFactor::posterior(x, iterations = iterations, progress = progress)
    )))
    posts <- posts[, seq_len(cells)]
    if (all(posts[1, ] <= 1)) {
      posts <- posts * N
    }
    colnames(posts) <- gsub("(pi|lambda)", "cell", colnames(posts))
    out <- posts
  } else {
    out <- NULL
  }

  if (isTRUE(summary) && !is.null(out)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }

  out
}


#' @export
get_parameters.stanmvreg <- function(x,
                                     effects = "fixed",
                                     parameters = NULL,
                                     summary = FALSE,
                                     centrality = "mean",
                                     ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))
  elements <- .get_elements(effects, "all")
  parms <- find_parameters(x, flatten = FALSE, parameters = parameters)

  for (i in names(parms)) {
    parms[[i]]$conditional <- sprintf("%s|%s", i, parms[[i]]$conditional)
    find_bracket <- regexpr(pattern = "[", parms[[i]]$random, fixed = TRUE)
    parms[[i]]$random <- paste0(
      substr(parms[[i]]$random, start = 1, stop = find_bracket),
      i, "|",
      substr(parms[[i]]$random, start = find_bracket + 1, stop = 1000000L)
    )
    parms[[i]]$sigma <- NULL
  }

  out <- as.data.frame(x)[unlist(lapply(compact_list(parms), function(i) i[elements]), use.names = FALSE)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters.BGGM
#' @export
get_parameters.brmsfit <- function(x,
                                   effects = "fixed",
                                   component = "all",
                                   parameters = NULL,
                                   summary = FALSE,
                                   centrality = "mean",
                                   ...) {
  effects <- validate_argument(effects, c("all", "fixed", "random"))
  component <- validate_argument(
    component,
    c("all", .all_elements(), "location", "distributional")
  )

  if (is_multivariate(x)) {
    parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
    elements <- .get_elements(effects, component)
    out <- as.data.frame(x)[unlist(lapply(parms, function(i) i[elements]), use.names = FALSE)]
  } else {
    out <- as.data.frame(x)[.get_parms_data(x, effects, component, parameters)]
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.stanreg <- function(x,
                                   effects = "fixed",
                                   component = "location",
                                   parameters = NULL,
                                   summary = FALSE,
                                   centrality = "mean",
                                   ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))
  component <- validate_argument(
    component,
    c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary")
  )
  out <- as.data.frame(x)[.get_parms_data(x, effects, component, parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.stanfit <- get_parameters.stanreg


#' @export
get_parameters.bcplm <- function(x,
                                 parameters = NULL,
                                 summary = FALSE,
                                 centrality = "mean",
                                 ...) {
  out <- as.data.frame(do.call(rbind, x$sims.list))
  if (!is.null(parameters)) {
    out <- out[grepl(pattern = parameters, x = colnames(out), perl = TRUE)]
  }
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.bayesx <- function(x,
                                  component = "conditional",
                                  summary = FALSE,
                                  centrality = "mean",
                                  ...) {
  component <- validate_argument(component, c("conditional", "smooth_terms", "all"))

  smooth_dat <- data.frame(
    Parameter = find_parameters(x, component = "smooth_terms", flatten = TRUE),
    Estimate = x$smooth.hyp[, 1],
    Component = "smooth_terms",
    stringsAsFactors = FALSE
  )

  fixed_dat <- data.frame(
    Parameter = find_parameters(x, component = "conditional", flatten = TRUE),
    Estimate = x$fixed.effects[, 1],
    Component = "conditional",
    stringsAsFactors = FALSE
  )

  params <- switch(component,
    all = rbind(fixed_dat, smooth_dat),
    conditional = fixed_dat,
    smooth_terms = smooth_dat
  )

  out <- text_remove_backticks(params)

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.mcmc.list <- function(x,
                                     parameters = NULL,
                                     summary = FALSE,
                                     centrality = "mean",
                                     ...) {
  out <- as.data.frame(do.call(rbind, x))
  if (!is.null(parameters)) {
    out <- out[grepl(pattern = parameters, x = colnames(out), perl = TRUE)]
  }
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.bamlss <- function(x,
                                  component = "all",
                                  parameters = NULL,
                                  summary = FALSE,
                                  centrality = "mean",
                                  ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "smooth_terms", "location", "distributional", "auxiliary")
  )
  elements <- .get_elements(effects = "all", component)

  parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
  out <- as.data.frame(unclass(x$samples))[unlist(parms[elements], use.names = FALSE)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.mcmc <- function(x,
                                parameters = NULL,
                                summary = FALSE,
                                centrality = "mean",
                                ...) {
  out <- as.data.frame(x)[.get_parms_data(x, "all", "all", parameters)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.bayesQR <- function(x,
                                   parameters = NULL,
                                   summary = FALSE,
                                   centrality = "mean",
                                   ...) {
  out <- as.data.frame(x[[1]]$betadraw)
  names(out) <- x[[1]]$names
  out <- out[.get_parms_data(x, "all", "all", parameters)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.blrm <- function(x,
                                parameters = NULL,
                                summary = FALSE,
                                centrality = "mean",
                                ...) {
  out <- as.data.frame(x$draws)
  out <- out[.get_parms_data(x, "all", "all", parameters)]
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.sim.merMod <- function(x,
                                      effects = "fixed",
                                      parameters = NULL,
                                      summary = FALSE,
                                      centrality = "mean",
                                      ...) {
  effects <- validate_argument(effects, c("fixed", "random", "all"))
  fe <- re <- NULL
  if (effects %in% c("fixed", "all")) fe <- .get_armsim_fixef_parms(x)
  if (effects %in% c("random", "all")) re <- .get_armsim_ranef_parms(x)

  dat <- do.call(cbind, compact_list(list(fe, re)))

  out <- as.data.frame(dat)[.get_parms_data(x, effects, "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @export
get_parameters.sim <- function(x,
                               parameters = NULL,
                               summary = FALSE,
                               centrality = "mean",
                               ...) {
  dat <- .get_armsim_fixef_parms(x)
  out <- as.data.frame(dat)[.get_parms_data(x, "all", "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


# helper -----------------------


.summary_of_posteriors <- function(out, centrality = "mean", ...) {
  s <- switch(centrality,
    mean = vapply(out, mean, numeric(1), na.rm = TRUE),
    median = vapply(out, stats::median, numeric(1), na.rm = TRUE),
    vapply(out, mean, numeric(1), na.rm = TRUE)
  )
  data.frame(
    Parameter = names(s),
    Estimate = unname(s),
    stringsAsFactors = FALSE
  )
}


.get_bf_posteriors <- function(posteriors, params) {
  cn <- intersect(colnames(posteriors), params)
  posteriors[, cn, drop = FALSE]
}


.get_parms_data <- function(x, effects, component, parameters = NULL) {
  elements <- .get_elements(effects, component)
  unlist(find_parameters(x, effects = "all", component = "all", flatten = FALSE, parameters = parameters)[elements])
}


# use temporarily code from BGGM package, as long as that package is archived on CRAN

.bggm_posterior_samples <- function(object, ...) {
  if (methods::is(object, "estimate") || methods::is(object, "explore")) {
    if (!methods::is(object, "default")) {
      format_error("Object must be from `estimate` or `explore`.")
    }
    p <- object$p
    pcors_total <- p * (p - 1) * 0.5
    I_p <- diag(p)
    iter <- object$iter
    pcor_samples <- matrix(object$post_samp$pcors[, , 51:(iter + 50)][upper.tri(I_p)],
      nrow = iter,
      ncol = pcors_total,
      byrow = TRUE
    )
    cn <- colnames(object$Y)
    if (is.null(cn)) {
      col_names <- sapply(1:p, function(x) paste(1:p, x, sep = "--"))[upper.tri(I_p)]
    } else {
      col_names <- sapply(cn, function(x) paste(cn, x, sep = "--"))[upper.tri(I_p)]
    }
    colnames(pcor_samples) <- col_names
    posterior_samples <- pcor_samples
    if (!is.null(object$formula)) {
      if (ncol(object$X) == 1) {
        beta_terms <- "(Intercept)"
      } else {
        beta_terms <- colnames(object$X)
      }
      n_beta_terms <- length(beta_terms)
      beta_samples <- object$post_samp$beta
      if (is.null(cn)) {
        col_names <- 1:p
      } else {
        col_names <- cn
      }
      beta_start <- matrix(beta_samples[1:n_beta_terms, 1, 51:(iter + 50)],
        nrow = iter,
        n_beta_terms,
        byrow = TRUE
      )
      colnames(beta_start) <- paste0(
        col_names[1], "_",
        beta_terms
      )
      for (i in 2:p) {
        beta_i <- matrix(beta_samples[1:n_beta_terms, i, 51:(iter + 50)],
          nrow = iter,
          n_beta_terms,
          byrow = TRUE
        )
        colnames(beta_i) <- paste0(col_names[i], "_", beta_terms)
        beta_start <- cbind(beta_start, beta_i)
      }
      posterior_samples <- cbind(posterior_samples, beta_start)
    }
  } else if (methods::is(object, "var_estimate")) {
    if (!methods::is(object, "default")) {
      format_error("Object must be from 'var_estimate'.")
    }
    p <- object$p
    pcors_total <- p * (p - 1) * 0.5
    I_p <- diag(p)
    iter <- object$iter
    pcor_samples <- matrix(object$fit$pcors[, , 51:(iter + 50)][upper.tri(I_p)],
      nrow = iter,
      ncol = pcors_total,
      byrow = TRUE
    )
    cn <- colnames(object$Y)
    if (is.null(cn)) {
      col_names <- sapply(1:p, function(x) paste(1:p, x, sep = "--"))[upper.tri(I_p)]
    } else {
      col_names <- sapply(cn, function(x) paste(cn, x, sep = "--"))[upper.tri(I_p)]
    }
    colnames(pcor_samples) <- col_names
    posterior_samples <- pcor_samples
    n_beta_terms <- nrow(object$beta_mu)
    beta_samples <- object$fit$beta
    col_names <- colnames(object$Y)
    beta_terms <- colnames(object$X)
    beta_start <- matrix(beta_samples[1:n_beta_terms, 1, 51:(iter + 50)],
      nrow = iter,
      n_beta_terms,
      byrow = TRUE
    )
    colnames(beta_start) <- paste0(col_names[1], "_", beta_terms)
    for (i in 2:p) {
      beta_i <- matrix(beta_samples[1:n_beta_terms, i, 51:(iter + 50)],
        nrow = iter,
        n_beta_terms,
        byrow = TRUE
      )
      colnames(beta_i) <- paste0(col_names[i], "_", beta_terms)
      beta_start <- cbind(beta_start, beta_i)
    }
    posterior_samples <- cbind(posterior_samples, beta_start)
  } else {
    format_error("Object class not currently supported.")
  }

  posterior_samples
}
