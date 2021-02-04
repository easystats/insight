#' @title Get model parameters from Bayesian models
#' @name get_parameters.BGGM
#'
#' @description Returns the coefficients (or posterior samples for Bayesian
#'    models) from a model.
#'
#' @param iterations Number of posterior draws.
#' @param progress Display progress.
#' @param summary Logical, indicates whether the full posterior samples
#'   (\code{summary = FALSE})) or the summarized centrality indices of
#'   the posterior samples (\code{summary = TRUE})) should be returned as
#'   estimates.
#' @param centrality Only for models with posterior samples, and when
#'   \code{summary = TRUE}. In this case, \code{centrality = "mean"} would
#'   calculate means of posterior samples for each parameter, while
#'   \code{centrality = "median"} would use the more robust median value as
#'   measure of central tendency.
#' @param verbose Toggle messages and warnings.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters.BGGM
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return The posterior samples from the requested parameters as data frame.
#'   If \code{summary = TRUE}, returns a data frame with two columns: the
#'   parameter names and the related point estimates (based on \code{centrality}).
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' \code{effects} and \code{component} can be used.
#'
#' @section BFBayesFactor Models:
#' Note that for \code{BFBayesFactor} models (from the \pkg{BayesFactor}
#' package), posteriors are only extracted from the first numerator model (i.e.,
#' \code{model[1]}). If you want to apply some function \code{foo()} to another
#' model stored in the \code{BFBayesFactor} object, index it directly, e.g.
#' \code{foo(model[2])}, \code{foo(1/model[5])}, etc.
#' See also \code{\link[bayestestR]{weighted_posteriors}}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.BGGM <- function(x,
                                component = c("correlation", "conditional", "intercept", "all"),
                                summary = FALSE, centrality = "mean",
                                ...) {
  if (!requireNamespace("BGGM", quietly = TRUE)) {
    stop("Package 'BGGM' required for this function to work. Please install it.")
  }

  out <- as.data.frame(BGGM::posterior_samples(x))
  intercepts <- grepl("_\\(Intercept\\)$", colnames(out))
  correlations <- grepl("(.*)--(.*)", colnames(out))
  conditional <- !intercepts & !correlations

  component <- match.arg(component)
  out <- switch(component,
    "conditional" = out[, conditional, drop = FALSE],
    "correlation" = out[, correlations, drop = FALSE],
    "intercept" = out[, intercepts, drop = FALSE],
    out
  )
  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters.BGGM
#' @export
get_parameters.MCMCglmm <- function(x,
                                    effects = c("fixed", "random", "all"),
                                    summary = FALSE,
                                    centrality = "mean",
                                    ...) {
  effects <- match.arg(effects)

  nF <- x$Fixed$nfl
  fixed <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  random <- as.data.frame(x$VCV[, find_random(x, split_nested = TRUE, flatten = TRUE), drop = FALSE])
  all <- cbind(fixed, random)

  out <- if (effects == "fixed") {
    fixed
  } else if (effects == "random") {
    random
  } else {
    all
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}


#' @rdname get_parameters.BGGM
#' @export
get_parameters.BFBayesFactor <- function(x,
                                         effects = c("all", "fixed", "random"),
                                         component = c("all", "extra"),
                                         iterations = 4000,
                                         progress = FALSE,
                                         verbose = TRUE,
                                         summary = FALSE,
                                         centrality = "mean",
                                         ...) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function requires package `BayesFactor` to work. Please install it.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)
  bf_type <- .classify_BFBayesFactor(x)

  # check if valid model was indexed...

  if (length(x@numerator) > 1 ||
    !xor(
      x@denominator@shortName == "Intercept only",
      grepl("^(Null|Indep)", x@denominator@shortName)
    )) {
    if (verbose) {
      message(
        "Multiple `BFBayesFactor` models detected - posteriors are extracted from the first numerator model.\n",
        'See help("get_parameters", package = "insight").'
      )
    }
  }


  params <- find_parameters(x, effects = effects, component = component, flatten = TRUE, ...)

  if (bf_type %in% c("correlation", "ttest1", "ttest2", "meta", "linear")) {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1, ...)
      ))

    out <- switch(bf_type,
      "correlation" = data.frame("rho" = as.numeric(posteriors$rho)),
      "ttest1" = data.frame("Difference" = x@numerator[[1]]@prior$mu - as.numeric(posteriors[, 1])),
      "ttest2" = data.frame("Difference" = x@numerator[[1]]@prior$mu - as.numeric(posteriors[, 2])),
      "meta" = data.frame("Effect" = as.numeric(posteriors$delta)),
      "linear" = .get_bf_posteriors(posteriors, params),
      NULL
    )
  } else if (bf_type == "proptest") {
    posteriors <- as.data.frame(as.matrix(suppressMessages(
      BayesFactor::posterior(x, iterations = iterations, progress = progress, index = 1)
    )[, "p"]))
    colnames(posteriors) <- "p"
    out <- posteriors
  } else if (bf_type == "xtable") {
    data <- get_data(x)
    N <- sum(data)
    cells <- prod(dim(data))
    posts <- as.data.frame(as.matrix(suppressMessages(
      BayesFactor::posterior(x, iterations = iterations, progress = progress)
    )))
    posts <- posts[, seq_len(cells)]
    if (sum(posts[1, ]) == 1) {
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



#' @rdname get_parameters.BGGM
#' @export
get_parameters.stanmvreg <- function(x,
                                     effects = c("fixed", "random", "all"),
                                     parameters = NULL,
                                     summary = FALSE,
                                     centrality = "mean",
                                     ...) {
  effects <- match.arg(effects)
  elements <- .get_elements(effects, "all")
  parms <- find_parameters(x, flatten = FALSE, parameters = parameters)

  for (i in names(parms)) {
    parms[[i]]$conditional <- sprintf("%s|%s", i, parms[[i]]$conditional)
    find_bracket <- regexpr(pattern = "\\[", parms[[i]]$random)
    parms[[i]]$random <- paste0(
      substr(parms[[i]]$random, start = 1, stop = find_bracket),
      i, "|",
      substr(parms[[i]]$random, start = find_bracket + 1, stop = 1000000L)
    )
    parms[[i]]$sigma <- NULL
  }

  out <- as.data.frame(x)[unlist(lapply(.compact_list(parms), function(i) i[elements]))]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @rdname get_parameters.BGGM
#' @export
get_parameters.brmsfit <- function(x,
                                   effects = c("fixed", "random", "all"),
                                   component = c("all", "conditional", "location", "distributional", "auxiliary", "zi", "zero_inflated", "dispersion", "simplex", "sigma", "smooth_terms"),
                                   parameters = NULL,
                                   summary = FALSE,
                                   centrality = "mean",
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (is_multivariate(x)) {
    parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
    elements <- .get_elements(effects, component)
    ## TODO remove "optional = FALSE" in a future update
    out <- as.data.frame(x, optional = FALSE)[unlist(lapply(parms, function(i) i[elements]))]
  } else {
    ## TODO remove "optional = FALSE" in a future update
    out <- as.data.frame(x, optional = FALSE)[.get_parms_data(x, effects, component, parameters)]
  }

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @rdname get_parameters.BGGM
#' @export
get_parameters.stanreg <- function(x,
                                   effects = c("fixed", "random", "all"),
                                   component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                                   parameters = NULL,
                                   summary = FALSE,
                                   centrality = "mean",
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
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



#' @rdname get_parameters.BGGM
#' @export
get_parameters.bayesx <- function(x,
                                  component = c("conditional", "smooth_terms", "all"),
                                  summary = FALSE,
                                  centrality = "mean",
                                  ...) {
  component <- match.arg(component)

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
    "all" = rbind(fixed_dat, smooth_dat),
    "conditional" = fixed_dat,
    "smooth_terms" = smooth_dat
  )

  out <- .remove_backticks_from_parameter_names(params)

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



#' @rdname get_parameters.BGGM
#' @export
get_parameters.bamlss <- function(x,
                                  component = c("all", "conditional", "smooth_terms", "location", "distributional", "auxiliary"),
                                  parameters = NULL,
                                  summary = FALSE,
                                  centrality = "mean",
                                  ...) {
  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component)

  parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
  out <- as.data.frame(unclass(x$samples))[unname(unlist(parms[elements]))]
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



#' @rdname get_parameters.BGGM
#' @export
get_parameters.sim.merMod <- function(x,
                                      effects = c("fixed", "random", "all"),
                                      parameters = NULL,
                                      summary = FALSE,
                                      centrality = "mean",
                                      ...) {
  effects <- match.arg(effects)
  fe <- re <- NULL
  if (effects %in% c("fixed", "all")) fe <- .get_armsim_fixef_parms(x)
  if (effects %in% c("random", "all")) re <- .get_armsim_ranef_parms(x)

  dat <- do.call(cbind, .compact_list(list(fe, re)))

  out <- as.data.frame(dat)[.get_parms_data(x, effects, "all", parameters)]

  if (isTRUE(summary)) {
    out <- .summary_of_posteriors(out, centrality = centrality)
  }
  out
}



#' @rdname get_parameters.BGGM
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


#' @importFrom stats median
.summary_of_posteriors <- function(out, centrality = "mean", ...) {
  s <- switch(centrality,
    "mean" = sapply(out, mean),
    "median" = sapply(out, stats::median),
    sapply(out, mean)
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
