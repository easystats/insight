#' @title Get model parameters
#' @name get_parameters
#'
#' @description Returns the point estimates (or posterior samples for Bayesian
#'    models) from a model.
#'
#' @param iterations Number of posterior draws.
#' @param progress Display progress.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return \itemize{
#'   \item for non-Bayesian models and if \code{effects = "fixed"}, a data frame with two columns: the parameter names and the related point estimates
#'   \item if \code{effects = "random"}, a list of data frames with the random effects (as returned by \code{ranef()}
#'   \item for Bayesian models, the posterior samples from the requested parameters as data frame.
#'   \item for Anova (\code{aov()}) with error term, a list of parameters for the conditional, the within-subject and the between-subjects parameters
#'   \item for models with smooth terms or zero-inflation component, a data frame with three columns: the parameter names, the related point estimates and the component
#' }
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' \code{effects} and \code{component} can be used. Not all model classes that
#' support these arguments are listed here in the 'Usage' section.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @importFrom stats coef
#' @export
get_parameters <- function(x, ...) {
  UseMethod("get_parameters")
}


#' @export
get_parameters.default <- function(x, ...) {
  if (inherits(x, "list") && obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    return(get_parameters.gam(x, ...))
  }

  ## TODO We may think about returning standard errors as well in a
  ## later update... However, we need to check models classes. "rms::lrm()"
  ## e.g. is of class "lrm", "glm" and "lm", and would go into this default
  ## method, which works with "stats::coef()", but not with "summary()$coefficients"

  # cf <- summary(x)$coefficients
  # data.frame(
  #   parameter = row.names(cf),
  #   estimate = unname(cf[, 1]),
  #   # std.error = unname(cf[, 2]),
  #   stringsAsFactors = FALSE,
  #   row.names = NULL
  # )

  tryCatch({
    cf <- stats::coef(x)
    data.frame(
      parameter = names(cf),
      estimate = unname(cf),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
get_parameters.gbm <- function(x, ...) {
  s <- summary(x, plotit = FALSE)
  data.frame(
    parameter = as.character(s$var),
    estimate = s$rel.inf,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
get_parameters.BBreg <- function(x, ...) {
  pars <- summary(x)$coefficients
  data.frame(
    parameter = rownames(pars),
    estimate = pars[, "Estimate"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @rdname get_parameters
#' @export
get_parameters.BBmm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = x$fixed.coef,
    random = x$random.coef
  ))

  fixed <- data.frame(
    parameter = rownames(l$conditional),
    estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.glimML <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = x@fixed.param,
    random = x@random.param
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = l$conditional,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  random <- data.frame(
    parameter = names(l$random),
    estimate = l$random,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (effects == "fixed") {
    fixed
  } else {
    random
  }
}


#' @export
get_parameters.gamlss <- function(x, ...) {
  pars <- list(
    conditional = stats::coef(x),
    sigma = stats::coef(x, what = "sigma"),
    nu = stats::coef(x, what = "nu"),
    tau = stats::coef(x, what = "tau")
  )

  data.frame(
    parameter = c(names(pars$conditional), names(pars$sigma), names(pars$nu), names(pars$tau)),
    estimate = c(unname(pars$conditional), unname(pars$sigma), unname(pars$nu), unname(pars$tau)),
    component = c(
      rep("conditional", length(pars$conditional)),
      rep("sigma", length(pars$sigma)),
      rep("nu", length(pars$nu)),
      rep("tau", length(pars$tau))
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
get_parameters.lrm <- function(x, ...) {
  tryCatch({
    cf <- stats::coef(x)
    data.frame(
      parameter = names(cf),
      estimate = unname(cf),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
get_parameters.aov <- function(x, ...) {
  cf <- stats::coef(x)
  data.frame(
    parameter = names(cf),
    estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
get_parameters.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}


#' @rdname get_parameters
#' @export
get_parameters.gam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  st <- summary(x)$s.table
  smooth_terms <- st[, 1]
  names(smooth_terms) <- row.names(st)

  return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = smooth_terms,
    component = component
  )
}


#' @rdname get_parameters
#' @export
get_parameters.vgam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = pars[.grep_smoothers(names(pars))],
    component = component
  )
}


#' @export
get_parameters.crq <- function(x, ...) {
  sc <- summary(x)
  data.frame(
    parameter = names(sc$coefficients[ ,1]),
    estimate = unname(sc$coefficients[ ,1])
  )
}


#' @export
get_parameters.rqss <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  sc <- summary(x)

  smooth_terms <- sc$qsstab[, 3]
  names(smooth_terms) <- rownames(sc$qsstab)

  return_smooth_parms(
    conditional = sc$coef[ ,1],
    smooth_terms = smooth_terms,
    component = component
  )
}


#' @rdname get_parameters
#' @export
get_parameters.Gam <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)
  pars <- stats::coef(x)

  return_smooth_parms(
    conditional = pars[.grep_non_smoothers(names(pars))],
    smooth_terms = pars[.grep_smoothers(names(pars))],
    component = component
  )
}


#' @rdname get_parameters
#' @export
get_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  return_zeroinf_parms(x, component)
}


#' @export
get_parameters.zerotrunc <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  return_zeroinf_parms(x, component)
}


#' @rdname get_parameters
#' @export
get_parameters.gamm <- function(x, component = c("all", "conditional", "smooth_terms"), ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  get_parameters.gam(x, component, ...)
}


#' @export
get_parameters.aovlist <- function(x, ...) {
  l <- lapply(stats::coef(x), function(i) {
    data.frame(
      parameter = names(i),
      estimate = unname(i),
      stringsAsFactors = FALSE
    )
  })
  names(l) <- c("conditional", "between", "within")
  l
}


#' @rdname get_parameters
#' @export
get_parameters.hurdle <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  return_zeroinf_parms(x, component)
}


#' @export
get_parameters.MCMCglmm <- function(x, effects = c("fixed", "random"), ...) {
  effects <- match.arg(effects)
  sc <- summary(x)

  l <- compact_list(list(
    conditional = sc$solutions[, 1],
    random = sc$Gcovariances[, 1]
  ))

  names(l$conditional) <- rownames(sc$solutions)
  names(l$random) <- rownames(sc$Gcovariances)

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  random <- data.frame(
    parameter = names(l$random),
    estimate = unname(l$random),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.coxme <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x),
    random = lme4::ranef(x)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.merMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x),
    random = lme4::ranef(x)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.rlmerMod <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x),
    random = lme4::ranef(x)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.mixed <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x$full_model),
    random = lme4::ranef(x$full_model)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.lme <- function(x, effects = c("fixed", "random"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)

  l <- compact_list(list(
    conditional = lme4::fixef(x),
    random = lme4::ranef(x)
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    fixed
  } else {
    l$random
  }
}


#' @rdname get_parameters
#' @export
get_parameters.MixMod <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  has_zeroinf <- !is.null(find_formula(x)[["zero_inflated"]])

  if (component %in% c("zi", "zero_inflated") && !has_zeroinf) {
    stop("Model has no zero-inflation component.", call. = FALSE)
  }


  re.names <- dimnames(lme4::ranef(x))[[2]]
  re <- lme4::ranef(x)


  if (has_zeroinf) {
    z_inflated <- lme4::fixef(x, sub_model = "zero_part")
    z_inflated_random <- re[grepl("^zi_", re.names, perl = TRUE)]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
    component <- "conditional"
  }


  l <- compact_list(list(
    conditional = lme4::fixef(x, sub_model = "main"),
    random = re[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    component = "conditional",
    stringsAsFactors = FALSE
  )

  if (has_zeroinf) {
    fixedzi <- data.frame(
      parameter = names(l$zero_inflated),
      estimate = unname(l$zero_inflated),
      component = "zero_inflated",
      stringsAsFactors = FALSE
    )
  } else {
    fixedzi <- NULL
  }

  if (effects == "fixed") {
    switch(
      component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
  } else if (effects == "random") {
    switch(
      component,
      all = compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = list(random = l$random),
      zi = ,
      zero_inflated = list(zero_inflated_random = l$zero_inflated_random)
    )
  }
}


#' @rdname get_parameters
#' @export
get_parameters.glmmTMB <- function(x, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  l <- compact_list(list(
    conditional = lme4::fixef(x)$cond,
    random = lme4::ranef(x)$cond,
    zero_inflated = lme4::fixef(x)$zi,
    zero_inflated_random = lme4::ranef(x)$zi,
    dispersion = lme4::fixef(x)$disp
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    component = "conditional",
    stringsAsFactors = FALSE
  )

  fixedzi <- data.frame(
    parameter = names(l$zero_inflated),
    estimate = unname(l$zero_inflated),
    component = "zero_inflated",
    stringsAsFactors = FALSE
  )

  if (effects == "fixed") {
    switch(
      component,
      all = rbind(fixed, fixedzi),
      conditional = fixed,
      zi = ,
      zero_inflated = fixedzi
    )
  } else if (effects == "random") {
    switch(
      component,
      all = compact_list(list(random = l$random, zero_inflated_random = l$zero_inflated_random)),
      conditional = l$random,
      zi = ,
      zero_inflated = l$zero_inflated_random
    )
  }
}


#' @rdname get_parameters
#' @export
get_parameters.brmsfit <- function(x, effects = c("fixed", "random", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "simplex", "sigma", "smooth_terms"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (is_multivariate(x)) {
    parms <- find_parameters(x, flatten = FALSE, parameters = parameters)
    elements <- .get_elements(effects, component)
    as.data.frame(x)[unlist(lapply(parms, function(i) i[elements]))]
  } else {
    as.data.frame(x)[get_parms_data(x, effects, component, parameters)]
  }
}


#' @rdname get_parameters
#' @export
get_parameters.stanreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  as.data.frame(x)[get_parms_data(x, effects, "all", parameters)]
}


#' @rdname get_parameters
#' @export
get_parameters.BFBayesFactor <- function(x, iterations = 4000, progress = FALSE, ...) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function needs `BayesFactor` to be installed.")
  }

  if (.classify_BFBayesFactor(x) == "correlation") {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, ...)
      ))
    data.frame("rho" = posteriors$rho)
  } else if (.classify_BFBayesFactor(x) == "ttest") {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, ...)
      ))
    data.frame("Difference" = posteriors$mu)
  } else if (.classify_BFBayesFactor(x) == "meta") {
    posteriors <-
      as.data.frame(suppressMessages(
        BayesFactor::posterior(x, iterations = iterations, progress = progress, ...)
      ))
    data.frame("Effect" = posteriors$delta)
  } else{
    NULL
  }
}


#' @rdname get_parameters
#' @export
get_parameters.stanmvreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
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

  as.data.frame(x)[unlist(lapply(compact_list(parms), function(i) i[elements]))]
}


get_parms_data <- function(x, effects, component, parameters = NULL) {
  elements <- .get_elements(effects, component)
  unlist(find_parameters(x, flatten = FALSE, parameters = parameters)[elements])
}


return_zeroinf_parms <- function(x, component) {
  cf <- stats::coef(x)

  conditional <- grepl("^count_", names(cf), perl = TRUE)
  zero_inflated <- grepl("^zero_", names(cf), perl = TRUE)

  cond <- data.frame(
    parameter = names(cf)[conditional],
    estimate = unname(cf)[conditional],
    component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zi <- data.frame(
    parameter = names(cf)[zero_inflated],
    estimate = unname(cf)[zero_inflated],
    component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )

  if (component != "all") {
    pars <- .remove_column(pars, "component")
  }

  pars
}


return_smooth_parms <- function(conditional, smooth_terms, component) {
  cond <- data.frame(
    parameter = names(conditional),
    estimate = conditional,
    component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  smooth <-  data.frame(
    parameter = names(smooth_terms),
    estimate = smooth_terms,
    component = "smooth_terms",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  pars <- switch(
    component,
    all = rbind(cond, smooth),
    conditional = cond,
    smooth_terms = smooth
  )

  if (component != "all") {
    pars <- .remove_column(pars, "component")
  }

  pars
}
