#' @title Get model parameters
#' @name get_parameters
#'
#' @description Returns the point estimates (or posterior samples for Bayesian
#'    models) from a model.
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#' @inheritParams find_parameters
#'
#' @return For non-Bayesian models and if \code{effects = "fixed"}, a data frame
#'    with two columns: the parameter names and the related point estimates; if
#'    \code{effects = "random"}, a list with the random effects (as returned by
#'    \code{ranef()}. For Bayesian models, the posterior samples from the
#'    requested parameters as data frame. For Anova (\code{aov()}) with error
#'    term, a list of parameters for the conditional, the within-subject and
#'    the between-subjects parameters.
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
  }

  tryCatch({
    cf <- stats::coef(x)
    data.frame(
      parameter = names(cf),
      estimate = unname(cf),
      stringsAsFactors = FALSE
    )
  },
  error = function(x) {
    NULL
  }
  )
}


#' @export
get_parameters.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
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


#' @export
get_parameters.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
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

  re.names <- dimnames(lme4::ranef(x))[[2]]
  re <- lme4::ranef(x)

  l <- compact_list(list(
    conditional = lme4::fixef(x, sub_model = "main"),
    random = re[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = lme4::fixef(x, sub_model = "zero_part"),
    zero_inflated_random = re[grepl("^zi_", re.names, perl = TRUE)]
  ))

  fixed <- data.frame(
    parameter = names(l$conditional),
    estimate = unname(l$conditional),
    group = "conditional",
    stringsAsFactors = FALSE
  )

  fixedzi <- data.frame(
    parameter = names(l$zero_inflated),
    estimate = unname(l$zero_inflated),
    group = "zero_inflated",
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
    group = "conditional",
    stringsAsFactors = FALSE
  )

  fixedzi <- data.frame(
    parameter = names(l$zero_inflated),
    estimate = unname(l$zero_inflated),
    group = "zero_inflated",
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
get_parameters.brmsfit <- function(x, effects = c("fixed", "random", "simplex", "smooth_terms", "all"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (is_multivariate(x)) {
    parms <- find_parameters(x, parameters)
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
get_parameters.stanmvreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  elements <- .get_elements(effects, "all")
  parms <- find_parameters(x, parameters)

  for (i in names(parms)) {
    parms[[i]]$conditional <- sprintf("%s|%s", i, parms[[i]]$conditional)
    find_bracket <- regexpr(pattern = "\\[", parms[[i]]$random)
    parms[[i]]$random <- paste0(
      substr(parms[[i]]$random, start = 1, stop = find_bracket),
      i, "|",
      substr(parms[[i]]$random, start = find_bracket + 1, stop = 1000000L)
    )
  }

  as.data.frame(x)[unlist(lapply(parms, function(i) i[elements]))]
}


get_parms_data <- function(x, effects, component, parameters = NULL) {
  elements <- .get_elements(effects, component)
  unlist(find_parameters(x, parameters)[elements])
}


return_zeroinf_parms <- function(x, component) {
  cf <- stats::coef(x)

  conditional <- grepl("^count_", names(cf), perl = TRUE)
  zero_inflated <- grepl("^zero_", names(cf), perl = TRUE)

  cond <- data.frame(
    parameter = names(cf)[conditional],
    estimate = unname(cf)[conditional],
    group = "conditional",
    stringsAsFactors = FALSE
  )

  zi <- data.frame(
    parameter = names(cf)[zero_inflated],
    estimate = unname(cf)[zero_inflated],
    group = "zero_inflated",
    stringsAsFactors = FALSE
  )

  switch(
    component,
    all = rbind(cond, zi),
    conditional = cond,
    zi = ,
    zero_inflated = zi
  )
}
