#' @title Find names of model parameters
#' @name find_parameters
#'
#' @description Returns the names of model parameters, like they typically
#'     appear in the \code{summary()} output. For Bayesian models, the parameter
#'     names equal the column names of the posterior samples after coercion
#'     from \code{as.data.frame()}.
#'
#' @param parameters Regular expression pattern that describes the parameters that
#'   should be returned.
#' @param effects Should parameters for fixed effects, random effects
#'    or both be returned? Only applies to mixed models. May be abbreviated.
#' @param component Should all parameters, parameters for the
#'    conditional model, the zero-inflated part of the model, the dispersion
#'    term or the instrumental variables be returned? Applies to models
#'    with zero-inflated and/or dispersion formula, or to models with instrumental
#'    variable (so called fixed-effects regressions). May be abbreviated.
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of parameter names. For simple models, only one list-element,
#'    \code{conditional}, is returned. For more complex models, the returned
#'    list may have following elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" part from the model
#'      \item \code{random}, the "random effects" part from the model
#'      \item \code{zero_inflated}, the "fixed effects" part from the zero-inflation component of the model
#'      \item \code{zero_inflated_random}, the "random effects" part from the zero-inflation component of the model
#'      \item \code{dispersion}, the dispersion parameters
#'      \item \code{simplex}, simplex parameters of monotonic effects (\pkg{brms} only)
#'      \item \code{smooth_terms}, the smooth parameters
#'    }
#'
#' @details In most cases when models either return different "effects" (fixed,
#' random) or "components" (conditional, zero-inflated, ...), the arguments
#' \code{effects} and \code{component} can be used. Not all model classes that
#' support these arguments are listed here in the 'Usage' section.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_parameters(m)
#' @importFrom stats coef
#' @export
find_parameters <- function(x, ...) {
  UseMethod("find_parameters")
}



# Default methods -------------------------------------------


#' @export
find_parameters.default <- function(x, flatten = FALSE, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    pars <- find_parameters.gam(x)
  } else {
    pars <- tryCatch(
      {
        p <- .remove_backticks_from_string(names(stats::coef(x)))
        list(conditional = p)
      },
      error = function(x) {
        NULL
      }
    )
  }


  if (is.null(pars$conditional) || is.null(pars)) {
    print_color(sprintf("Parameters can't be retrieved for objects of class '%s'.\n", class(x)[1]), "red")
    return(NULL)
  }


  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.data.frame <- function(x, flatten = FALSE, ...) {
  stop("A data frame is no valid object for this function.")
}






# Ordinal -----------------------------------------------


#' @export
find_parameters.polr <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = c(sprintf("Intercept: %s", names(x$zeta)), names(stats::coef(x))))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.bracl <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.clmm2 <- function(x, flatten = FALSE, ...) {
  s <- summary(x)
  pars <- list(conditional = as.character(rownames(s$coefficients)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.multinom <- function(x, flatten = FALSE, ...) {
  params <- stats::coef(x)


  pars <- if (is.matrix(params)) {
    list(conditional = colnames(params))
  } else {
    list(conditional = names(params))
  }

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.brmultinom <- find_parameters.multinom





# GAM (additive models) ---------------------------------------------


#' @importFrom stats na.omit coef
#' @export
find_parameters.gamlss <- function(x, flatten = FALSE, ...) {
  pars <- lapply(x$parameters, function(i) {
    .remove_backticks_from_string(names(stats::na.omit(stats::coef(x, what = i))))
  })

  names(pars) <- x$parameters
  if ("mu" %in% names(pars)) names(pars)[1] <- "conditional"

  pars <- .compact_list(pars)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @rdname find_parameters
#' @export
find_parameters.gam <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  st <- summary(x)$s.table

  pars$conditional <- pars$conditional[.grep_non_smoothers(pars$conditional)]
  pars$smooth_terms <- row.names(st)

  pars <- .compact_list(pars)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  pars <- .compact_list(pars[elements])

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.Gam <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, ...) {
  pars <- names(stats::coef(x))

  l <- .compact_list(list(
    conditional = pars[.grep_non_smoothers(pars)],
    smooth_terms = pars[.grep_smoothers(pars)]
  ))

  l <- lapply(l, .remove_backticks_from_string)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_parameters.vgam <- find_parameters.Gam



#' @export
find_parameters.gamm <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  component <- match.arg(component)

  l <- find_parameters.gam(x, component = component)

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}











# Mixed Models -------------------------------------------------------


#' @export
find_parameters.glmmTMB <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x)$cond),
    random = lapply(lme4::ranef(x)$cond, colnames),
    zero_inflated = names(lme4::fixef(x)$zi),
    zero_inflated_random = lapply(lme4::ranef(x)$zi, colnames),
    dispersion = names(lme4::fixef(x)$disp)
  ))

  l <- rapply(l, .remove_backticks_from_string, how = "list")

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects = effects, component = component)

  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.MixMod <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  re.names <- dimnames(lme4::ranef(x))[[2]]

  has_zeroinf <- !is.null(find_formula(x)[["zero_inflated"]])

  if (has_zeroinf) {
    z_inflated <- names(lme4::fixef(x, sub_model = "zero_part"))
    z_inflated_random <- re.names[grepl("^zi_", re.names, perl = TRUE)]
  } else {
    z_inflated <- NULL
    z_inflated_random <- NULL
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x, sub_model = "main")),
    random = re.names[grepl("^(?!zi_)", re.names, perl = TRUE)],
    zero_inflated = z_inflated,
    zero_inflated_random = z_inflated_random
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects = effects, component = component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.nlmerMod <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  startvectors <- .get_startvector_from_env(x)

  l <- .compact_list(list(
    conditional = setdiff(names(lme4::fixef(x)), startvectors),
    nonlinear = startvectors,
    random = lapply(lme4::ranef(x), colnames)
  ))

  l <- rapply(l, .remove_backticks_from_string, how = "list")

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @rdname find_parameters
#' @export
find_parameters.merMod <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = lapply(lme4::ranef(x), colnames)
  ))

  l <- rapply(l, .remove_backticks_from_string, how = "list")

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")

  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_parameters.rlmerMod <- find_parameters.merMod


#' @export
find_parameters.coxme <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = names(lme4::ranef(x))
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.mixed <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x$full_model)),
    random = lapply(lme4::ranef(x$full_model), colnames)
  ))

  l <- rapply(l, .remove_backticks_from_string, how = "list")

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")

  l <- .compact_list(l[elements])


  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.lme <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  re <- lme4::ranef(x)
  if (is.data.frame(re)) {
    rn <- colnames(re)
  } else {
    rn <- lapply(re, colnames)
  }

  l <- .compact_list(list(
    conditional = names(lme4::fixef(x)),
    random = rn
  ))

  l <- rapply(l, .remove_backticks_from_string, how = "list")

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")

  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}








# zero-inflated models --------------------------------------------

#' @rdname find_parameters
#' @export
find_parameters.zeroinfl <- function(x, component = c("all", "conditional", "zi", "zero_inflated"), flatten = FALSE, ...) {
  cf <- names(stats::coef(x))

  l <- .compact_list(list(
    conditional = cf[grepl("^count_", cf, perl = TRUE)],
    zero_inflated = cf[grepl("^zero_", cf, perl = TRUE)]
  ))


  l <- lapply(l, .remove_backticks_from_string)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component = component)

  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @rdname find_parameters
#' @export
find_parameters.hurdle <- find_parameters.zeroinfl


#' @export
find_parameters.zerotrunc <- find_parameters.zeroinfl






# Bayesian models -----------------------------------------


#' @export
find_parameters.MCMCglmm <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  sc <- summary(x)
  l <- .compact_list(list(
    conditional = rownames(sc$solutions),
    random = rownames(sc$Gcovariances)
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @rdname find_parameters
#' @export
find_parameters.brmsfit <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "simplex", "sigma", "smooth_terms"), flatten = FALSE, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))
  is_mv <- NULL

  cond <- fe[grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!zi_)(.*)", fe, perl = TRUE)]
  zi <- fe[grepl(pattern = "(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)", fe, perl = TRUE)]
  rand <- fe[grepl(pattern = "(?!.*__zi)(?=.*r_)", fe, perl = TRUE) & !grepl(pattern = "^prior_", fe, perl = TRUE)]
  randzi <- fe[grepl(pattern = "r_(.*__zi)", fe, perl = TRUE)]
  simo <- fe[grepl(pattern = "^simo_", fe, perl = TRUE)]
  smooth_terms <- fe[grepl(pattern = "^sds_", fe, perl = TRUE)]
  priors <- fe[grepl(pattern = "^prior_", fe, perl = TRUE)]
  sigma <- fe[grepl(pattern = "^sigma_", fe, perl = TRUE)]

  l <- .compact_list(list(
    conditional = cond,
    random = rand,
    zero_inflated = zi,
    zero_inflated_random = randzi,
    simplex = simo,
    smooth_terms = smooth_terms,
    sigma = sigma,
    priors = priors
  ))

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects = effects, component = component)
  elements <- c(elements, "priors")

  if (is_multivariate(x)) {
    rn <- names(find_response(x))
    l <- lapply(rn, function(i) {
      if (.obj_has_name(l, "conditional")) {
        conditional <- l$conditional[grepl(sprintf("^(b_|bs_|bsp_|bcs_)\\Q%s\\E_", i), l$conditional)]
      } else {
        conditional <- NULL
      }

      if (.obj_has_name(l, "random")) {
        random <- l$random[grepl(sprintf("__\\Q%s\\E\\.", i), l$random)]
      } else {
        random <- NULL
      }

      if (.obj_has_name(l, "zero_inflated")) {
        zero_inflated <- l$zero_inflated[grepl(sprintf("^(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)\\Q%s\\E_", i), l$zero_inflated)]
      } else {
        zero_inflated <- NULL
      }

      if (.obj_has_name(l, "zero_inflated_random")) {
        zero_inflated_random <- l$zero_inflated_random[grepl(sprintf("__zi_\\Q%s\\E\\.", i), l$zero_inflated_random)]
      } else {
        zero_inflated_random <- NULL
      }

      if (.obj_has_name(l, "simplex")) {
        simplex <- l$simplex
      } else {
        simplex <- NULL
      }

      if (.obj_has_name(l, "sigma")) {
        sigma <- l$sigma[grepl(sprintf("^sigma_\\Q%s\\E", i), l$sigma)]
      } else {
        sigma <- NULL
      }

      if (.obj_has_name(l, "smooth_terms")) {
        smooth_terms <- l$smooth_terms
      } else {
        smooth_terms <- NULL
      }

      if (.obj_has_name(l, "priors")) {
        priors <- l$priors
      } else {
        priors <- NULL
      }

      pars <- .compact_list(list(
        conditional = conditional,
        random = random,
        zero_inflated = zero_inflated,
        zero_inflated_random = zero_inflated_random,
        simplex = simplex,
        smooth_terms = smooth_terms,
        sigma = sigma,
        priors = priors
      ))

      .compact_list(pars[elements])
    })

    names(l) <- rn
    is_mv <- "1"
  } else {
    l <- .compact_list(l[elements])
  }

  l <- .filter_pars(l, parameters, !is.null(is_mv) && is_mv == "1")
  attr(l, "is_mv") <- is_mv

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @importFrom stats coef
#' @rdname find_parameters
#' @export
find_parameters.bayesx <- function(x, component = c("all", "conditional", "smooth_terms"), flatten = FALSE, parameters = NULL, ...) {
  cond <- rownames(stats::coef(x))
  smooth_terms <- rownames(x$smooth.hyp)

  l <- .compact_list(list(
    conditional = cond,
    smooth_terms = smooth_terms
  ))

  l <- .filter_pars(l, parameters)

  component <- match.arg(component)
  elements <- .get_elements(effects = "all", component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @rdname find_parameters
#' @export
find_parameters.stanreg <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "smooth_terms"), flatten = FALSE, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))

  cond <- fe[grepl(pattern = "^(?!(b\\[|sigma|Sigma))", fe, perl = TRUE) & .grep_non_smoothers(fe)]
  rand <- fe[grepl(pattern = "^b\\[", fe, perl = TRUE)]
  smooth_terms <- fe[grepl(pattern = "^smooth_sd", fe, perl = TRUE)]

  l <- .compact_list(list(
    conditional = cond,
    random = rand,
    smooth_terms = smooth_terms
  ))

  l <- .filter_pars(l, parameters)

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects, component)
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.stanmvreg <- function(x, effects = c("all", "fixed", "random"), component = c("all", "conditional", "sigma"), flatten = FALSE, parameters = NULL, ...) {
  fe <- colnames(as.data.frame(x))
  rn <- names(find_response(x))

  cond <- fe[grepl(pattern = "^(?!(b\\[|sigma|Sigma))", fe, perl = TRUE) & .grep_non_smoothers(fe)]
  rand <- fe[grepl(pattern = "^b\\[", fe, perl = TRUE)]
  sigma <- fe[grepl(pattern = "\\|sigma$", fe, perl = TRUE) & .grep_non_smoothers(fe)]

  l <- .compact_list(list(
    conditional = cond,
    random = rand,
    sigma = sigma
  ))


  if (.obj_has_name(l, "conditional")) {
    x1 <- sub(pattern = "(.*)(\\|)(.*)", "\\1", l$conditional)
    x2 <- sub(pattern = "(.*)(\\|)(.*)", "\\3", l$conditional)

    l.cond <- lapply(rn, function(i) {
      list(conditional = x2[which(x1 == i)])
    })
    names(l.cond) <- rn
  } else {
    l.cond <- NULL
  }


  if (.obj_has_name(l, "random")) {
    x1 <- sub(pattern = "b\\[(.*)(\\|)(.*)", "\\1", l$random)
    x2 <- sub(pattern = "(b\\[).*(.*)(\\|)(.*)", "\\1\\4", l$random)

    l.random <- lapply(rn, function(i) {
      list(random = x2[which(x1 == i)])
    })
    names(l.random) <- rn
  } else {
    l.random <- NULL
  }


  if (.obj_has_name(l, "sigma")) {
    l.sigma <- lapply(rn, function(i) {
      list(sigma = "sigma")
    })
    names(l.sigma) <- rn
  } else {
    l.sigma <- NULL
  }


  l <- mapply(c, l.cond, l.random, l.sigma, SIMPLIFY = FALSE)
  l <- .filter_pars(l, parameters, is_mv = TRUE)

  effects <- match.arg(effects)
  component <- match.arg(component)
  elements <- .get_elements(effects, component)
  l <- lapply(l, function(i) .compact_list(i[elements]))

  attr(l, "is_mv") <- "1"

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}




# Simulation models -----------------------------


#' @rdname find_parameters
#' @export
find_parameters.sim.merMod <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, parameters = NULL, ...) {
  fe <- colnames(.get_armsim_fixef_parms(x))
  re <- colnames(.get_armsim_ranef_parms(x))

  l <- .compact_list(list(
    conditional = fe,
    random = re
  ))

  l <- .filter_pars(l, parameters)

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}


#' @export
find_parameters.sim <- function(x, flatten = FALSE, parameters = NULL, ...) {
  l <- .filter_pars(
    list(conditional = colnames(.get_armsim_fixef_parms(x))),
    parameters
  )

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}







# SEM models ------------------------------------------------------

#' @export
find_parameters.blavaan <- function(x, flatten = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  pars <- data.frame(
    pars = names(lavaan::coef(x)),
    comp = NA,
    stringsAsFactors = FALSE
  )

  pars$comp[grepl("~", pars$pars, fixed = TRUE)] <- "regression"
  pars$comp[grepl("=~", pars$pars, fixed = TRUE)] <- "latent"
  pars$comp[grepl("~~", pars$pars, fixed = TRUE)] <- "residual"
  pars$comp[grepl("~1", pars$pars, fixed = TRUE)] <- "intercept"

  pos_latent <- grep("=~", pars$pars, fixed = TRUE)
  pos_residual <- grep("~~", pars$pars, fixed = TRUE)
  pos_intercept <- grep("~1", pars$pars, fixed = TRUE)
  pos_regression <- setdiff(1:nrow(pars), c(pos_latent, pos_residual, pos_intercept))

  pos <- c(min(pos_latent), min(pos_residual), min(pos_intercept), min(pos_regression))

  comp_levels <- c("latent", "residual", "intercept", "regression")
  comp_levels <- comp_levels[order(pos)]

  pars$comp <- factor(pars$comp, levels = comp_levels)
  pars <- split(pars, pars$comp)
  pars <- lapply(pars, function(i) i$pars)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.lavaan <- function(x, flatten = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

  pars <- get_parameters(x)
  pars$Component <- factor(pars$Component, levels = unique(pars$Component))
  pars <- split(pars$Parameter, pars$Component)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}




# Panel models ----------------------------------------


#' @export
find_parameters.wbm <- function(x, flatten = FALSE, ...) {
  s <- summary(x)

  pars <- .compact_list(list(
    conditional = rownames(s$within_table),
    instruments = rownames(s$between_table),
    random = rownames(s$ints_table)
  ))

  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}


#' @export
find_parameters.wbgee <- find_parameters.wbm






# Other models -----------------------------------


#' @export
find_parameters.mlm <- function(x, flatten = FALSE, ...) {
  cs <- stats::coef(summary(x))

  out <- lapply(cs, function(i) {
    list(conditional = .remove_backticks_from_string(rownames(i)))
  })

  names(out) <- gsub("^Response (.*)", "\\1", names(cs))
  attr(out, "is_mv") <- TRUE

  if (flatten) {
    unique(unlist(out))
  } else {
    out
  }
}



#' @export
find_parameters.gbm <- function(x, flatten = FALSE, ...) {
  s <- summary(x, plotit = FALSE)
  pars <- list(conditional = as.character(s$var))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.BBreg <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = rownames(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.lrm <- function(x, flatten = FALSE, ...) {
  pars <- list(conditional = names(stats::coef(x)))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}

#' @export
find_parameters.flexsurvreg <- find_parameters.lrm



#' @export
find_parameters.BBmm <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  l <- .compact_list(list(
    conditional = rownames(x$fixed.coef),
    random = x$namesRand
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.glimML <- function(x, effects = c("all", "fixed", "random"), flatten = FALSE, ...) {
  l <- .compact_list(list(
    conditional = names(x@fixed.param),
    random = names(x@random.param)
  ))

  l <- lapply(l, .remove_backticks_from_string)

  effects <- match.arg(effects)
  elements <- .get_elements(effects, component = "all")
  l <- .compact_list(l[elements])

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.aovlist <- function(x, flatten = FALSE, ...) {
  l <- lapply(stats::coef(x), names)
  # merge "intercept" and "block" into conditional
  # while "Within" becomes "random"
  l <- list(unname(unlist(l[c(1, 2)])), l[[3]])
  l <- lapply(l, .remove_backticks_from_string)

  names(l) <- c("conditional", "random")

  if (flatten) {
    unique(unlist(l))
  } else {
    l
  }
}



#' @export
find_parameters.crq <- function(x, flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(conditional = rownames(sc$coefficients))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.rqss <- function(x, flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(conditional = rownames(sc$coef))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.aareg <- function(x, flatten = FALSE, ...) {
  sc <- summary(x)

  pars <- list(conditional = rownames(sc$table))
  pars$conditional <- .remove_backticks_from_string(pars$conditional)

  if (flatten) {
    unique(unlist(pars))
  } else {
    pars
  }
}



#' @export
find_parameters.rma <- function(x, flatten = FALSE, ...) {
  tryCatch(
    {
      cf <- stats::coef(x)
      pars <- list(conditional = names(cf))

      pars$conditional[grepl("intrcpt", pars$conditional)] <- "(Intercept)"
      pars$conditional <- .remove_backticks_from_string(pars$conditional)

      if (flatten) {
        unique(unlist(pars))
      } else {
        pars
      }
    },
    error = function(x) {
      NULL
    }
  )
}
