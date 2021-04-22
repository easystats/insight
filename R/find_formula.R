#' @title Find model formula
#' @name find_formula
#'
#' @description Returns the formula(s) for the different parts of a model
#'    (like fixed or random effects, zero-inflated component, ...).
#'
#' @param ... Currently not used.
#' @inheritParams find_predictors
#'
#' @return A list of formulas that describe the model. For simple models,
#'    only one list-element, \code{conditional}, is returned. For more complex
#'    models, the returned list may have following elements:
#'    \itemize{
#'      \item \code{conditional}, the "fixed effects" part from the model. One exception are \code{DirichletRegModel} models from \pkg{DirichletReg}, which has two or three components, depending on \code{model}.
#'      \item \code{random}, the "random effects" part from the model (or the \code{id} for gee-models and similar)
#'      \item \code{zero_inflated}, the "fixed effects" part from the zero-inflation component of the model
#'      \item \code{zero_inflated_random}, the "random effects" part from the zero-inflation component of the model
#'      \item \code{dispersion}, the dispersion formula
#'      \item \code{instruments}, for fixed-effects regressions like \code{ivreg::ivreg()}, \code{lfe::felm()} or \code{plm::plm()}, the instrumental variables
#'      \item \code{cluster}, for fixed-effects regressions like \code{lfe::felm()}, the cluster specification
#'      \item \code{correlation}, for models with correlation-component like \code{nlme::gls()}, the formula that describes the correlation structure
#'      \item \code{slopes}, for fixed-effects individual-slope models like \code{feisr::feis()}, the formula for the slope parameters
#'      \item \code{precision}, for \code{DirichletRegModel} models from \pkg{DirichletReg}, when parametrization (i.e. \code{model}) is \code{"alternative"}.
#'    }
#'
#' @note For models of class \code{lme} or \code{gls} the correlation-component
#'   is only returned, when it is explicitly defined as named argument
#'   (\code{form}), e.g. \code{corAR1(form = ~1 | Mare)}
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_formula(m)
#'
#' if (require("lme4")) {
#'   m <- lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
#'   f <- find_formula(m)
#'   f
#'   format(f)
#' }
#' @export
find_formula <- function(x, ...) {
  UseMethod("find_formula")
}



# Default method -----------------------------------


#' @export
find_formula.default <- function(x, ...) {
  f <- tryCatch(
    {
      list(conditional = stats::formula(x))
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}



#' @export
find_formula.list <- function(x, ...) {
  if (.obj_has_name(x, "gam")) {
    if ("mer" %in% names(x)) {
      f.random <- .fix_gamm4_random_effect(find_formula(x$mer)$random)
      if (length(f.random) == 1) {
        f.random <- f.random[[1]]
      } else if (length(f.random) == 0) {
        f.random <- NULL
      }
    }
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
    f <- .compact_list(list(conditional = stats::formula(x), random = f.random))
  } else {
    f <- find_formula.default(x, ...)
  }
  .find_formula_return(f)
}



#' @export
find_formula.data.frame <- function(x, ...) {
  stop("A data frame is not a valid object for this function.")
}



#' @export
find_formula.aovlist <- function(x, ...) {
  f <- attr(x, "terms", exact = TRUE)
  attributes(f) <- NULL
  .find_formula_return(list(conditional = f))
}



#' @export
find_formula.anova <- function(x, ...) {
  stop("Formulas cannot be retrieved from anova() objects.")
}






# GAM -----------------------------------------------------------


#' @export
find_formula.SemiParBIV <- function(x, ...) {
  f <- stats::formula(x, ...)
  names(f) <- c("Equation 1", "Equation 2", "Equation 3")[1:length(f)]
  f <- list(conditional = f)
  .find_formula_return(f)
}


#' @export
find_formula.gam <- function(x, ...) {
  f <- tryCatch(
    {
      stats::formula(x)
    },
    error = function(x) {
      NULL
    }
  )

  if (!is.null(f)) {
    if (is.list(f)) {
      mi <- .gam_family(x)
      if (!is.null(mi) && mi$family == "ziplss") {
        # handle formula for zero-inflated models
        f <- list(conditional = f[[1]], zero_inflated = f[[2]])
      } else if (mi$family == "Multivariate normal") {
        # handle formula for multivariate models
        r <- lapply(f, function(.i) deparse(.i[[2]]))
        f <- lapply(f, function(.i) list(conditional = .i))
        names(f) <- r
        attr(f, "is_mv") <- "1"
      }
    } else {
      f <- list(conditional = f)
    }
  }

  .find_formula_return(f)
}



#' @export
find_formula.gamlss <- function(x, ...) {
  f <- tryCatch(
    {
      f.cond <- stats::as.formula(.get_fixed_effects(x$mu.formula))

      f.random <- lapply(.findbars(x$mu.formula), function(.x) {
        f <- .safe_deparse(.x)
        stats::as.formula(paste0("~", f))
      })

      if (length(f.random) == 1) {
        f.random <- f.random[[1]]
      } else if (grepl("random\\((.*)\\)", .safe_deparse(f.cond))) {
        re <- gsub("(.*)random\\((.*)\\)", "\\2", .safe_deparse(f.cond))
        f.random <- stats::as.formula(paste0("~1|", re))
        f.cond <- stats::update.formula(f.cond, stats::as.formula(paste0(". ~ . - random(", re, ")")))
      }

      .compact_list(list(
        conditional = f.cond,
        random = f.random,
        sigma = x$sigma.formula,
        nu = x$nu.formula,
        tau = x$tau.formula
      ))
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}



#' @export
find_formula.bamlss <- function(x, ...) {
  f <- stats::formula(x)

  if (!is.null(f$mu)) {
    f.cond <- f$mu$formula
  } else if (!is.null(f$pi)) {
    f.cond <- f$pi$formula
  }

  if (!is.null(f$sigma)) {
    f.sigma <- stats::as.formula(paste0("~", as.character(f$sigma$formula)[3]))
  } else if (!is.null(f$pi)) {
    f.sigma <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(.safe_deparse(f.cond)),
    sigma = f.sigma
  ))
  .find_formula_return(f)
}



#' @export
find_formula.gamm <- function(x, ...) {
  f <- .compact_list(find_formula(x$gam))
  random <- .fix_gamm_random_effect(names(x$lme$groups))

  if (length(random) == 0) {
    f.random <- NULL
  } else if (length(random) > 1) {
    f.random <- lapply(random, function(r) stats::as.formula(paste0("~1|", r)))
  } else {
    f.random <- stats::as.formula(paste0("~1|", random))
  }

  .find_formula_return(.compact_list(c(f, list(random = f.random))))
}






# Meta-Analysis -----------------------


#' @export
find_formula.rma <- function(x, ...) {
  NULL
}

#' @export
find_formula.metaplus <- find_formula.rma

#' @export
find_formula.meta_random <- find_formula.rma

#' @export
find_formula.meta_fixed <- find_formula.rma

#' @export
find_formula.meta_bma <- find_formula.rma







# Other models ----------------------------------------------


#' @export
find_formula.censReg <- find_formula.default

#' @export
find_formula.maxLik <- find_formula.default

#' @export
find_formula.maxim <- find_formula.default


#' @export
find_formula.selection <- function(x, ...) {
  model_call <- parse(text = deparse(get_call(x)))[[1]]
  f <- list(conditional = list(selection = stats::as.formula(model_call$selection),
                               outcome = stats::as.formula(model_call$outcome)))
  attr(f, "two_stage") <- TRUE
  .find_formula_return(f)
}


#' @export
find_formula.svy_vglm <- function(x, ...) {
  find_formula(x$fit)
}


#' @export
find_formula.mjoint <- function(x, ...) {
  s <- summary(x)

  f.cond <- s$formLongFixed
  if (length(s$formLongFixed) == 1) {
    names(f.cond) <- "conditional"
  } else {
    names(f.cond) <- paste0("conditional", 1:length(f.cond))
  }

  f.rand <- s$formLongRandom
  if (length(s$formLongRandom) == 1) {
    names(f.rand) <- "random"
  } else {
    names(f.rand) <- paste0("random", 1:length(f.rand))
  }

  f <- c(f.cond, f.rand, list(survival = s$formSurv))
  .find_formula_return(f)
}


#' @export
find_formula.mvord <- function(x, ...) {
  f <- list(conditional = x$rho$formula)
  .find_formula_return(f)
}


#' @export
find_formula.btergm <- function(x, ...) {
  f <- list(conditional = x@formula)
  .find_formula_return(f)
}


#' @export
find_formula.mediate <- function(x, ...) {
  f <- list(
    mediator = find_formula(x$model.m),
    outcome = find_formula(x$model.y)
  )
  .find_formula_return(f)
}


#' @export
find_formula.averaging <- function(x, ...) {
  f_random <- tryCatch(
    {
      models <- attributes(x)$modelList
      find_formula(models[[1]])
    },
    error = function(e) {
      NULL
    }
  )

  f <- find_formula.default(x)
  if (!.obj_has_name(f, "random") && .obj_has_name(f_random, "random")) {
    f$random <- f_random$random
  }

  .find_formula_return(f)
}


#' @export
find_formula.glht <- function(x, ...) {
  .find_formula_return(list(conditional = stats::formula(x$model)))
}


#' @export
find_formula.joint <- function(x, ...) {
  f <- stats::formula(x)
  .find_formula_return(list(conditional = f$lformula, survival = f$sformula))
}


#' @export
find_formula.betareg <- function(x, ...) {
  f <- stats::formula(x)
  fs <- .safe_deparse(f)

  if (grepl("|", fs, fixed = TRUE)) {
    fs <- trimws(unlist(strsplit(fs, "|", fixed = TRUE)))
    f <- list(
      conditional = stats::as.formula(fs[1]),
      precision = stats::as.formula(paste0("~", fs[2]))
    )
  } else {
    f <- list(conditional = f)
  }
  .find_formula_return(f)
}


#' @export
find_formula.afex_aov <- function(x, ...) {
  if ("aov" %in% names(x)) {
    find_formula(x$aov)
  } else {
    find_formula(x$lm)
  }
}


#' @export
find_formula.mira <- function(x, ...) {
  .find_formula_return(find_formula(x$analyses[[1]]))
}


#' @export
find_formula.gee <- function(x, ...) {
  f <- tryCatch(
    {
      id <- parse(text = .safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", .safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", .safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}



#' @export
find_formula.MANOVA <- function(x, ...) {
  f <- .compact_list(list(
    conditional = x$input$formula,
    random = stats::as.formula(paste0("~", x$input$subject))
  ))
  .find_formula_return(f)
}

#' @export
find_formula.RM <- find_formula.MANOVA



#' @export
find_formula.gls <- function(x, ...) {
  ## TODO this is an intermediate fix to return the correlation variables from gls-objects
  fcorr <- x$call$correlation
  if (!is.null(fcorr)) {
    f_corr <- parse(text = .safe_deparse(x$call$correlation))[[1]]
  } else {
    f_corr <- NULL
  }
  if (is.symbol(f_corr)) {
    f_corr <- paste("~", .safe_deparse(f_corr))
  } else {
    f_corr <- f_corr$form
  }

  l <- tryCatch(
    {
      list(
        conditional = stats::formula(x),
        correlation = stats::as.formula(f_corr)
      )
    },
    error = function(x) {
      NULL
    }
  )

  .find_formula_return(.compact_list(l))
}



#' @export
find_formula.LORgee <- function(x, ...) {
  f <- tryCatch(
    {
      id <- parse(text = .safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", .safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", .safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}



#' @export
find_formula.cglm <- function(x, ...) {
  f <- tryCatch(
    {
      id <- parse(text = .safe_deparse(x$call))[[1]]$id

      # alternative regex-patterns that also work:
      # sub(".*id ?= ?(.*?),.*", "\\1", .safe_deparse(x$call), perl = TRUE)
      # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", .safe_deparse(x$call), perl = TRUE)

      list(
        conditional = stats::formula(x),
        random = stats::as.formula(paste0("~", id))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}







# mfx models ---------------------------------------


#' @export
find_formula.betamfx <- find_formula.betareg

#' @export
find_formula.betaor <- find_formula.betareg

#' @export
find_formula.logitmfx <- function(x, ...) {
  find_formula.default(x$fit, ...)
}

#' @export
find_formula.poissonmfx <- find_formula.logitmfx

#' @export
find_formula.negbinmfx <- find_formula.logitmfx

#' @export
find_formula.logitor <- find_formula.logitmfx

#' @export
find_formula.negbinirr <- find_formula.logitmfx

#' @export
find_formula.poissonirr <- find_formula.logitmfx

#' @export
find_formula.probitmfx <- find_formula.logitmfx








# Panel data models ---------------------------------------


#' @export
find_formula.ivreg <- function(x, ...) {
  f <- tryCatch(
    {
      f <- .safe_deparse(stats::formula(x))
      cond <- .trim(substr(f, start = 0, stop = regexpr(pattern = "\\|", f) - 1))
      instr <- .trim(substr(f, regexpr(pattern = "\\|", f) + 1, stop = 10000L))

      list(
        conditional = stats::as.formula(cond),
        instruments = stats::as.formula(paste0("~", instr))
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}


#' @export
find_formula.iv_robust <- find_formula.ivreg


#' @export
find_formula.ivFixed <- find_formula.ivreg


#' @export
find_formula.plm <- function(x, ...) {
  f <- tryCatch(
    {
      f <- .safe_deparse(stats::formula(x))
      bar_pos <- regexpr(pattern = "\\|", f)

      if (bar_pos == -1) {
        stop_pos <- nchar(f) + 1
      } else {
        stop_pos <- bar_pos
      }

      cond <- .trim(substr(f, start = 0, stop = stop_pos - 1))
      instr <- .trim(substr(f, stop_pos + 1, stop = 10000L))

      if (.is_empty_string(instr)) {
        list(conditional = stats::as.formula(cond))
      } else {
        # check if formula starts with dot, and remove it
        instr <- gsub("(^\\.\\s*)(.*)", "\\2", instr)
        list(
          conditional = stats::as.formula(cond),
          instruments = stats::as.formula(paste0("~", instr))
        )
      }
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}



#' @export
find_formula.felm <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- .trim(unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE)))

  f.cond <- f_parts[1]

  if (length(f_parts) > 1) {
    f.rand <- paste0("~", f_parts[2])
  } else {
    f.rand <- NULL
  }

  if (length(f_parts) > 2) {
    f.instr <- paste0("~", f_parts[3])
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 3) {
    f.clus <- paste0("~", f_parts[4])
  } else {
    f.clus <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.mhurdle <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x)[[3]])
  f_parts <- .trim(unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE)))

  f.zi <- paste0("~", f_parts[1])

  if (length(f_parts) > 1) {
    f.cond <- paste0(.safe_deparse(stats::formula(x)[[2]]), "~", f_parts[2])
  } else {
    f.cond <- NULL
  }

  if (length(f_parts) > 2) {
    f.ip <- paste0("~", f_parts[3])
  } else {
    f.ip <- NULL
  }

  # remove "empty" parts
  if (f.zi == "~0") {
    f.zi <- NULL
  }
  if (f.ip == "~0") {
    f.ip <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    zero_inflated = stats::as.formula(f.zi),
    infrequent_purchase = stats::as.formula(f.ip)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.feglm <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.instr <- paste0("~", .trim(f_parts[2]))
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 2) {
    f.clus <- paste0("~", .trim(f_parts[3]))
  } else {
    f.clus <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.fixest <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.clus <- paste0("~", .trim(f_parts[2]))
  } else {
    f.clus <- parse(text = deparse(x$call))[[1]]$fixef
    if (!is.null(f.clus)) {
      f.clus <- paste("~", paste(eval(f.clus), collapse = " + "))
    }
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    cluster = stats::as.formula(f.clus)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.feis <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])
  id <- parse(text = .safe_deparse(x$call))[[1]]$id

  # alternative regex-patterns that also work:
  # sub(".*id ?= ?(.*?),.*", "\\1", .safe_deparse(x$call), perl = TRUE)
  # sub(".*\\bid\\s*=\\s*([^,]+).*", "\\1", .safe_deparse(x$call), perl = TRUE)

  if (length(f_parts) > 1) {
    f.slopes <- paste0("~", .trim(f_parts[2]))
  } else {
    f.slopes <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    slopes = stats::as.formula(f.slopes),
    random = stats::as.formula(paste0("~", id))
  ))
  .find_formula_return(f)
}



#' @export
find_formula.bife <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "|", fixed = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.rand <- paste0("~", .trim(f_parts[2]))
  } else {
    f.rand <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.ivprobit <- function(x, ...) {
  NULL
}



#' @export
find_formula.wbm <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.instr <- paste0("~", .trim(f_parts[2]))
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 2) {
    f_parts[3] <- .trim(f_parts[3])
    if (grepl("\\((.+)\\|(.+)\\)", f_parts[3])) {
      # we have multiple random effects, which we can better extract
      # via ".findbars()"
      if (length(gregexpr("\\|", f_parts[3])[[1]]) > 1) {
        f.rand <- .findbars(stats::as.formula(paste("~", f_parts[3])))
      } else {
        f.rand <- gsub("(\\(|\\))", "", f_parts[3])
        f.rand <- stats::as.formula(paste0("~", .trim(f.rand)))
      }
      f.clint <- NULL
    } else {
      ## TODO dangerous fix to convert cross-level interactions
      # into random effects...
      f.clint <- f_parts[3]
      f.clint <- paste0("~", .trim(f.clint))
      f.rand <- NULL
    }
  } else {
    f.rand <- NULL
    f.clint <- NULL
  }

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    interactions = stats::as.formula(f.clint),
    random = f.rand
  ))
  .find_formula_return(f)
}

#' @export
find_formula.wbgee <- find_formula.wbm


#' @export
find_formula.glimML <- function(x, ...) {
  f <- .compact_list(list(
    conditional = x@formula,
    random = x@random
  ))
  .find_formula_return(f)
}



#' @export
find_formula.tobit <- function(x, ...) {
  f <- tryCatch(
    {
      list(conditional = parse(text = .safe_deparse(x$call))[[1]]$formula)
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}





# Zero inflated models --------------------------------------


#' @export
find_formula.hurdle <- function(x, ...) {
  .zeroinf_formula(x)
}

#' @export
find_formula.zeroinfl <- find_formula.hurdle

#' @export
find_formula.zerotrunc <- find_formula.hurdle


#' @export
find_formula.zcpglm <- function(x, ...) {
  .zeroinf_formula(x, separator = "\\|\\|")
}









# Ordinal models  --------------------------------------


#' @export
find_formula.clmm2 <- function(x, ...) {
  f <- .compact_list(list(
    conditional = stats::as.formula(.safe_deparse(attr(x$location, "terms", exact = TRUE))),
    scale = stats::as.formula(.safe_deparse(attr(x$scale, "terms", exact = TRUE))),
    random = stats::as.formula(paste0("~", parse(text = .safe_deparse(x$call))[[1]]$random))
  ))
  .find_formula_return(f)
}


#' @export
find_formula.clm2 <- function(x, ...) {
  f <- .compact_list(list(
    conditional = stats::formula(attr(x$location, "terms", exact = TRUE)),
    scale = stats::formula(attr(x$scale, "terms", exact = TRUE))
  ))
  .find_formula_return(f)
}


#' @export
find_formula.DirichletRegModel <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.cond2 <- paste0("~", .trim(f_parts[2]))
  } else {
    f.cond2 <- NULL
  }

  if (length(f_parts) > 2) {
    f.cond3 <- paste0("~", .trim(f_parts[3]))
  } else {
    f.cond3 <- NULL
  }

  out <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    conditional2 = stats::as.formula(f.cond2),
    conditional3 = stats::as.formula(f.cond3)
  ))

  if (x$parametrization == "alternative") {
    if (length(out) == 2) names(out)[2] <- "precision"
  }

  .find_formula_return(out)
}












# Mixed models -----------------------


#' @export
find_formula.glmmTMB <- function(x, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, component = "zi")
  f.disp <- stats::formula(x, component = "disp")

  if (identical(.safe_deparse(f.zi), "~0") ||
    identical(.safe_deparse(f.zi), "~1")) {
    f.zi <- NULL
  }

  if (identical(.safe_deparse(f.disp), "~0") ||
    identical(.safe_deparse(f.disp), "~1")) {
    f.disp <- NULL
  }


  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.zirandom <- lapply(.findbars(f.zi), function(.x) {
    f <- .safe_deparse(.x)
    if (f == "NULL") {
      return(NULL)
    }
    stats::as.formula(paste0("~", f))
  })

  if (length(f.zirandom) == 1) {
    f.zirandom <- f.zirandom[[1]]
  }


  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  if (!is.null(f.zi)) f.zi <- stats::as.formula(.get_fixed_effects(f.zi))

  f <- .compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom,
    dispersion = f.disp
  ))
  .find_formula_return(f)
}



#' @export
find_formula.nlmerMod <- function(x, ...) {
  f.random <- lapply(.findbars(stats::formula(x)), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- .nobars(stats::as.formula(gsub("(.*)(~)(.*)~(.*)", "\\1\\2\\4", .safe_deparse(stats::formula(x)))))
  f.nonlin <- stats::as.formula(paste0("~", .trim(gsub("(.*)~(.*)~(.*)", "\\2", .safe_deparse(stats::formula(x))))))

  f <- .compact_list(list(
    conditional = f.cond,
    nonlinear = f.nonlin,
    random = f.random
  ))
  .find_formula_return(f)
}



#' @export
find_formula.merMod <- function(x, ...) {
  f.cond <- stats::formula(x)
  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  f <- .compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f)
}

#' @export
find_formula.rlmerMod <- find_formula.merMod

#' @export
find_formula.cpglmm <- find_formula.merMod

#' @export
find_formula.glmmadmb <- find_formula.merMod

#' @export
find_formula.mixed <- find_formula.merMod

#' @export
find_formula.clmm <- find_formula.merMod

#' @export
find_formula.cgamm <- find_formula.merMod

#' @export
find_formula.coxme <- find_formula.merMod

#' @export
find_formula.HLfit <- find_formula.merMod

#' @export
find_formula.merModList <- function(x, ...) {
  find_formula(x[[1]], ...)
}



#' @export
find_formula.sem <- function(x, ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  f.cond <- x$formula
  f.random <- lapply(.findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  f <- .compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f)
}


#' @export
find_formula.lme <- function(x, ...) {
  fm <- eval(x$call$fixed)
  fmr <- eval(x$call$random)
  ## TODO this is an intermediate fix to return the correlation variables from lme-objects
  fcorr <- x$call$correlation
  if (!is.null(fcorr)) {
    fc <- parse(text = .safe_deparse(x$call$correlation))[[1]]$form
  } else {
    fc <- NULL
  }

  f <- .compact_list(list(
    conditional = fm,
    random = fmr,
    correlation = stats::as.formula(fc)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.lqmm <- function(x, ...) {
  fm <- eval(x$call$fixed)
  fmr <- .safe_deparse(x$call$random)
  fmg <- .safe_deparse(x$call$group)

  f <- .compact_list(list(
    conditional = fm,
    random = stats::as.formula(paste0(fmr, "|", fmg))
  ))
  .find_formula_return(f)
}



#' @export
find_formula.mixor <- function(x, ...) {
  fm <- x$call$formula

  f_id <- deparse(x$call$id)
  f_rs <- x$call$which.random.slope

  if (!is.null(f_rs)) {
    f_rs <- trimws(unlist(strsplit(.safe_deparse(x$call$formula[[3]]), "\\+")))[f_rs]
    fmr <- paste(f_rs, "|", f_id)
  } else {
    fmr <- f_id
  }

  fmr <- stats::as.formula(paste("~", fmr))

  f <- .compact_list(list(
    conditional = fm,
    random = fmr
  ))
  .find_formula_return(f)
}



#' @export
find_formula.MixMod <- function(x, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, type = "zi_fixed")
  f.random <- stats::formula(x, type = "random")
  f.zirandom <- stats::formula(x, type = "zi_random")

  f <- .compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom
  ))
  .find_formula_return(f)
}



#' @export
find_formula.BBmm <- function(x, ...) {
  f.cond <- parse(text = .safe_deparse(x$call))[[1]]$fixed.formula
  f.rand <- parse(text = .safe_deparse(x$call))[[1]]$random.formula

  f <- .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
  .find_formula_return(f)
}



#' @export
find_formula.mmclogit <- function(x, ...) {
  f <- tryCatch(
    {
      list(
        conditional = stats::formula(x),
        random = stats::as.formula(parse(text = .safe_deparse(x$call))[[1]]$random)
      )
    },
    error = function(x) {
      NULL
    }
  )
  .find_formula_return(f)
}


#' @export
find_formula.glmm <- function(x, ...) {
  f.cond <- stats::as.formula(x$fixedcall)
  f.random <- lapply(x$randcall, function(.x) {
    av <- all.vars(.x)
    stats::as.formula(paste0("~1|", av[length(av)]))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f <- .compact_list(list(conditional = f.cond, random = f.random))
  .find_formula_return(f)
}







# Bayesian models --------------------------------


#' @export
find_formula.BGGM <- function(x, ...) {
  list(conditional = x$formula)
}



#' @export
find_formula.mcmc.list <- function(x, ...) {
  NULL
}



#' @export
find_formula.stanreg <- function(x, ...) {
  if (inherits(x, "nlmerMod")) {
    find_formula.nlmerMod(x, ...)
  } else {
    f.cond <- stats::formula(x)
    # special handling for stan_gamm4
    if (inherits(x, "gamm4")) {
      f.random <- tryCatch(
        {
          lapply(.findbars(stats::formula(x$glmod)), function(.x) {
            f <- .safe_deparse(.x)
            stats::as.formula(paste0("~", f))
          })
        },
        error = function(e) {
          NULL
        }
      )
    } else {
      f.random <- lapply(.findbars(f.cond), function(.x) {
        f <- .safe_deparse(.x)
        stats::as.formula(paste0("~", f))
      })
    }

    if (length(f.random) == 1) {
      f.random <- f.random[[1]]
    }

    f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
    f <- .compact_list(list(conditional = f.cond, random = f.random))
    .find_formula_return(f)
  }
}



#' @export
find_formula.brmsfit <- function(x, ...) {
  f <- stats::formula(x)

  if (.obj_has_name(f, "forms")) {
    mv_formula <- lapply(f$forms, .get_brms_formula)
    attr(mv_formula, "is_mv") <- "1"
    f <- mv_formula
  } else {
    f <- .get_brms_formula(f)
  }
  .find_formula_return(f)
}



#' @export
find_formula.stanmvreg <- function(x, ...) {
  f <- stats::formula(x)
  mv_formula <- lapply(f, .get_stanmv_formula)
  attr(mv_formula, "is_mv") <- "1"
  .find_formula_return(mv_formula)
}



#' @export
find_formula.MCMCglmm <- function(x, ...) {
  fm <- x$Fixed$formula
  fmr <- x$Random$formula

  f <- .compact_list(list(conditional = fm, random = fmr))
  .find_formula_return(f)
}



#' @export
find_formula.BFBayesFactor <- function(x, ...) {
  if (.classify_BFBayesFactor(x) == "linear") {
    fcond <- utils::tail(x@numerator, 1)[[1]]@identifier$formula
    dt <- utils::tail(x@numerator, 1)[[1]]@dataTypes
    frand <- names(dt)[which(dt == "random")]

    if (!.is_empty_object(frand)) {
      f.random <- stats::as.formula(paste0("~", frand))
      fcond <- sub(frand, "", fcond, fixed = TRUE)
      fcond <- gsub("(.*)\\+$", "\\1", .trim(fcond))
      # random effects only?
      if (grepl("~$", fcond)) {
        fcond <- paste(fcond, "1")
      }
      f.cond <- stats::as.formula(.trim(fcond))
    } else {
      f.random <- NULL
      f.cond <- stats::as.formula(fcond)
    }
  } else if (.classify_BFBayesFactor(x) %in% c("ttest1", "ttest2")) {
    f.cond <- tryCatch(
      {
        stats::as.formula(x@numerator[[1]]@identifier$formula)
      },
      error = function(e) {
        NULL
      }
    )
    f.random <- NULL
  } else {
    return(NULL)
  }

  f <- .compact_list(list(
    conditional = f.cond,
    random = f.random
  ))
  .find_formula_return(f)
}





# tidymodels --------------------------------------------------------------

#' @export
find_formula.model_fit <- function(x, ...) {
  find_formula(x$fit, ...)
}





# helper ---------------------------


.get_brms_formula <- function(f) {
  f.cond <- f$formula
  f.random <- lapply(.findbars(f.cond), function(.x) {
    fm <- .safe_deparse(.x)
    stats::as.formula(paste0("~", fm))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))

  f.zi <- f$pforms$zi
  f.zirandom <- NULL

  if (!.is_empty_object(f.zi)) {
    f.zirandom <- lapply(.findbars(f.zi), function(.x) {
      f <- .safe_deparse(.x)
      stats::as.formula(paste0("~", f))
    })

    if (length(f.zirandom) == 1) {
      f.zirandom <- f.zirandom[[1]]
    }

    f.zi <- stats::as.formula(paste0("~", .safe_deparse(f.zi[[3L]])))
    f.zi <- stats::as.formula(.get_fixed_effects(f.zi))
  }

  .compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom
  ))
}




.get_stanmv_formula <- function(f) {
  f.cond <- f
  f.random <- lapply(.findbars(f.cond), function(.x) {
    fm <- .safe_deparse(.x)
    stats::as.formula(paste0("~", fm))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))

  .compact_list(list(
    conditional = f.cond,
    random = f.random
  ))
}



# Find formula for zero-inflated regressions, where
# zero-inflated part is separated by | from count part
.zeroinf_formula <- function(x, separator = "\\|") {
  f <- tryCatch(
    {
      stats::formula(x)
    },
    error = function(x) {
      NULL
    }
  )

  if (is.null(f)) {
    return(NULL)
  }

  f <- .trim(unlist(strsplit(.safe_deparse(f), separator)))

  c.form <- stats::as.formula(f[1])
  if (length(f) == 2) {
    zi.form <- stats::as.formula(paste0("~", f[2]))
  } else {
    zi.form <- NULL
  }

  ## TODO could be extended to all find_formula()

  # fix dot-formulas
  c.form <- .dot_formula(f = c.form, model = x)

  # fix dot-formulas
  zi.form <- tryCatch(
    {
      if (as.character(zi.form[2]) == ".") {
        resp <- .safe_deparse(c.form[2])
        pred <- setdiff(colnames(.get_data_from_env(x)), resp)
        zi.form <- stats::as.formula(paste(resp, "~", paste0(pred, collapse = " + ")))
      }
      zi.form
    },
    error = function(e) {
      zi.form
    }
  )


  f <- .compact_list(list(conditional = c.form, zero_inflated = zi.form))
  .find_formula_return(f)
}



# try to guess "full" formula for dot-abbreviation, e.g.
# lm(mpg ~., data = mtcars)
.dot_formula <- function(f, model) {
  # fix dot-formulas
  tryCatch(
    {
      if (as.character(f[[3]])[1] == ".") {
        resp <- .safe_deparse(f[[2]])
        pred <- setdiff(colnames(.get_data_from_env(model)), resp)
        f <- stats::as.formula(paste(resp, "~", paste0(pred, collapse = " + ")))
      }
      f
    },
    error = function(e) {
      f
    }
  )
}


.fix_gamm_random_effect <- function(x) {
  g_in_terms <- length(x) > 1 && x[length(x)] == "g"
  xr_in_terms <- length(x) > 1 && x[length(x)] == "Xr"
  x <- x[!(grepl("(Xr\\.\\d|g\\.\\d)", x) | x %in% c("Xr", "g"))]
  # exceptions, if random effect is named g
  if (!length(x) && isTRUE(g_in_terms)) {
    x <- "g"
  }
  if (!length(x) && isTRUE(xr_in_terms)) {
    x <- "Xr"
  }
  x
}


.fix_gamm4_random_effect <- function(f) {
  if (inherits(f, "formula")) {
    f <- list(f)
  }
  len <- length(f)
  keep <- sapply(f, function(i) {
    i <- gsub("(~1| | \\|)", "", deparse(i))
    !any(grepl("(Xr\\.\\d|g\\.\\d)", i) | i %in% c("Xr", "g"))
  })
  f <- .compact_list(f[keep])
  # exceptions, if random effect is named Xr
  if (!length(f) && len > 1) {
    f <- list(stats::as.formula("~1 | Xr"))
  }
  f
}



# Helpers and Methods -----------------------------------------------------


.find_formula_return <- function(f) {
  if (is.null(f)) {
    return(NULL)
  }

  .check_formula_for_dollar(f)
  class(f) <- c("insight_formula", class(f))
  f
}


# formulas with $, like "lm(mtcars$mpg ~ mtcars$hp), may cause problems
# in various functions throughout the easystats packages. We warn the user
# here...

.check_formula_for_dollar <- function(f) {
  error_message <- paste(
    "Using `$` in model formulas can produce unexpected results.",
    "Specify your model using the `data` argument instead.",
    sep = "\n  "
  )

  if (any(grepl("\\$", .safe_deparse(f[[1]])))) {
    fc <- try(.formula_clean(f[[1]]), silent = TRUE)
    if (inherits(fc, "try-error")) {
      stop(attributes(fc)$condition$message, call. = FALSE)
    } else {
      warning(
        paste(
          error_message,
          paste0("Try: ", fc$formula, ", data = ", fc$data),
          sep = "\n  "
        ),
        call. = FALSE
      )
    }
  }
}


.formula_clean <- function(f) {
  fc <- as.character(f)
  LHS <- fc[2]
  RHS <- fc[3]

  pattern <- "[\\s*+:()|^,\\-\\/]" # was: "[\\s\\*\\+:\\-\\|/\\(\\)\\^,]"

  parts <- trimws(unlist(strsplit(split = pattern, x = LHS, perl = TRUE)))
  d_LHS <- unique(gsub("(.*)\\$(.*)", "\\1", parts[grepl("(.*)\\$(.*)", parts)]))

  parts <- trimws(unlist(strsplit(split = pattern, x = RHS, perl = TRUE)))
  d_RHS <- unique(gsub("(.*)\\$(.*)", "\\1", parts[grepl("(.*)\\$(.*)", parts)]))

  if (.n_unique(c(d_LHS, d_RHS)) > 1) {
    stop("Multiple data objects present in formula. Specify your model using the `data` argument instead.", call. = FALSE)
  } else {
    d <- unique(d_RHS)
  }
  LHS_clean <- gsub(paste0(d_LHS, "\\$"), "", LHS)
  RHS_clean <- gsub(paste0(d_RHS, "\\$"), "", RHS)

  list(data = d, formula = paste(LHS_clean, fc[1], RHS_clean))
}




# methods -------------------------


#' @export
format.insight_formula <- function(x, what = c("conditional", "random"), ...) {
  # The purpose of this function is to flatten the formula

  # Start by first part (conditional by default)
  ft <- format(x[[1]])

  # Wrap random in brackets
  if ("random" %in% names(x)) {
    x[["random"]] <- paste0("(", format(x[["random"]]), ")")
  }

  # Add all the components
  for (part in what[-1]) {
    if (part %in% names(x)) {
      ft <- paste0(ft, " + ", format(x[[part]]))
    }
  }

  ft
}
