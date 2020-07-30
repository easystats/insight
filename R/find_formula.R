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
#'      \item \code{instruments}, for fixed-effects regressions like \code{ivreg}, \code{felm} or \code{plm}, the instrumental variables
#'      \item \code{cluster}, for fixed-effects regressions like \code{felm}, the cluster specification
#'      \item \code{correlation}, for models with correlation-component like \code{gls}, the formula that describes the correlation structure
#'      \item \code{slopes}, for fixed-effects individual-slope models like \code{feis}, the formula for the slope parameters
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
#' @importFrom stats formula terms as.formula
#' @export
find_formula <- function(x, ...) {
  UseMethod("find_formula")
}



# Default method -----------------------------------


#' @export
find_formula.default <- function(x, ...) {
  if (inherits(x, "list") && .obj_has_name(x, "gam")) {
    x <- x$gam
    class(x) <- c(class(x), c("glm", "lm"))
  }

  tryCatch(
    {
      list(conditional = stats::formula(x))
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
find_formula.data.frame <- function(x, ...) {
  stop("A data frame is no valid object for this function")
}



#' @export
find_formula.aovlist <- function(x, ...) {
  f <- attr(x, "terms", exact = TRUE)
  attributes(f) <- NULL
  list(conditional = f)
}








# GAM -----------------------------------------------------------


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

  f
}



#' @export
find_formula.gamlss <- function(x, ...) {
  tryCatch(
    {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("To use this function, please install package 'lme4'.")
      }
      f.random <- lapply(lme4::findbars(x$mu.formula), function(.x) {
        f <- .safe_deparse(.x)
        stats::as.formula(paste0("~", f))
      })

      if (length(f.random) == 1) {
        f.random <- f.random[[1]]
      }

      .compact_list(list(
        conditional = stats::as.formula(.get_fixed_effects(x$mu.formula)),
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
}



#' @importFrom stats as.formula
#' @export
find_formula.bamlss <- function(x, ...) {
  f <- stats::formula(x)

  .compact_list(list(
    conditional = stats::as.formula(.safe_deparse(f$mu$formula)),
    sigma = stats::as.formula(paste0("~", as.character(f$sigma$formula)[3]))
  ))
}



#' @export
find_formula.gamm <- function(x, ...) {
  x <- x$gam
  class(x) <- c(class(x), c("glm", "lm"))
  NextMethod()
}






# Other models ----------------------------------------------


#' @export
find_formula.glht <- function(x, ...) {
  list(conditional = stats::formula(x$model))
}


#' @export
find_formula.betareg <- function(x, ...) {
  f <- stats::formula(x)
  fs <- .safe_deparse(f)

  if (grepl("|", fs, fixed = TRUE)) {
    fs <- trimws(unlist(strsplit(fs, "|", fixed = TRUE)))
    list(conditional = stats::as.formula(fs[1]),
         precision = stats::as.formula(paste0("~", fs[2])))
  } else {
    list(conditional = f)
  }
}


#' @export
find_formula.rma <- function(x, ...) {
  NULL
}

#' @export
find_formula.metaplus <- find_formula.rma



#' @export
find_formula.afex_aov <- function(x, ...) {
  if ("aov" %in% names(x)) {
    find_formula(x$aov)
  } else {
    find_formula(x$lm)
  }
}


#' @export
find_formula.gee <- function(x, ...) {
  tryCatch(
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
}



#' @export
find_formula.MANOVA <- function(x, ...) {
  .compact_list(list(
    conditional = x$input$formula,
    random = stats::as.formula(paste0("~", x$input$subject))
  ))
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

  .compact_list(l)
}



#' @export
find_formula.LORgee <- function(x, ...) {
  tryCatch(
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
}



#' @export
find_formula.cglm <- function(x, ...) {
  tryCatch(
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
  tryCatch(
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
}



#' @export
find_formula.iv_robust <- function(x, ...) {
  tryCatch(
    {
      f <- .safe_deparse(stats::formula(x))
      cond <- .trim(gsub("(.*)\\+(\\s)*\\((.*)\\)", "\\1", f))
      instr <- .trim(gsub("(.*)\\((.*)\\)", "\\2", f))

      list(
        conditional = stats::as.formula(cond),
        instruments = stats::as.formula(paste0("~", instr))
      )
    },
    error = function(x) {
      NULL
    }
  )
}



#' @export
find_formula.plm <- function(x, ...) {
  tryCatch(
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
}



#' @export
find_formula.felm <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))

  f.cond <- .trim(f_parts[1])

  if (length(f_parts) > 1) {
    f.rand <- paste0("~", .trim(f_parts[2]))
  } else {
    f.rand <- NULL
  }

  if (length(f_parts) > 2) {
    f.instr <- paste0("~", .trim(f_parts[3]))
  } else {
    f.instr <- NULL
  }

  if (length(f_parts) > 3) {
    f.clus <- paste0("~", .trim(f_parts[4]))
  } else {
    f.clus <- NULL
  }

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
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

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    cluster = stats::as.formula(f.clus)
  ))
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

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    cluster = stats::as.formula(f.clus)
  ))
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

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    slopes = stats::as.formula(f.slopes),
    random = stats::as.formula(paste0("~", id))
  ))
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

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
}



#' @export
find_formula.wbm <- function(x, ...) {
  f <- .safe_deparse(stats::formula(x))
  f_parts <- unlist(strsplit(f, "(?<!\\()\\|(?![\\w\\s\\+\\(~]*[\\)])", perl = TRUE))
  # .split_formula(as.formula(f))

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
      # via "lme4::findbars()"
      if (length(gregexpr("\\|", f_parts[3])[[1]]) > 1) {
        if (!requireNamespace("lme4", quietly = TRUE)) {
          stop("To use this function, please install package 'lme4'.")
        }
        f.rand <- lme4::findbars(stats::as.formula(paste("~", f_parts[3])))
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

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    instruments = stats::as.formula(f.instr),
    interactions = stats::as.formula(f.clint),
    random = f.rand
  ))
}

#' @export
find_formula.wbgee <- find_formula.wbm


#' @export
find_formula.glimML <- function(x, ...) {
  .compact_list(list(
    conditional = x@formula,
    random = x@random
  ))
}



#' @export
find_formula.tobit <- function(x, ...) {
  tryCatch(
    {
      list(conditional = parse(text = .safe_deparse(x$call))[[1]]$formula)
    },
    error = function(x) {
      NULL
    }
  )
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


#' @importFrom stats as.formula
#' @export
find_formula.clmm2 <- function(x, ...) {
  .compact_list(list(
    conditional = stats::as.formula(.safe_deparse(attr(x$location, "terms", exact = TRUE))),
    scale = stats::as.formula(.safe_deparse(attr(x$scale, "terms", exact = TRUE))),
    random = stats::as.formula(paste0("~", parse(text = .safe_deparse(x$call))[[1]]$random))
  ))
}


#' @importFrom stats formula
#' @export
find_formula.clm2 <- function(x, ...) {
  .compact_list(list(
    conditional = stats::formula(attr(x$location, "terms", exact = TRUE)),
    scale = stats::formula(attr(x$scale, "terms", exact = TRUE))
  ))
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

  out
}












# Mixed models -----------------------


#' @export
find_formula.glmmTMB <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

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


  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.zirandom <- lapply(lme4::findbars(f.zi), function(.x) {
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

  .compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom,
    dispersion = f.disp
  ))
}



#' @export
find_formula.nlmerMod <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.random <- lapply(lme4::findbars(stats::formula(x)), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- lme4::nobars(stats::as.formula(gsub("(.*)(~)(.*)~(.*)", "\\1\\2\\4", .safe_deparse(stats::formula(x)))))
  f.nonlin <- stats::as.formula(paste0("~", .trim(gsub("(.*)~(.*)~(.*)", "\\2", .safe_deparse(stats::formula(x))))))

  .compact_list(list(
    conditional = f.cond,
    nonlinear = f.nonlin,
    random = f.random
  ))
}



#' @export
find_formula.merMod <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- stats::formula(x)
  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  .compact_list(list(conditional = f.cond, random = f.random))
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
find_formula.sem <- function(x, ...) {
  if (!.is_semLme(x)) {
    return(NULL)
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- x$formula
  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
    f <- .safe_deparse(.x)
    stats::as.formula(paste0("~", f))
  })

  if (length(f.random) == 1) {
    f.random <- f.random[[1]]
  }

  f.cond <- stats::as.formula(.get_fixed_effects(f.cond))
  .compact_list(list(conditional = f.cond, random = f.random))
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

  .compact_list(list(
    conditional = fm,
    random = fmr,
    correlation = stats::as.formula(fc)
  ))
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

  .compact_list(list(
    conditional = fm,
    random = fmr
  ))
}



#' @export
find_formula.MixMod <- function(x, ...) {
  f.cond <- stats::formula(x)
  f.zi <- stats::formula(x, type = "zi_fixed")
  f.random <- stats::formula(x, type = "random")
  f.zirandom <- stats::formula(x, type = "zi_random")

  .compact_list(list(
    conditional = f.cond,
    random = f.random,
    zero_inflated = f.zi,
    zero_inflated_random = f.zirandom
  ))
}



#' @export
find_formula.BBmm <- function(x, ...) {
  f.cond <- parse(text = .safe_deparse(x$call))[[1]]$fixed.formula
  f.rand <- parse(text = .safe_deparse(x$call))[[1]]$random.formula

  .compact_list(list(
    conditional = stats::as.formula(f.cond),
    random = stats::as.formula(f.rand)
  ))
}



#' @export
find_formula.mmclogit <- function(x, ...) {
  tryCatch(
    {
      list(
        conditional = stats::formula(x),
        random = as.formula(parse(text = .safe_deparse(x$call))[[1]]$random)
      )
    },
    error = function(x) {
      NULL
    }
  )
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

  .compact_list(list(conditional = f.cond, random = f.random))
}







# Bayesian models --------------------------------


#' @export
find_formula.BGGM <- function(x, ...) {
  list(conditional = x$formula)
}



#' @export
find_formula.stanreg <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  if (inherits(x, "nlmerMod")) {
    find_formula.nlmerMod(x, ...)
  } else {
    f.cond <- stats::formula(x)
    f.random <- lapply(lme4::findbars(f.cond), function(.x) {
      f <- .safe_deparse(.x)
      stats::as.formula(paste0("~", f))
    })

    if (length(f.random) == 1) {
      f.random <- f.random[[1]]
    }

    f.cond <- stats::as.formula(.get_fixed_effects(f.cond))

    .compact_list(list(conditional = f.cond, random = f.random))
  }
}



#' @export
find_formula.brmsfit <- function(x, ...) {
  f <- stats::formula(x)

  if (.obj_has_name(f, "forms")) {
    mv_formula <- lapply(f$forms, .get_brms_formula)
    attr(mv_formula, "is_mv") <- "1"
    mv_formula
  } else {
    .get_brms_formula(f)
  }
}



#' @export
find_formula.stanmvreg <- function(x, ...) {
  f <- stats::formula(x)
  mv_formula <- lapply(f, .get_stanmv_formula)
  attr(mv_formula, "is_mv") <- "1"
  mv_formula
}



#' @export
find_formula.MCMCglmm <- function(x, ...) {
  fm <- x$Fixed$formula
  fmr <- x$Random$formula

  .compact_list(list(conditional = fm, random = fmr))
}



#' @importFrom utils tail
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
      f.cond <- stats::as.formula(.trim(fcond))
    } else {
      f.random <- NULL
      f.cond <- stats::as.formula(fcond)
    }
  } else {
    return(NULL)
  }

  .compact_list(list(
    conditional = f.cond,
    random = f.random
  ))
}









# helper ---------------------------


.get_brms_formula <- function(f) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- f$formula
  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
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
    f.zirandom <- lapply(lme4::findbars(f.zi), function(.x) {
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
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  f.cond <- f
  f.random <- lapply(lme4::findbars(f.cond), function(.x) {
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


  .compact_list(list(conditional = c.form, zero_inflated = zi.form))
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
