# remove trailing/leading spaces from character vectors
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)



# remove NULL elements from lists
.compact_list <- function(x) x[!sapply(x, function(i) all(length(i) == 0) || all(is.null(i)) || (!is.data.frame(i) && any(i == "NULL", na.rm = TRUE)) || (is.data.frame(i) && nrow(i) == 0))]



# remove empty string from character
.compact_character <- function(x) x[!sapply(x, function(i) nchar(i) == 0 || is.null(i) || any(i == "NULL", na.rm = TRUE))]



# remove values from vector
.remove_values <- function(x, values) {
  remove <- x %in% values
  if (any(remove)) {
    x <- x[!remove]
  }
  x
}


# rename values in a vector
.rename_values <- function(x, old, new) {
  x[x %in% old] <- new
  x
}



# is string empty?
.is_empty_string <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0 || all(nchar(x) == 0)
}


# is object empty?
.is_empty_object <- function(x) {
  flag_empty <- FALSE
  if (inherits(x, "data.frame")) {
    x <- as.data.frame(x)
    if (nrow(x) > 0 && ncol(x) > 0) {
      x <- x[, !sapply(x, function(i) all(is.na(i))), drop = FALSE]
      # this is much faster than apply(x, 1, FUN)
      flag_empty <- all(rowSums(is.na(x)) == ncol(x))
    } else {
      flag_empty <- TRUE
    }
    # a list but not a data.frame
  } else if (is.list(x) && length(x) > 0) {
    x <- tryCatch(
      {
        .compact_list(x)
      },
      error = function(x) {
        x
      }
    )
  } else if (!is.null(x)) {
    x <- stats::na.omit(x)
  }
  # need to check for is.null for R 3.4
  isTRUE(flag_empty) ||
    length(x) == 0 ||
    is.null(x) ||
    isTRUE(nrow(x) == 0) ||
    isTRUE(ncol(x) == 0)
}



# does string contain pattern?
.string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grepl(pattern, x, perl = TRUE)
}



# has object an element with given name?
.obj_has_name <- function(x, name) {
  name %in% names(x)
}


# checks if a brms-models is a multi-membership-model
.is_multi_membership <- function(x) {
  if (inherits(x, "brmsfit")) {
    re <- find_random(x, split_nested = TRUE, flatten = TRUE)
    any(grepl("^(mmc|mm)\\(", re))
  } else {
    return(FALSE)
  }
}


# merge data frames, remove double columns
.merge_dataframes <- function(data, ..., replace = TRUE) {
  # check for identical column names
  tmp <- cbind(...)

  if (nrow(data) == 0) {
    return(tmp)
  }

  doubles <- colnames(tmp) %in% colnames(data)

  # keep order?
  reihenfolge <- c(which(!doubles), which(doubles))

  # remove duplicate column names, if requested
  if (replace && any(doubles)) tmp <- tmp[, !doubles, drop = FALSE]

  # bind all data
  x <- cbind(tmp, data)

  # restore order
  if (replace) {
    # check for correct length. if "data" had duplicated variables,
    # but not all variable are duplicates, add indices of regular values
    if (ncol(x) > length(reihenfolge)) {
      # get remaining indices
      xl <- seq_len(ncol(x))[-seq_len(length(reihenfolge))]
      # add to "reihefolge"
      reihenfolge <- c(reihenfolge, xl)
    }
    # sort data frame
    x <- x[, order(reihenfolge), drop = FALSE]
  }

  x
}


# removes random effects from a formula that is in lmer-notation
.get_fixed_effects <- function(f) {
  # remove random effects from RHS
  fl <- length(f)
  f[[fl]] <- .nobars(f[[fl]])
  f
}


# check if any terms appear in the formula after random effects
# like "~ (1|school) + open + extro + agree + school"
# this regex removes "(1|school)", as well as any +, -, *, whitespace etc.
# if there are any chars left, these come from further terms that come after
# random effects...
.formula_empty_after_random_effect <- function(f) {
  nchar(gsub("(~|\\+|\\*|-|/|:)", "", gsub(" ", "", gsub("\\((.*)\\)", "", f)))) == 0
}



# extract random effects from formula
.get_model_random <- function(f, split_nested = FALSE, model) {
  is_special <- inherits(
    model,
    c(
      "MCMCglmm", "gee", "LORgee", "mixor", "clmm2", "felm", "feis", "bife",
      "BFBayesFactor", "BBmm", "glimML", "MANOVA", "RM", "cglm", "glmm"
    )
  )

  if (identical(.safe_deparse(f), "~0") || identical(.safe_deparse(f), "~1")) {
    return(NULL)
  }

  re <- sapply(.findbars(f), .safe_deparse)

  if (is_special && .is_empty_object(re)) {
    re <- all.vars(f[[2L]])
    if (length(re) > 1) {
      re <- as.list(re)
      split_nested <- FALSE
    }
  } else {
    re <- .trim(substring(re, max(gregexpr(pattern = "\\|", re)[[1]]) + 1))
  }

  # check for multi-membership models
  if (inherits(model, "brmsfit")) {
    if (grepl("mm\\((.*)\\)", re)) {
      re <- trimws(unlist(strsplit(gsub("mm\\((.*)\\)", "\\1", re), ",")))
    }
  }

  if (split_nested) {
    # remove parenthesis for nested models
    re <- unique(unlist(strsplit(re, "\\:")))

    # nested random effects, e.g. g1 / g2 / g3, deparse to "g0:(g1:g2)".
    # when we split at ":", we have "g0", "(g1" and "g2)". In such cases,
    # we need to remove the parentheses. But we need to preserve them in
    # case we have group factors in other models, like panelr, where we can
    # have "lag(union)" as group factor. In such cases, parentheses should be
    # preserved. We here check if group factors, after passing to "clean_names()",
    # still have "(" or ")" in their name, and if so, just remove parentheses
    # for these cases...

    has_parantheses <- vapply(
      clean_names(re),
      function(i) {
        grepl("[\\(\\)]", x = i)
      },
      logical(1)
    )

    if (any(has_parantheses)) {
      re[has_parantheses] <- gsub(pattern = "[\\(\\)]", replacement = "", x = re[has_parantheses])
    }

    re
  } else {
    unique(re)
  }
}



# in case we need the random effects terms as formula (symbol),
# not as character string, then call this functions instead of
# .get_model_random()
.get_group_factor <- function(x, f) {
  if (is.list(f)) {
    f <- lapply(f, function(.x) {
      .get_model_random(.x, split_nested = TRUE, x)
    })
  } else {
    f <- .get_model_random(f, split_nested = TRUE, x)
  }

  if (is.null(f)) {
    return(NULL)
  }

  if (is.list(f)) {
    f <- lapply(f, function(i) sapply(i, as.symbol))
  } else {
    f <- sapply(f, as.symbol)
  }

  f
}



# helper to access model components ----------------


.all_elements <- function() {
  c(
    "conditional", "conditional1", "conditional2", "conditional3", "precision",
    "nonlinear", "random", "zi", "zero_inflated", "zero_inflated_random", "shape",
    "dispersion", "instruments", "interactions", "simplex", "smooth_terms",
    "sigma", "nu", "tau", "correlation", "slopes", "cluster", "extra", "scale",
    "marginal", "alpha", "beta", "survival", "infrequent_purchase", "auxiliary",
    "mix", "shiftprop", "phi", "ndt", "hu", "xi", "coi", "zoi", "aux", "dist",
    "selection", "outcome", "time_dummies", "sigma_random", "beta_random", "car"
  )
}

.aux_elements <- function() {
  c(
    "sigma", "alpha", "beta", "dispersion", "precision", "nu", "tau", "shape",
    "phi", "(phi)", "ndt", "hu", "xi", "coi", "zoi", "mix", "shiftprop", "auxiliary",
    "aux", "dist",

    # random parameters
    "sigma_random", "beta_random"
  )
}

.get_elements <- function(effects, component) {

  # all elements of a model
  elements <- .all_elements()

  # zero-inflation component
  zero_inflated_component <- c("zi", "zero_inflated", "zero_inflated_random")

  # auxiliary parameters
  auxiliary_parameters <- .aux_elements()

  # random parameters
  random_parameters <- c("random", "zero_inflated_random", "sigma_random", "beta_random", "car")

  # conditional component
  conditional_component <- setdiff(elements, c(auxiliary_parameters, zero_inflated_component, "smooth_terms"))

  # location parameters
  location_parameters <- if (effects == "fixed") {
    setdiff(elements, c(auxiliary_parameters, random_parameters))
  } else {
    setdiff(elements, auxiliary_parameters)
  }

  # fixed pattern?
  if (all(component == "location")) {
    return(location_parameters)
  }

  # fixed pattern?
  if (all(component %in% c("aux", "dist", "distributional", "auxiliary"))) {
    return(auxiliary_parameters)
  }


  elements <- switch(effects,
    all = elements,
    fixed = elements[!elements %in% random_parameters],
    random = elements[elements %in% random_parameters]
  )

  elements <- switch(component,
    all = elements,
    cond = ,
    conditional = elements[elements %in% conditional_component],
    zi = ,
    zero_inflated = elements[elements %in% zero_inflated_component],
    elements[elements == component]
  )

  elements
}




# checks if a mixed model fit is singular or not. Need own function,
# because lme4::isSingular() does not work with glmmTMB
.is_singular <- function(x, vals, tolerance = 1e-5) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  tryCatch(
    {
      if (inherits(x, c("glmmTMB", "clmm", "cpglmm"))) {
        is_si <- any(sapply(vals$vc, function(.x) any(abs(diag(.x)) < tolerance)))
      } else if (inherits(x, "merMod")) {
        theta <- lme4::getME(x, "theta")
        diag.element <- lme4::getME(x, "lower") == 0
        is_si <- any(abs(theta[diag.element]) < tolerance)
      } else if (inherits(x, "MixMod")) {
        vc <- diag(x$D)
        is_si <- any(sapply(vc, function(.x) any(abs(.x) < tolerance)))
      } else if (inherits(x, "lme")) {
        is_si <- any(abs(stats::na.omit(as.numeric(diag(vals$vc))) < tolerance))
      } else {
        is_si <- FALSE
      }

      is_si
    },
    error = function(x) {
      FALSE
    }
  )
}



# Filter parameters from Stan-model fits
.filter_pars <- function(l, parameters = NULL, is_mv = NULL) {
  if (!is.null(parameters)) {
    if (is.null(is_mv)) {
      is_mv <- isTRUE(attr(l, "is_mv", exact = TRUE) == "1")
    }
    if (is_multivariate(l) || is_mv) {
      for (i in names(l)) {
        l[[i]] <- .filter_pars_univariate(l[[i]], parameters)
      }
    } else {
      l <- .filter_pars_univariate(l, parameters)
    }
    if (isTRUE(is_mv)) attr(l, "is_mv") <- "1"
  }

  l
}



.filter_pars_univariate <- function(l, parameters) {
  lapply(l, function(component) {
    unlist(unname(sapply(
      parameters,
      function(pattern) {
        component[grepl(pattern = pattern, x = component, perl = TRUE)]
      },
      simplify = FALSE
    )))
  })
}



# remove column
.remove_column <- function(data, variables) {
  data[, -which(colnames(data) %in% variables), drop = FALSE]
}



.grep_smoothers <- function(x) {
  grepl("^(s\\()", x, perl = TRUE) |
    grepl("^(ti\\()", x, perl = TRUE) |
    grepl("^(te\\()", x, perl = TRUE) |
    grepl("^(t2\\()", x, perl = TRUE) |
    grepl("^(gam::s\\()", x, perl = TRUE) |
    grepl("^(VGAM::s\\()", x, perl = TRUE) |
    grepl("^(mgcv::s\\()", x, perl = TRUE) |
    grepl("^(mgcv::ti\\()", x, perl = TRUE) |
    grepl("^(mgcv::t2\\()", x, perl = TRUE) |
    grepl("^(mgcv::te\\()", x, perl = TRUE) |
    grepl("^(brms::s\\()", x, perl = TRUE) |
    grepl("^(brms::t2\\()", x, perl = TRUE) |
    grepl("^(smooth_sd\\[)", x, perl = TRUE)
}



.grep_zi_smoothers <- function(x) {
  grepl("^(s\\.\\d\\()", x, perl = TRUE) |
    grepl("^(gam::s\\.\\d\\()", x, perl = TRUE) |
    grepl("^(mgcv::s\\.\\d\\()", x, perl = TRUE)
}



.grep_non_smoothers <- function(x) {
  grepl("^(?!(s\\())", x, perl = TRUE) &
    # this one captures smoothers in zi- or mv-models from gam
    grepl("^(?!(s\\.\\d\\())", x, perl = TRUE) &
    grepl("^(?!(ti\\())", x, perl = TRUE) &
    grepl("^(?!(te\\())", x, perl = TRUE) &
    grepl("^(?!(t2\\())", x, perl = TRUE) &
    grepl("^(?!(gam::s\\())", x, perl = TRUE) &
    grepl("^(?!(gam::s\\.\\d\\())", x, perl = TRUE) &
    grepl("^(?!(VGAM::s\\())", x, perl = TRUE) &
    grepl("^(?!(mgcv::s\\())", x, perl = TRUE) &
    grepl("^(?!(mgcv::s\\.\\d\\())", x, perl = TRUE) &
    grepl("^(?!(mgcv::ti\\())", x, perl = TRUE) &
    grepl("^(?!(mgcv::te\\())", x, perl = TRUE) &
    grepl("^(?!(brms::s\\())", x, perl = TRUE) &
    grepl("^(?!(brms::t2\\())", x, perl = TRUE) &
    grepl("^(?!(smooth_sd\\[))", x, perl = TRUE)
}



# .split_formula <- function(f) {
#   rhs <- if (length(f) > 2)
#     f[[3L]]
#   else
#     f[[2L]]
#
#   lapply(.extract_formula_parts(rhs), .safe_deparse)
# }
#
#
# .extract_formula_parts <- function(x, sep = "|") {
#   if (is.null(x))
#     return(NULL)
#   rval <- list()
#   if (length(x) > 1L && x[[1L]] == sep) {
#     while (length(x) > 1L && x[[1L]] == sep) {
#       rval <- c(x[[3L]], rval)
#       x <- x[[2L]]
#     }
#   }
#   c(x, rval)
# }



.safe_deparse <- function(string) {
  if (is.null(string)) {
    return(NULL)
  }
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = " ")
}



.gam_family <- function(x) {
  faminfo <- tryCatch(
    {
      stats::family(x)
    },
    error = function(e) {
      NULL
    }
  )

  # try to set manually, if not found otherwise
  if (is.null(faminfo)) {
    faminfo <- tryCatch(
      {
        x$family
      },
      error = function(e) {
        NULL
      }
    )
  }

  faminfo
}



# for models with zero-inflation component, return
# required component of model-summary
.filter_component <- function(dat, component) {
  switch(component,
    "cond" = ,
    "conditional" = dat[dat$Component == "conditional", , drop = FALSE],
    "zi" = ,
    "zero_inflated" = dat[dat$Component == "zero_inflated", , drop = FALSE],
    "dispersion" = dat[dat$Component == "dispersion", , drop = FALSE],
    "smooth_terms" = dat[dat$Component == "smooth_terms", , drop = FALSE],
    "ip" = ,
    "infrequent_purchase" = dat[dat$Component == "infrequent_purchase", , drop = FALSE],
    "auxiliary" = dat[dat$Component == "auxiliary", , drop = FALSE],
    "distributional" = dat[dat$Component == "distributional", , drop = FALSE],
    "sigma" = dat[dat$Component == "sigma", , drop = FALSE],
    dat
  )
}




# capitalizes the first letter in a string
.capitalize <- function(x) {
  capped <- grep("^[A-Z]", x, invert = TRUE)
  substr(x[capped], 1, 1) <- toupper(substr(x[capped], 1, 1))
  x
}



.remove_backticks_from_parameter_names <- function(x) {
  if (is.data.frame(x) && "Parameter" %in% colnames(x)) {
    x$Parameter <- gsub("`", "", x$Parameter, fixed = TRUE)
  }
  x
}


.remove_backticks_from_string <- function(x) {
  if (is.character(x)) {
    x <- gsub("`", "", x, fixed = TRUE)
  }
  x
}


.remove_backticks_from_matrix_names <- function(x) {
  if (is.matrix(x)) {
    colnames(x) <- gsub("`", "", colnames(x), fixed = TRUE)
    rownames(x) <- gsub("`", "", colnames(x), fixed = TRUE)
  }
  x
}





#' @keywords internal
.gather <- function(x,
                    names_to = "key",
                    values_to = "value",
                    columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]

  dat <- stats::reshape(
    x,
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]])) {
    dat[[values_to]] <- as.character(dat[[values_to]])
  }

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}



.is_baysian_emmeans <- function(x) {
  if (inherits(x, "emm_list")) {
    x <- x[[1]]
  }
  post.beta <- methods::slot(x, "post.beta")
  !(all(dim(post.beta) == 1) && is.na(post.beta))
}



.is_bayesian_model <- function(x) {
  inherits(x, c(
    "brmsfit", "stanfit", "MCMCglmm", "stanreg",
    "stanmvreg", "bmerMod", "BFBayesFactor", "bamlss",
    "bayesx", "mcmc", "bcplm", "bayesQR", "BGGM",
    "meta_random", "meta_fixed", "meta_bma", "blavaan",
    "blrm"
  ))
}



# safe conversion from factor to numeric
.factor_to_numeric <- function(x, lowest = NULL) {
  if (is.data.frame(x)) {
    as.data.frame(lapply(x, .factor_to_numeric_helper, lowest = lowest))
  } else {
    .factor_to_numeric_helper(x, lowest = lowest)
  }
}


.factor_to_numeric_helper <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }

  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }

  out <- as.numeric(as.character(x))

  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }

  out
}





## copied from lme4::findbars() -----------------------


.expandDoubleVert <- function(term) {
  frml <- stats::formula(substitute(~x, list(x = term[[2]])))
  newtrms <- paste0("0+", attr(stats::terms(frml), "term.labels"))
  if (attr(stats::terms(frml), "intercept") != 0) {
    newtrms <- c("1", newtrms)
  }
  stats::as.formula(paste("~(", paste(vapply(newtrms, function(trm) {
    paste0(trm, "|", deparse(term[[3]]))
  }, ""), collapse = ")+("), ")"))[[2]]
}



.expandDoubleVerts <- function(term) {
  if (!is.name(term) && is.language(term)) {
    if (term[[1]] == as.name("(")) {
      term[[2]] <- .expandDoubleVerts(term[[2]])
    }
    stopifnot(is.call(term))
    if (term[[1]] == as.name("||")) {
      return(.expandDoubleVert(term))
    }
    term[[2]] <- .expandDoubleVerts(term[[2]])
    if (length(term) != 2 && length(term) == 3) {
      term[[3]] <- .expandDoubleVerts(term[[3]])
    }
  }
  term
}



.findbars <- function(term) {
  fb <- function(term) {
    if (is.name(term) || !is.language(term)) {
      return(NULL)
    }
    if (term[[1]] == as.name("(")) {
      return(fb(term[[2]]))
    }
    stopifnot(is.call(term))
    if (term[[1]] == as.name("|")) {
      return(term)
    }
    if (length(term) == 2) {
      return(fb(term[[2]]))
    }
    c(fb(term[[2]]), fb(term[[3]]))
  }

  expandSlash <- function(bb) {
    makeInteraction <- function(x) {
      if (length(x) < 2) {
        return(x)
      }
      trm1 <- makeInteraction(x[[1]])
      trm11 <- if (is.list(trm1)) {
        trm1[[1]]
      } else {
        trm1
      }
      list(substitute(foo:bar, list(foo = x[[2]], bar = trm11)), trm1)
    }
    slashTerms <- function(x) {
      if (!("/" %in% all.names(x))) {
        return(x)
      }
      if (x[[1]] != as.name("/")) {
        stop("unparseable formula for grouping factor", call. = FALSE)
      }
      list(slashTerms(x[[2]]), slashTerms(x[[3]]))
    }
    if (!is.list(bb)) {
      expandSlash(list(bb))
    } else {
      unlist(lapply(bb, function(x) {
        if (length(x) > 2 && is.list(trms <- slashTerms(x[[3]]))) {
          lapply(unlist(makeInteraction(trms)), function(trm) {
            substitute(foo | bar, list(foo = x[[2]], bar = trm))
          })
        } else {
          x
        }
      }))
    }
  }
  modterm <- .expandDoubleVerts(if (methods::is(term, "formula")) {
    term[[length(term)]]
  } else {
    term
  })
  expandSlash(fb(modterm))
}




## copied from lme4::nobars() -----------------------


.nobars <- function(term) {
  nb <- .nobars_(term)
  if (methods::is(term, "formula") && length(term) == 3 && is.symbol(nb)) {
    nb <- stats::reformulate("1", response = deparse(nb))
  }
  if (is.null(nb)) {
    nb <- if (methods::is(term, "formula")) {
      ~1
    } else {
      1
    }
  }
  nb
}



.nobars_ <- function(term) {
  if (!(any(c("|", "||") %in% all.names(term)))) {
    return(term)
  }

  if (.isBar(term)) {
    return(NULL)
  }

  if (.isAnyArgBar(term)) {
    return(NULL)
  }

  if (length(term) == 2) {
    nb <- .nobars_(term[[2]])
    if (is.null(nb)) {
      return(NULL)
    }
    term[[2]] <- nb
    return(term)
  }

  nb2 <- .nobars_(term[[2]])
  nb3 <- .nobars_(term[[3]])

  if (is.null(nb2)) {
    return(nb3)
  }

  if (is.null(nb3)) {
    return(nb2)
  }

  term[[2]] <- nb2
  term[[3]] <- nb3
  term
}



.isBar <- function(term) {
  if (is.call(term)) {
    if ((term[[1]] == as.name("|")) || (term[[1]] == as.name("||"))) {
      return(TRUE)
    }
  }
  FALSE
}


.isAnyArgBar <- function(term) {
  if ((term[[1]] != as.name("~")) && (term[[1]] != as.name("("))) {
    for (i in seq_along(term)) {
      if (.isBar(term[[i]])) {
        return(TRUE)
      }
    }
  }
  FALSE
}



.n_unique <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  length(unique(x))
}




# classify emmeans objects -------------


is.emmeans.contrast <- function(x) {
  if (inherits(x, "list")) {
    out <- vector("list", length = length(x))
    for (i in seq_along(x)) {
      out[[i]] <- is.emmeans.contrast(x[[i]])
    }
    return(unlist(out))
  }

  res <- "con.coef" %in% names(x@misc)
  rep(res, nrow(x@linfct))
}


is.emmeans.trend <- function(x) {
  if (inherits(x, "list")) {
    out <- vector("list", length = length(x))
    for (i in seq_along(x)) {
      out[[i]] <- is.emmeans.trend(x[[i]])
    }
    return(unlist(out))
  }

  "trend" %in% names(x@roles) & !is.emmeans.contrast(x)
}


is.emmean <- function(x) {
  !is.emmeans.trend(x) & !is.emmeans.contrast(x)
}


.classify_emmeans <- function(x) {
  c_ <- is.emmeans.contrast(x)
  t_ <- is.emmeans.trend(x)

  ifelse(c_, "contrasts",
    ifelse(t_, "emtrends", "emmeans")
  )
}
