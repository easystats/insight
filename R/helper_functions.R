# small wrapper around this commonly used try-catch
.safe <- function(code, on_error = NULL) {
  if (isTRUE(getOption("easystats_errors", FALSE)) && is.null(on_error)) {
    code
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


# remove values from vector
.remove_values <- function(x, values) {
  to_remove <- x %in% values
  if (any(to_remove)) {
    x <- x[!to_remove]
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
  length(x) == 0 || all(nchar(x) == 0) # nolint
}


# does string contain pattern?
.string_contains <- function(pattern, x) {
  # This is a bit slower - restore if tests fail...
  # pattern <- paste0("\\Q", pattern, "\\E")
  # grepl(pattern, x, perl = TRUE)

  grepl(pattern, x, fixed = TRUE)
}


.remove_namespace_from_string <- function(x) {
  # remove namespace prefixes
  if (grepl("::", x, fixed = TRUE)) {
    # Here's a regular expression pattern in R that removes any word
    # followed by two colons from a string: This pattern matches a word
    # boundary (\\b), followed by one or more word characters (\\w+),
    # followed by two colons (::)
    x <- gsub("\\b\\w+::", "\\2", x)
  }
  x
}


# checks if a brms-models is a multi-membership-model
.is_multi_membership <- function(x) {
  if (!inherits(x, "brmsfit")) {
    return(FALSE)
  }
  re <- find_random(x, split_nested = TRUE, flatten = TRUE)
  any(grepl("^(mmc|mm)\\(", re))
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
  nchar(gsub("(~|\\+|\\*|-|/|:)", "", gsub(" ", "", gsub("\\((.*)\\)", "", f), fixed = TRUE))) == 0 # nolint
}


# extract random effects from formula
.get_model_random <- function(f, model, split_nested = FALSE) {
  is_special <- inherits(
    model,
    c(
      "MCMCglmm", "gee", "LORgee", "mixor", "clmm2", "felm", "feis", "bife",
      "BFBayesFactor", "BBmm", "glimML", "MANOVA", "RM", "cglm", "glmm",
      "glmgee"
    )
  )

  if (identical(safe_deparse(f), "~0") || identical(safe_deparse(f), "~1")) {
    return(NULL)
  }

  re <- sapply(.findbars(f), safe_deparse)

  if (is_special && is_empty_object(re)) {
    re <- all.vars(f[[2L]])
    if (length(re) > 1L) {
      re <- as.list(re)
      split_nested <- FALSE
    }
  } else {
    re <- trim_ws(substring(re, max(gregexpr(pattern = "|", re, fixed = TRUE)[[1]]) + 1))
  }

  # check for multi-membership models
  if (inherits(model, "brmsfit")) {
    if (grepl("mm\\((.*)\\)", re)) {
      # extract variables
      re <- clean_names(re)
    } else if (grepl("gr\\((.*)\\)", re)) {
      # remove namespace prefixes
      re <- .remove_namespace_from_string(re)
      # extract random effects term
      re <- trim_ws(gsub("gr\\((\\w+)(,.*|)\\)", "\\1", re))
    }
  }

  if (split_nested) {
    # remove parenthesis for nested models
    re <- unique(unlist(strsplit(re, ":", fixed = TRUE)))

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
    f <- lapply(f, .get_model_random, model = x, split_nested = TRUE)
  } else {
    f <- .get_model_random(f, model = x, split_nested = TRUE)
  }

  if (is.null(f)) {
    return(NULL)
  }

  if (is.list(f)) {
    f <- lapply(f, sapply, as.symbol)
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
    "dispersion", "dispersion_random", "instruments", "interactions", "simplex",
    "smooth_terms", "sigma", "nu", "tau", "correlation", "slopes", "cluster",
    "extra", "scale", "marginal", "alpha", "beta", "survival", "infrequent_purchase",
    "auxiliary", "mix", "shiftprop", "phi", "ndt", "hu", "xi", "coi", "zoi",
    "aux", "dist", "selection", "outcome", "time_dummies", "sigma_random",
    "beta_random", "car", "nominal", "bidrange", "mu", "kappa", "bias"
  )
}

.aux_elements <- function() {
  c(
    "sigma", "alpha", "beta", "dispersion", "precision", "nu", "tau", "shape",
    "phi", "(phi)", "ndt", "hu", "xi", "coi", "zoi", "mix", "shiftprop", "auxiliary",
    "aux", "dist", "mu", "kappa", "bias",
    # random parameters
    "dispersion_random", "sigma_random", "beta_random"
  )
}

.brms_aux_elements <- function() {
  c(
    "sigma", "mu", "nu", "shape", "beta", "phi", "hu", "ndt", "zoi", "coi",
    "kappa", "bias", "bs", "zi", "alpha", "xi"
  )
}

.get_elements <- function(effects, component, model = NULL) {
  # all elements of a model
  elements <- .all_elements()

  # zero-inflation component
  zero_inflated_component <- c("zi", "zero_inflated", "zero_inflated_random")

  # auxiliary parameters
  auxiliary_parameters <- .aux_elements()

  # random parameters
  random_parameters <- c("random", "zero_inflated_random", "dispersion_random", "sigma_random", "beta_random", "car")

  # conditional component
  conditional_component <- setdiff(
    elements,
    c(auxiliary_parameters, zero_inflated_component, "smooth_terms", "nonlinear")
  )

  # location parameters
  location_parameters <- switch(effects,
    fixed = setdiff(elements, c(auxiliary_parameters, random_parameters)),
    random = intersect(setdiff(elements, auxiliary_parameters), random_parameters),
    setdiff(elements, auxiliary_parameters)
  )

  # fixed pattern?
  if (all(component == "location")) {
    return(location_parameters)
  }

  # fixed pattern?
  if (all(component %in% c("aux", "dist", "distributional", "auxiliary"))) {
    return(auxiliary_parameters)
  }

  # if we have brms-models with custom formulas, we have element-names
  # that are not covered by the standard elements. We then just do not
  # filter elements.
  if (inherits(model, "brmsfit") && component == "all") {
    f <- insight::find_formula(model, verbose = FALSE)
    elements <- unique(c(elements, names(f)))
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
    unlist(sapply(
      parameters,
      function(pattern) {
        grep(pattern = pattern, x = component, perl = TRUE, value = TRUE)
      },
      simplify = FALSE
    ), use.names = FALSE)
  })
}


# remove column
.remove_column <- function(data, variables) {
  data[, -which(colnames(data) %in% variables), drop = FALSE]
}


.grep_smoothers <- function(x) {
  startsWith(x, "s(") |
    startsWith(x, "ti(") |
    startsWith(x, "te(") |
    startsWith(x, "t2(") |
    startsWith(x, "gam::s(") |
    startsWith(x, "VGAM::s(") |
    startsWith(x, "mgcv::s(") |
    startsWith(x, "mgcv::ti(") |
    startsWith(x, "mgcv::t2(") |
    startsWith(x, "mgcv::te(") |
    startsWith(x, "brms::s(") |
    startsWith(x, "brms::t2") |
    startsWith(x, "smooth_sd[")
}


.grep_zi_smoothers <- function(x) {
  # this one captures smoothers in zi- or mv-models from gam
  grepl("^(s\\.\\d\\()", x) | grepl("^(gam::s\\.\\d\\()", x) | grepl("^(mgcv::s\\.\\d\\()", x)
}


.grep_non_smoothers <- function(x) {
  !.grep_smoothers(x) & !.grep_zi_smoothers(x)
}


# .split_formula <- function(f) {
#   rhs <- if (length(f) > 2L)
#     f[[3L]]
#   else
#     f[[2L]]
#
#   lapply(.extract_formula_parts(rhs), safe_deparse)
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


.gam_family <- function(x) {
  faminfo <- .safe(stats::family(x))

  # try to set manually, if not found otherwise
  if (is.null(faminfo)) {
    faminfo <- .safe(x$family)
  }

  faminfo
}


# for models with zero-inflation component, return
# required component of model-summary
.filter_component <- function(dat, component) {
  switch(component,
    cond = ,
    conditional = dat[dat$Component == "conditional", , drop = FALSE],
    zi = ,
    zero_inflated = dat[dat$Component == "zero_inflated", , drop = FALSE],
    dispersion = dat[dat$Component == "dispersion", , drop = FALSE],
    smooth_terms = dat[dat$Component == "smooth_terms", , drop = FALSE],
    ip = ,
    infrequent_purchase = dat[dat$Component == "infrequent_purchase", , drop = FALSE],
    auxiliary = dat[dat$Component == "auxiliary", , drop = FALSE],
    distributional = dat[dat$Component == "distributional", , drop = FALSE],
    sigma = dat[dat$Component == "sigma", , drop = FALSE],
    dat
  )
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


.is_bayesian_model <- function(x, exclude = NULL) {
  bayes_classes <- c(
    "brmsfit", "stanfit", "MCMCglmm", "stanreg", "stanmvreg", "bmerMod",
    "BFBayesFactor", "bamlss", "bayesx", "mcmc", "bcplm", "bayesQR", "BGGM",
    "meta_random", "meta_fixed", "meta_bma", "blavaan", "blrm", "blmerMod",
    "bglmerMod"
  )
  # if exclude is not NULL, remove elements in exclude from bayes_class
  if (!is.null(exclude)) {
    bayes_classes <- bayes_classes[!bayes_classes %in% exclude]
  }
  inherits(x, bayes_classes)
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

  x_no_na <- x[!is.na(x)]
  if (anyNA(suppressWarnings(as.numeric(as.character(x_no_na))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  } else if (is.unsorted(levels(x))) {
    # for numeric factors, we need to check the order of levels
    x_inverse <- rep(NA_real_, length(x))
    for (i in 1:nlevels(x)) {
      x_inverse[x == levels(x)[i]] <- as.numeric(levels(x)[nlevels(x) - i + 1])
    }
    x <- factor(x_inverse)
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
  stats::as.formula(paste("~(", paste(vapply(newtrms, function(trm) { # nolint
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
      if (length(x) < 2L) {
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
        format_error("unparseable formula for grouping factor")
      }
      list(slashTerms(x[[2]]), slashTerms(x[[3]]))
    }
    if (is.list(bb)) {
      unlist(lapply(bb, function(x) {
        trms <- slashTerms(x[[3]])
        if (length(x) > 2 && is.list(trms)) {
          lapply(unlist(makeInteraction(trms)), function(trm) {
            substitute(foo | bar, list(foo = x[[2]], bar = trm))
          })
        } else {
          x
        }
      }))
    } else {
      expandSlash(list(bb))
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
  if (is.call(term) && ((term[[1]] == as.name("|") || term[[1]] == as.name("||")))) {
    return(TRUE)
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

  if (isTRUE(c_)) {
    "contrasts"
  } else if (isTRUE(t_)) {
    "emtrends"
  } else {
    "emmeans"
  }
}
