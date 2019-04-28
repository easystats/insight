# remove trailing/leading spaces from character vectors
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# remove NULL elements from lists
compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]

# is string empty?
is_empty_string <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0 || all(nchar(x) == 0)
}

# is string empty?
is_empty_object <- function(x) {
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}

# does string contain pattern?
string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grepl(pattern, x, perl = TRUE)
}

# has object an element with given name?
obj_has_name <- function(x, name) {
  name %in% names(x)
}

# merge data frames, remove double columns
merge_dataframes <- function(data, ..., replace = TRUE) {
  # check for identical column names
  tmp <- cbind(...)

  if (nrow(data) == 0) return(tmp)

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
    x <- x[, order(reihenfolge)]
  }

  x
}


# removes random effects from a formula that which is in lmer-notation
get_fixed_effects <- function(f) {
  trim(gsub("\\+(\\s)*\\((.*)\\)", "", deparse(f, width.cutoff = 500)))
}


# extract random effects from formula
get_model_random <- function(f, split_nested = FALSE, model) {

  is_special <- inherits(model, c("MCMCglmm", "gee", "LORgee", "felm", "feis", "BFBayesFactor"))

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("To use this function, please install package 'lme4'.")
  }

  if (identical(deparse(f, width.cutoff = 500), "~0") ||
    identical(deparse(f, width.cutoff = 500), "~1")) {
    return(NULL)
  }

  re <- sapply(lme4::findbars(f), deparse, width.cutoff = 500)

  if (is_special && is_empty_object(re)) {
    re <- all.vars(f[[2L]])
    if (length(re) > 1) {
      re <- as.list(re)
      split_nested <- FALSE
    }
  } else {
    re <- trim(substring(re, max(gregexpr(pattern = "\\|", re)[[1]]) + 1))
  }

  if (split_nested) {
    unique(unlist(strsplit(re, "\\:")))
  } else {
    unique(re)
  }
}


# in case we need the random effects terms as formula (symbol),
# not as character string, then call this functions instead of
# get_model_random()
#' @keywords internal
get_group_factor <- function(x, f) {
  if (is.list(f)) {
    f <- lapply(f, function(.x) {
      get_model_random(.x, split_nested = TRUE, x)
    })
  } else {
    f <- get_model_random(f, split_nested = TRUE, x)
  }

  if (is.null(f)) return(NULL)

  if (is.list(f)) {
    f <- lapply(f, as.symbol)
  } else {
    f <- as.symbol(f)
  }

  f
}


# to reduce redundant code, I extract this part which is used several
# times accross this package
#' @keywords internal
.get_elements <- function(effects, component) {
  elements <- c("conditional", "random", "zero_inflated", "zero_inflated_random", "dispersion", "instruments", "simplex", "smooth_terms", "sigma", "nu", "tau", "correlation", "slopes")

  elements <- switch(
    effects,
    all = elements,
    fixed = elements[elements %in% c("conditional", "zero_inflated", "dispersion", "instruments", "simplex", "smooth_terms", "correlation", "slopes")],
    random = elements[elements %in% c("random", "zero_inflated_random")]
  )

  elements <- switch(
    component,
    all = elements,
    conditional = elements[elements %in% c("conditional", "random", "slopes")],
    zi = ,
    zero_inflated = elements[elements %in% c("zero_inflated", "zero_inflated_random")],
    dispersion = elements[elements == "dispersion"],
    instruments = elements[elements == "instruments"],
    simplex = elements[elements == "simplex"],
    smooth_terms = elements[elements == "smooth_terms"],
    correlation = elements[elements == "correlation"],
    slopes = elements[elements == "slopes"]
  )

  elements
}


# return data from a data frame that is in the environment,
# and subset the data, if necessary
#' @keywords internal
.get_data_from_env <- function(x) {
  dat <- eval(x$call$data, envir = parent.frame())
  if (obj_has_name(x$call, "subset")) {
    dat <- subset(dat, subset = eval(x$call$subset))
  }

  dat
}


# checks if a mixed model fit is singular or not. Need own function,
# because lme4::isSingular() does not work with glmmTMB
#' @importFrom stats na.omit
#' @keywords internal
.is_singular <- function(x, vals, tolerance = 1e-5) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute variances for mixed models.", call. = FALSE)
  }

  tryCatch(
    {
      if (inherits(x, "glmmTMB")) {
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
    error = function(x) { FALSE }
  )
}


# Filter parameters from Stan-model fits
#' @keywords internal
.filter_pars <- function(l, parameters = NULL) {
  if (!is.null(parameters)) {
    is_mv <- attr(l, "is_mv", exact = TRUE)
    if (is_multivariate(l)) {
      for (i in names(l)) {
        l[[i]] <- .filter_pars_univariate(l[[i]], parameters)
      }
    } else {
      l <- .filter_pars_univariate(l, parameters)
    }
    attr(l, "is_mv") <- is_mv
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
#' @keywords internal
.remove_column <- function(data, variables) {
  data[, -which(colnames(data) %in% variables), drop = FALSE]
}


.grep_smoothers <- function(x) {
  grepl("^(s\\()", x, perl = TRUE) |
    grepl("^(gam::s\\()", x, perl = TRUE) |
    grepl("^(VGAM::s\\()", x, perl = TRUE) |
    grepl("^(mgcv::s\\()", x, perl = TRUE) |
    grepl("^(brms::s\\()", x, perl = TRUE) |
    grepl("^(smooth_sd\\[)", x, perl = TRUE)
}


.grep_non_smoothers <- function(x) {
  grepl("^(?!(s\\())", x, perl = TRUE) &
    grepl("^(?!(gam::s\\())", x, perl = TRUE) &
    grepl("^(?!(VGAM::s\\())", x, perl = TRUE) &
    grepl("^(?!(mgcv::s\\())", x, perl = TRUE) &
    grepl("^(?!(brms::s\\())", x, perl = TRUE) &
    grepl("^(?!(smooth_sd\\[))", x, perl = TRUE)
}