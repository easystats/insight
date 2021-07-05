#' @title Get summary of priors used for a model
#' @name get_priors
#'
#' @description Provides a summary of the prior distributions used
#'   for the parameters in a given model.
#'
#' @param x A Bayesian model.
#' @param verbose Toggle warnings and messages.
#' @param ... Currently not used.
#'
#' @return A data frame with a summary of the prior distributions used
#'   for the parameters in a given model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' get_priors(model)
#' }
#'
#' @export
get_priors <- function(x, ...) {
  UseMethod("get_priors")
}



# =========================================================================
# RSTANARM ----------------------------------------------------------------
# =========================================================================



#' @export
get_priors.stanreg <- function(x, verbose = TRUE, ...) {
  # installed?
  check_if_installed("rstanarm")

  ps <- rstanarm::prior_summary(x)

  l <- .compact_list(lapply(ps[c("prior_intercept", "prior")], function(.x) {
    if (!is.null(.x)) {
      # quick and dirty fix for flat priors
      # else, .compact_list() will set this item as "NA"
      if (is.na(.x$dist)) {
        .x$dist <- "uniform"
        .x$location <- 0
        .x$scale <- 0
        .x$adjusted_scale <- 0
      }
      .x <- do.call(cbind, .x)
      as.data.frame(.x)
    }
  }))

  # find all column names, add missing columns manually, so merge() works
  cn <- unique(unlist(lapply(l, colnames)))
  l <- lapply(l, function(.x) {
    missing <- setdiff(cn, colnames(.x))
    if (length(missing)) {
      .x[missing] <- NA
    }
    .x
  })

  if (length(l) > 1) {
    prior_info <- do.call(rbind, l)
  } else {
    cn <- colnames(l[[1]])
    prior_info <- as.data.frame(l)
    colnames(prior_info) <- cn
  }

  # fix parameters for flat priors here
  flat <- which(prior_info$dist == "uniform")
  if (length(flat) > 0) {
    prior_info$location[flat] <- NA
    prior_info$scale[flat] <- NA
    prior_info$adjusted_scale[flat] <- NA
  }


  params <- find_parameters(x, parameters = "^(?!(R2|log-fit_ratio))")$conditional

  # this is a particular fix for the "R2" prior, which conveys prior
  # information about *all* the parameters. In this case, number of
  # parameters doesn't match number of priors

  if (length(params) != nrow(prior_info)) {
    if (length(params) == 1) {
      prior_info$parameter <- "(Intercept)"
    } else if ("R2" %in% prior_info$dist) {
      prior_info$parameter <- prior_info$dist
      prior_info$parameter[prior_info$dist != "R2"] <- "(Intercept)"
    }
  } else {
    prior_info$parameter <- params
  }
  prior_info <- prior_info[, intersect(c("parameter", "dist", "location", "scale", "adjusted_scale"), colnames(prior_info))]

  colnames(prior_info) <- gsub("dist", "distribution", colnames(prior_info))
  colnames(prior_info) <- gsub("df", "DoF", colnames(prior_info))

  priors <- as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x)) {
      as.numeric(as.character(x))
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)

  string <- strsplit(names(priors), "_", fixed = TRUE)
  string <- lapply(string, .capitalize)
  names(priors) <- unlist(lapply(string, paste0, collapse = "_"))

  priors
}


#' @export
get_priors.stanmvreg <- function(x, ...) {
  # installed?
  check_if_installed("rstanarm")


  ps <- rstanarm::prior_summary(x)

  l <- .compact_list(lapply(ps[c("prior_intercept", "prior")], function(.x) {
    lapply(.x, function(.i) {
      if (!is.null(.i)) do.call(cbind, .i)
    })
  }))

  prior_info <- do.call(rbind, lapply(l, function(.x) {
    if (length(.x) > 1) {
      out <- lapply(names(.x), function(.i) {
        if (!("adjusted_scale" %in% colnames(.x[[.i]]))) .x[[.i]] <- cbind(.x[[.i]], adjusted_scale = NA)
        data.frame(.x[[.i]], response = .i, stringsAsFactors = FALSE)
      })
      do.call(rbind, out)
    } else {
      cn <- colnames(.x[[1]])
      prior_info <- as.data.frame(.x)
      colnames(prior_info) <- cn
    }
  }))

  # find parameter names
  params <- unlist(lapply(find_parameters(x), function(.i) .i$conditional))
  params <- params[c(which(params == "(Intercept)"), which(params != "(Intercept)"))]
  prior_info$parameter <- params

  prior_info <- prior_info[, intersect(c("parameter", "dist", "location", "scale", "adjusted_scale", "response"), colnames(prior_info))]

  colnames(prior_info) <- gsub("dist", "distribution", colnames(prior_info))
  colnames(prior_info) <- gsub("df", "DoF", colnames(prior_info))

  priors <- as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x)) {
      as.numeric(as.character(x))
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)

  string <- strsplit(names(priors), "_", fixed = TRUE)
  string <- lapply(string, .capitalize)
  names(priors) <- unlist(lapply(string, paste0, collapse = "_"))

  # minor fixes
  priors$Parameter <- sprintf("%s|%s", priors$Response, priors$Parameter)

  priors
}


# =========================================================================
# BRMS ----------------------------------------------------------------
# =========================================================================

#' @rdname get_priors
#' @export
get_priors.brmsfit <- function(x, verbose = TRUE, ...) {

  info <- as.data.frame(.print_brmsprior_preparation(x$prior))
  info$Parameter <- .match_brms_priors_to_params(info)

  priors <- data.frame(Parameter = info$Parameter)

  # Format the prior string ------------------------------------
  priors$Distribution <- gsub("(.*)\\(.*", "\\1",
                              ifelse(info$prior == "(flat)",
                                     "uniform",
                                     info$prior))
  priors$Distribution[priors$Distribution == "lkj_corr_cholesky"] <- "lkj"

  # Initialize empty
  priors$Location <- NA
  priors$Scale <- NA
  priors$df <- NA

  # student_t(df, location, scale)
  is_student_t <- priors$Distribution == "student_t"
  priors$Location[is_student_t] <- gsub("(.*)\\((.*)\\,(.*)\\,(.*)\\)", "\\3", info$prior[is_student_t])
  priors$Scale[is_student_t] <- gsub("(.*)\\((.*)\\,(.*)\\,(.*)\\)", "\\4", info$prior[is_student_t])
  priors$df[is_student_t] <- gsub("(.*)\\((.*)\\,(.*)\\,(.*)\\)", "\\2", info$prior[is_student_t])

  # normal(location, scale)
  is_normal <- priors$Distribution == "normal"
  priors$Location[is_normal] <- gsub("(.*)\\((.*)\\,(.*)\\)", "\\2", info$prior[is_normal])
  priors$Scale[is_normal] <- gsub("(.*)\\((.*)\\,(.*)\\)", "\\3", info$prior[is_normal])

  # lkj(eta)
  is_lkj <- priors$Distribution == "lkj"
  priors$Location[is_lkj] <- gsub("(.*)\\((.*)\\)", "\\2", info$prior[is_lkj])

  # Transform to numeric
  priors$Location <- as.numeric(priors$Location)
  priors$Scale <- as.numeric(priors$Scale)
  priors$df <- as.numeric(priors$df)

  # Get parameters
  params <- find_parameters(x, ..., flatten = TRUE)

  # Loop through all parameters and try to retrieve its correct prior
  out <- data.frame()
  for(p in params) {

    subset <- priors[priors$Parameter == p, ]

    # If nothing corresponding directly to the parameter...
    if(nrow(subset) == 0) {
      # Special treatment for cor_*
      subset <- priors[sapply(priors$Parameter, grepl, x = p), ]

      # If still empty, make empty df
      if(nrow(subset) == 0) {
        subset <- setNames(data.frame(t(rep(NA, 5))), c("Parameter", "Distribution", "Location", "Scale", "df"))
      }
    }

    # Rbind the stuff
    subset$Parameter <- p
    out <- rbind(out, subset)
  }

  row.names(out) <- NULL
  attr(out, "priors") <- info

  out
}

# Utils -------

.print_brmsprior_preparation <- function(x) {
  # This function is taken from brms:::print.brmsprior
  # which adds information using private functions upon printing
  # but doesn't return it

  .stan_base_prior <- function(prior) {
    stopifnot(length(unique(prior$class)) <= 1)
    take <- with(prior, !nzchar(coef) & nzchar(prior))
    prior <- prior[take, ]
    if (!NROW(prior)) {
      return("")
    }
    vars <- c("group", "nlpar", "dpar", "resp", "class")
    for (v in vars) {
      take <- nzchar(prior[[v]])
      if (any(take)) {
        prior <- prior[take, ]
      }
    }
    stopifnot(NROW(prior) == 1)
    prior$prior
  }

  .find_rows <- function(x, ..., ls = list(), fun = '%in%') {
    x <- as.data.frame(x)
    if (!nrow(x)) {
      return(logical(0))
    }
    out <- rep(TRUE, nrow(x))
    ls <- c(ls, list(...))
    if (!length(ls)) {
      return(out)
    }
    if (is.null(names(ls))) {
      stop("Argument 'ls' must be named.")
    }
    for (name in names(ls)) {
      out <- out & brms::do_call(fun, list(x[[name]], ls[[name]]))
    }
    out
  }


  stopifnot(is.brmsprior(x))
  x$source[!nzchar(x$source)] <- "(unknown)"
  # column names to vectorize over
  cols <- c("group", "nlpar", "dpar", "resp", "class")
  empty_strings <- rep("", 4)
  for (i in which(!nzchar(x$prior))) {
    ls <- x[i, cols]
    ls <- rbind(ls, c(empty_strings, ls$class))
    ls <- as.list(ls)
    # sub_prior <- subset2(x, ls = ls)
    sub_prior <- x[.find_rows(x, ls = ls, fun = '%in%'), , drop = FALSE]
    base_prior <- .stan_base_prior(sub_prior)
    if (nzchar(base_prior)) {
      x$prior[i] <- base_prior
      x$source[i] <- "(vectorized)"
    } else {
      x$prior[i] <- "(flat)"
    }
  }
  x
}


.match_brms_priors_to_params <- function(prior_summary) {
  # Rename for easier manipulation
  pr <- prior_summary

  # Initialize empty string
  p <- rep("", nrow(pr))

  # class == Intercept -------------------------
  p <- ifelse(pr$class == "Intercept",
              paste0(
                "b",
                ifelse(pr$dpar != "", paste0("_", pr$dpar), ""),
                "_Intercept"
              ),
              p
  )

  # class == b ------------------------------
  # Are there other possible parameters?
  p <- ifelse(
    pr$class == "b",
    paste0("b_",
           ifelse(pr$dpar != "", paste0(pr$dpar, "_"), ""),
           pr$coef),
    p)

  # class == L ------------------------------
  p <- ifelse(pr$class == "L", paste0("cor_", pr$group, "_"), p)


  # class == sigma ------------------------------
  # TODO: I only saw it alone, but possibly can have other parameters
  p <- ifelse(pr$class == "sigma", "sigma", p)

  # class == sd  -------------------------------
  p <- ifelse(
    pr$class == "sd",
    paste0("sd_",
           pr$group,
           "__",
           ifelse(pr$dpar != "", paste0(pr$dpar, "_"), ""),
           pr$coef),
    p)

  # class == sds  -------------------------------
  # TODO: Fix coef for sds_
  # TODO: Fix beta for smooth term (bs_coef instead of b_coef)
  # p <- ifelse(
  #   pr$class == "sds",
  #   paste0("sds", ifelse(pr$coef  != "", paste0("_", pr$coef), "")),
  #   p)

  p
}

# =========================================================================
# BCPLM ----------------------------------------------------------------
# =========================================================================

#' @export
get_priors.bcplm <- function(x, ...) {
  params <- setdiff(find_parameters(x, flatten = TRUE), c("phi", "p"))

  location <- eval(parse(text = .safe_deparse(x@call))[[1]]$prior.beta.mean)
  if (is.null(location)) location <- 0

  scale <- eval(parse(text = .safe_deparse(x@call))[[1]]$prior.beta.var)
  if (is.null(scale)) scale <- 10000

  data.frame(
    Parameter = params,
    Distribution = "normal",
    Location = location,
    Scale = scale,
    stringsAsFactors = FALSE
  )
}


# =========================================================================
# meta -------------------------------------------------------------
# =========================================================================


#' @export
get_priors.meta_random <- function(x, ...) {
  params <- rownames(x$estimates)
  params[params == "d"] <- "(Intercept)"

  prior_info1 <- attr(x$prior_d, "param")
  prior_info2 <- attr(x$prior_tau, "param")

  fam1 <- attr(x$prior_d, "family")
  fam2 <- attr(x$prior_tau, "family")

  loc1 <- which(names(prior_info1) %in% c("mean", "location", "shape"))[1]
  loc2 <- which(names(prior_info2) %in% c("mean", "location", "shape"))[1]

  scale1 <- which(names(prior_info1) %in% c("scale", "sd"))[1]
  scale2 <- which(names(prior_info2) %in% c("scale", "sd"))[1]

  out <- data.frame(
    Parameter = params,
    Distribution = c(fam1, fam2),
    Location = c(prior_info1[loc1], prior_info2[loc2]),
    Scale = c(prior_info1[scale1], prior_info2[scale2]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .fix_metabma_priorname(out)
}


#' @export
get_priors.meta_fixed <- function(x, ...) {
  params <- rownames(x$estimates)
  params[params == "d"] <- "(Intercept)"

  prior_info <- attr(x$prior_d, "param")
  fam <- attr(x$prior_d, "family")

  loc <- which(names(prior_info) %in% c("mean", "location", "shape"))[1]
  scale <- which(names(prior_info) %in% c("scale", "sd"))[1]

  out <- data.frame(
    Parameter = params,
    Distribution = fam,
    Location = prior_info[loc],
    Scale = prior_info[scale],
    stringsAsFactors = FALSE
  )

  .fix_metabma_priorname(out)
}


.fix_metabma_priorname <- function(x) {
  x$Distribution <- gsub("t", "Student's t", x$Distribution, fixed = TRUE)
  x$Distribution <- gsub("norm", "Normal", x$Distribution, fixed = TRUE)
  x$Distribution <- gsub("invgamma", "Inverse gamma", x$Distribution, fixed = TRUE)
  x
}


# =========================================================================
# BayesFactor -------------------------------------------------------------
# =========================================================================


#' @export
get_priors.BFBayesFactor <- function(x, ...) {
  prior <- .compact_list(utils::tail(x@numerator, 1)[[1]]@prior[[1]])
  bf_type <- .classify_BFBayesFactor(x)

  prior_names <- switch(bf_type,
    "correlation" = "rho",
    "ttest1" = ,
    "ttest2" = "Difference",
    "meta" = "Effect",
    "proptest" = "Proportion",
    "xtable" = "Ratio",
    names(prior)
  )

  # Distribution
  if (bf_type == "xtable") {
    Distribution <- x@denominator@type[[1]]
  } else if (bf_type == "correlation") {
    Distribution <- "beta"
  } else {
    Distribution <- "cauchy"
  }

  # Prior
  prior_scale <- unlist(prior)

  if (length(prior_names) != length(prior_scale)) {
    prior_names <- unlist(lapply(prior_names, function(i) {
      if (!is.null(names(prior[[i]]))) {
        names(prior[[i]])
      } else {
        rep(i, times = length(prior[[i]]))
      }
    }))
  }

  if (bf_type == "correlation") {
    # "A shifted, scaled beta(1/rscale,1/rscale) prior distribution is assumed for rho"
    prior_scale <- 1 / prior_scale
  }

  # Location
  if (bf_type == "correlation") {
    location <- prior_scale
  } else {
    location <- 0
  }


  # Prepare output
  if (bf_type == "linear") {
    # find data types, to match priors
    data_types <- x@numerator[[1]]@dataTypes
    params <- find_parameters(x)

    # create data frame of parameter names and components
    out <- as.data.frame(utils::stack(params), stringsAsFactors = FALSE)
    colnames(out) <- c("Parameter", "Component")
    out$Distribution <- Distribution
    out$Location <- location
    out$Scale <- NA

    # find parameter names pattern to match data types
    find_types <- do.call(rbind, strsplit(out$Parameter, "-", TRUE))[, 1, drop = TRUE]
    interactions <- grepl(":", find_types, fixed = TRUE)
    find_types[interactions] <- gsub("(.*):(.*)", "\\2", find_types[interactions])
    cont_types <- data_types == "continuous"
    data_types[cont_types] <- paste0(data_types[cont_types], ".", names(data_types[cont_types]))
    for (i in 1:length(data_types)) {
      out$Scale[find_types == names(data_types)[i]] <- prior_scale[data_types[i]]
    }

    # missing information to NA
    out$Distribution[is.na(out$Scale)] <- NA
    out$Location[is.na(out$Scale)] <- NA
    out[c("Parameter", "Distribution", "Location", "Scale")]
  } else {
    data.frame(
      Parameter = prior_names,
      Distribution = Distribution,
      Location = location,
      Scale = prior_scale,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
}


# =========================================================================
# blavaan -------------------------------------------------------------
# =========================================================================


#' @export
get_priors.blavaan <- function(x, ...) {
  # installed?
  check_if_installed("lavaan")


  PE <- lavaan::parameterEstimates(
    x,
    se = FALSE, ci = FALSE, remove.eq = FALSE, remove.system.eq = TRUE,
    remove.ineq = FALSE, remove.def = FALSE, add.attributes = TRUE
  )

  if (!("group" %in% names(PE))) PE$group <- 1

  newpt <- x@ParTable
  pte2 <- which(newpt$free > 0)

  relevant_rows <- match(
    with(newpt, paste(lhs[pte2], op[pte2], rhs[pte2], group[pte2], sep = "")),
    paste(PE$lhs, PE$op, PE$rhs, PE$group, sep = "")
  )

  # Priors
  priors <- rep(NA, nrow(PE))
  priors[relevant_rows] <- newpt$prior[pte2]
  priors[is.na(PE$prior)] <- NA

  stats::na.omit(data.frame(
    Parameter = paste(PE$lhs, PE$op, PE$rhs, sep = ""),
    Distribution = gsub("(.*)\\((.*)", "\\1", priors),
    Location = as.numeric(gsub("(.*)\\((.*)\\,(.*)\\)(.*)", "\\2", priors)),
    Scale = as.numeric(gsub("(.*)\\((.*)\\,(.*)\\)(.*)", "\\3", priors)),
    stringsAsFactors = FALSE
  ))
}



#' @export
get_priors.mcmc.list <- function(x, ...) {
  NULL
}





# Utils -------------------------------------------------------------------



.is_numeric_character <- function(x) {
  (is.character(x) && !anyNA(suppressWarnings(as.numeric(stats::na.omit(x[nchar(x) > 0]))))) ||
    (is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x)))))
}



