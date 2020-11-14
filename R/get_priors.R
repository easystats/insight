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


#' @export
get_priors.stanreg <- function(x, ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("To use this function, please install package 'rstanarm'.")
  }

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

  prior_info$parameter <- find_parameters(x)$conditional
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
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("To use this function, please install package 'rstanarm'.")
  }

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


#' @rdname get_priors
#' @export
get_priors.brmsfit <- function(x, verbose = TRUE, ...) {
  ## TODO needs testing for edge cases - check if "coef"-column is
  # always empty for intercept-class
  x$prior$coef[x$prior$class == "Intercept"] <- "(Intercept)"


  # get default prior for all parameters, if defined
  def_prior_b <- which(x$prior$prior != "" & x$prior$class == "b" & x$prior$coef == "")

  # check which parameters have a default prior
  need_def_prior <- which(x$prior$prior == "" & x$prior$class == "b" & x$prior$coef != "")

  if (!.is_empty_object(def_prior_b) && !.is_empty_object(need_def_prior)) {
    x$prior$prior[need_def_prior] <- x$prior$prior[def_prior_b]
  }


  # get default prior for all parameters, if defined
  def_prior_intercept <- which(x$prior$prior != "" & x$prior$class == "Intercept" & x$prior$coef == "")

  # check which parameters have a default prior
  need_def_prior <- which(x$prior$prior == "" & x$prior$class == "Intercept" & x$prior$coef != "")

  if (!.is_empty_object(def_prior_intercept) && !.is_empty_object(need_def_prior)) {
    x$prior$prior[need_def_prior] <- x$prior$prior[def_prior_intercept]
  }


  prior_info <- x$prior[x$prior$coef != "" & x$prior$class %in% c("b", "(Intercept)"), ]
  # find additional components, avoid duplicated coef-names
  components <- prior_info$dpar != ""
  prior_info$dpar[components] <- paste0(prior_info$dpar[components], "_")
  prior_info$coef <- paste0(prior_info$dpar, prior_info$coef)

  prior_info$Distribution <- gsub("(.*)\\(.*", "\\1", prior_info$prior)
  prior_info$Location <- gsub("(.*)\\((.*)\\,(.*)", "\\2", prior_info$prior)
  prior_info$Scale <- gsub("(.*)\\,(.*)\\)(.*)", "\\2", prior_info$prior)
  prior_info$Parameter <- prior_info$coef

  prior_info <- prior_info[, c("Parameter", "Distribution", "Location", "Scale")]

  pinfo <- as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x)) {
      as.numeric(as.character(x))
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)

  if (.is_empty_string(pinfo$Distribution)) {
    if (verbose) {
      print_color("Model was fitted with uninformative (flat) priors!\n", "red")
    }
    pinfo$Distribution <- "uniform"
    pinfo$Location <- 0
    pinfo$Scale <- NA
  }

  pinfo
}



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



#' @export
get_priors.meta_random <- function(x, ...) {
  params <- rownames(x$estimates)
  params[params == "d"] <- "(Intercept)"

  prior_info1 <- attr(x$prior_d, "param")
  prior_info2 <- attr(x$prior_tau, "param")

  fam1 <- attr(x$prior_d, "family")
  fam2 <- attr(x$prior_tau, "family")

  data.frame(
    Parameter = params,
    Distribution = c(fam1, fam2),
    Location = c(prior_info1["mean"], prior_info2["shape"]),
    Scale = c(prior_info1["sd"], prior_info2["scale"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
get_priors.meta_fixed <- function(x, ...) {
  params <- rownames(x$estimates)
  params[params == "d"] <- "(Intercept)"

  prior_info <- attr(x$prior_d, "param")
  fam <- attr(x$prior_d, "family")

  data.frame(
    Parameter = params,
    Distribution = fam,
    Location = prior_info["mean"],
    Scale = prior_info["sd"],
    stringsAsFactors = FALSE
  )
}



#' @importFrom utils tail
#' @export
get_priors.BFBayesFactor <- function(x, ...) {
  prior <- .compact_list(utils::tail(x@numerator, 1)[[1]]@prior[[1]])
  bf_type <- .classify_BFBayesFactor(x)

  prior_names <- switch(
    .classify_BFBayesFactor(x),
    "correlation" = "rho",
    "ttest1" = ,
    "ttest2" = "Difference",
    "meta" = "Effect",
    "proptest" = "Proportion",
    "xtable" = "Ratio",
    names(prior)
  )

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

  data.frame(
    Parameter = prior_names,
    Distribution = "cauchy",
    Location = 0,
    Scale = prior_scale,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



#' @export
get_priors.blavaan <- function(x, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it.")
  }

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



#' @importFrom stats na.omit
.is_numeric_character <- function(x) {
  (is.character(x) && !anyNA(suppressWarnings(as.numeric(stats::na.omit(x))))) ||
    (is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x)))))
}
