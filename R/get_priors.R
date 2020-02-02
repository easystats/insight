#' @title Get summary of priors used for a model
#' @name get_priors
#'
#' @description Provides a summary of the prior distributions used
#'   for the parameters in a given model.
#'
#' @param x A Bayesian model.
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
      do.call(cbind, .x)
    }
  }))

  if (length(l) > 1) {
    prior_info <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), l)
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


#' @export
get_priors.brmsfit <- function(x, ...) {
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
    print_color("Model was fitted with uninformative (flat) priors!\n", "red")
    pinfo$Distribution <- "uniform"
    pinfo$Location <- 0
    pinfo$Scale <- NA
  }

  pinfo
}


#' @importFrom utils tail
#' @export
get_priors.BFBayesFactor <- function(x, ...) {
  prior <- .compact_list(utils::tail(x@numerator, 1)[[1]]@prior[[1]])

  switch(
    .classify_BFBayesFactor(x),
    "correlation" = names(prior) <- "rho",
    "ttest" = names(prior) <- "Difference"
  )

  data.frame(
    Parameter = names(prior),
    Distribution = "cauchy",
    Location = 0,
    Scale = unlist(prior),
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
