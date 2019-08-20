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
    if (!is.null(.x)) do.call(cbind, .x)
  }))

  if (length(l) > 1) {
    prior_info <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), l)
  } else {
    cn <- colnames(l[[1]])
    prior_info <- as.data.frame(l)
    colnames(prior_info) <- cn
  }

  prior_info$parameter <- find_parameters(x)$conditional
  prior_info <- prior_info[, intersect(c("parameter", "dist", "location", "scale", "adjusted_scale"), colnames(prior_info))]

  colnames(prior_info) <- gsub("dist", "distribution", colnames(prior_info))
  colnames(prior_info) <- gsub("df", "DoF", colnames(prior_info))

  as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x)) {
      as.numeric(as.character(x))
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)
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

  prior_info$distribution <- gsub("(.*)\\(.*", "\\1", prior_info$prior)
  prior_info$location <- gsub("(.*)\\((.*)\\,(.*)", "\\2", prior_info$prior)
  prior_info$scale <- gsub("(.*)\\,(.*)\\)(.*)", "\\2", prior_info$prior)
  prior_info$parameter <- prior_info$coef

  prior_info <- prior_info[, c("parameter", "distribution", "location", "scale")]

  pinfo <- as.data.frame(lapply(prior_info, function(x) {
    if (.is_numeric_character(x)) {
      as.numeric(as.character(x))
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)

  if (.is_empty_string(pinfo$distribution)) {
    print_color("Model was fitted with uninformative (flat) priors!\n", "red")
    pinfo$distribution <- "uniform"
    pinfo$location <- 0
    pinfo$scale <- NA
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
    parameter = names(prior),
    distribution = "cauchy",
    location = 0,
    scale = unlist(prior),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



#' @importFrom stats na.omit
.is_numeric_character <- function(x) {
  (is.character(x) && !anyNA(suppressWarnings(as.numeric(stats::na.omit(x))))) ||
    (is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x)))))
}
