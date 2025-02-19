#' @title Return function of transformed response variables
#' @name get_transformation
#'
#' @description
#'
#' This functions checks whether any transformation, such as log- or
#' exp-transforming, was applied to the response variable (dependent variable)
#' in a regression formula, and returns the related function that was used for
#' transformation. See [`find_transformation()`] for an overview of supported
#' transformations that are detected.
#'
#' @param verbose Logical, if `TRUE`, prints a warning if the transformation
#' could not be determined.
#' @inheritParams find_transformation
#'
#' @return
#'
#' A list of two functions: `$transformation`, the function that was used to
#' transform the response variable; `$inverse`, the inverse-function of
#' `$transformation` (can be used for "back-transformation"). If no
#' transformation was applied, both list-elements `$transformation` and
#' `$inverse` just return `function(x) x`. If transformation is unknown,
#' `NULL` is returned.
#'
#' @examples
#' # identity, no transformation
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' get_transformation(model)
#'
#' # log-transformation
#' model <- lm(log(Sepal.Length) ~ Species, data = iris)
#' get_transformation(model)
#'
#' # log-function
#' get_transformation(model)$transformation(0.3)
#' log(0.3)
#'
#' # inverse function is exp()
#' get_transformation(model)$inverse(0.3)
#' exp(0.3)
#'
#' # get transformations for all model terms
#' model <- lm(mpg ~ log(wt) + I(gear^2) + exp(am), data = mtcars)
#' get_transformation(model, include_all = TRUE)
#' @export
get_transformation <- function(x, include_all = FALSE, verbose = TRUE) {
  if (include_all) {
    lapply(find_terms(x), function(i) {
      stats::setNames(
        lapply(i, .get_transformation, verbose = verbose),
        clean_names(i)
      )
    })
  } else {
    .get_transformation(model = x, verbose)
  }
}


.get_transformation <- function(model, verbose) {
  # extract transformation
  transform_fun <- find_transformation(model)

  # unknown
  if (is.null(transform_fun)) {
    return(NULL)
  }

  # init
  out <- NULL

  if (transform_fun == "identity") {
    out <- list(transformation = function(x) x, inverse = function(x) x)
  } else if (transform_fun == "log") {
    out <- list(transformation = log, inverse = exp)
  } else if (transform_fun %in% c("log1p", "log(x+1)")) {
    out <- list(transformation = log1p, inverse = expm1)
  } else if (transform_fun == "log10") {
    out <- list(transformation = log10, inverse = function(x) 10^x)
  } else if (transform_fun == "log2") {
    out <- list(transformation = log2, inverse = function(x) 2^x)
  } else if (startsWith(transform_fun, "log(x,base=")) {
    base_value <- gsub("log\\(x,base=(\\d+)\\)", "\\1", transform_fun)
    out <- list(
      transformation = eval(parse(text = paste0("function(x) log(x,base=", base_value, ")"))),
      inverse = eval(parse(text = paste0("function(x) ", base_value, "^x")))
    )
  } else if (transform_fun == "exp") {
    out <- list(transformation = exp, inverse = log)
  } else if (transform_fun == "sqrt") {
    out <- list(transformation = sqrt, inverse = function(x) x^2)
  } else if (transform_fun == "inverse") {
    out <- list(transformation = function(x) 1 / x, inverse = function(x) x^-1)
  } else if (transform_fun == "scale") {
    denominator <- .extract_scale_denominator(model)
    out <- list(
      transformation = eval(parse(text = paste0("function(x) x / ", as.character(denominator)))), # nolint
      inverse = eval(parse(text = paste0("function(x) x * ", as.character(denominator))))
    )
  } else if (transform_fun == "box-cox") {
    denominator <- .extract_scale_denominator(model)
    out <- list(
      transformation = eval(parse(text = paste0("function(x) (x^", as.character(denominator), "-1) / ", as.character(denominator)))), # nolint
      inverse = eval(parse(text = paste0("function(x) exp(log(1 + ", as.character(denominator), " * x) / ", as.character(denominator), ")"))) # nolint
    )
  } else if (transform_fun == "power") {
    trans_power <- .extract_power_transformation(model)
    # trans_power == 0 is an invalid transformation - power to 0 *always*
    # returns 1, independent from the input-values
    if (!is.null(trans_power) && trans_power != 0) {
      out <- list(
        transformation = eval(parse(text = paste0("function(x) x^", as.character(trans_power)))), # nolint
        inverse = eval(parse(text = paste0("function(x) x^(", as.character(trans_power), "^-1)")))
      )
    }
  } else if (transform_fun == "expm1") {
    out <- list(transformation = expm1, inverse = log1p)
  } else if (transform_fun == "log-log") {
    out <- list(
      transformation = function(x) log(log(x)),
      inverse = function(x) exp(exp(x))
    )
  }

  # warn if no transformation could be identified
  if (verbose && is.null(out)) {
    insight::format_alert(
      paste0("The transformation and inverse-transformation functions for `", transform_fun, "` could not be determined.") # nolint
    )
  }

  out
}


# helper ------------------------------


.extract_power_transformation <- function(model) {
  if (is.character(model)) {
    resp_term <- model
  } else {
    resp_term <- find_terms(model)[["response"]]
  }
  .safe(as.numeric(gsub("\\(|\\)", "", gsub("(.*)(\\^|\\*\\*)\\s*(\\d+|[()])", "\\3", resp_term)))) # nolint
}


.extract_scale_denominator <- function(model) {
  if (is.character(model)) {
    resp_term <- model
  } else {
    resp_term <- find_terms(model)[["response"]]
  }
  # more complicated case: scale is inside `I()`
  if (startsWith(resp_term[1], "I(")) {
    as.numeric(gsub("(.*)/(.*)\\)", "\\2", resp_term[1]))
  } else {
    as.numeric(resp_term[2])
  }
}
