#' @title Get the values from the response variable
#' @name get_response
#'
#' @description Returns the values the response variable(s) from a model object.
#'    If the model is a multivariate response model, a data frame with values
#'    from all response variables is returned.
#'
#' @param select Optional name(s) of response variables for which to extract values.
#'   Can be used in case of regression models with multiple response variables.
#' @param as_proportion Logical, if `TRUE` and the response value is a proportion
#'   (e.g. `y1 / y2`), then the returned response value will be a vector with
#'   the result of this proportion. Else, always a data frame is returned.
#' @inheritParams find_predictors
#' @inheritParams get_data
#'
#' @return The values of the response variable, as vector, or a data frame if
#'   `x` has more than one defined response variable.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' data(cbpp)
#' cbpp$trials <- cbpp$size - cbpp$incidence
#' dat <<- cbpp
#'
#' m <- glm(cbind(incidence, trials) ~ period, data = dat, family = binomial)
#' head(get_response(m))
#' get_response(m, select = "incidence")
#'
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_response(m)
#' @export
get_response <- function(x, select = NULL, as_proportion = TRUE, source = "environment", verbose = TRUE) {
  rn <- find_response(x, combine = FALSE)
  combined_rn <- find_response(x, combine = TRUE)

  if (is.null(rn)) {
    return(NULL)
  }

  # check if response is a proportion for a binomial glm
  proportion_response <- combined_rn[!grepl("I\\((.*)\\)", combined_rn)]
  binom_fam <- tryCatch(stats::family(x)$family == "binomial", error = function(x) FALSE)
  glm_proportion <- any(grepl("/", proportion_response, fixed = TRUE)) && binom_fam

  # data used to fit the model
  model_data <- get_data(x, source = source, verbose = FALSE)

  # exceptions
  if (inherits(x, "DirichletRegModel")) {
    response <- x$Y
    class(response) <- "matrix"
    data.frame(response)
  } else if (inherits(x, "bfsl")) {
    response <- model_data[["y"]]
  } else {
    response <- model_data[, rn, drop = FALSE]
    # if user only wants specific response value, return this only
    if (!is.null(select) && all(select %in% colnames(response))) {
      response <- response[, select, drop = TRUE]
    }
    # check if more than one column, else coerce to vector
    if ((is.data.frame(response) || is.matrix(response)) && ncol(response) > 1L) {
      # preserve response proportion?
      if (as_proportion && glm_proportion) {
        response <- response[[1]] / response[[2]]
      }
      # make sure we have a vector for 1-column data frames
    } else {
      response <- response[[1]]
    }
    # for special classes, coerce to simple vector
    if (!is.factor(response) && !is.numeric(response) && !is.character(response) &&
      !is.logical(response) && !is.integer(response) && !is.data.frame(response) &&
      !is.matrix(response)) {
      response <- as.vector(response)
    }
  }
  response
}
