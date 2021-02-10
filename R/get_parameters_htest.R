#' @title Get model parameters from htest-objects
#' @name get_parameters.htest
#'
#' @description Returns the parameters from a hypothesis test.
#'
#' @param ... Currently not used.
#' @inheritParams find_parameters
#'
#' @return A data frame with two columns: the parameter names and the related
#'   point estimates.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.htest <- function(x, ...) {
  m_info <- model_info(x)

  if (m_info$is_correlation) {
    out <- .extract_htest_correlation(x)
  } else if (m_info$is_levenetest) {
    out <- .extract_htest_levenetest(x)
  } else if (m_info$is_ttest) {
    out <- .extract_htest_ttest(x)
  } else if (m_info$is_ranktest) {
    out <- .extract_htest_ranktest(x)
  } else if (m_info$is_onewaytest) {
    out <- .extract_htest_oneway(x)
  } else if (m_info$is_chi2test) {
    out <- .extract_htest_chi2(x)
  } else if (m_info$is_proptest) {
    out <- .extract_htest_prop(x)
  } else if (m_info$is_binomtest) {
    out <- .extract_htest_binom(x)
  } else {
    stop("'get_parameters()' not implemented for such hypothesis tests yet.")
  }
  row.names(out) <- NULL
  out
}




# extract htest correlation ----------------------


.extract_htest_correlation <- function(model) {
  out <- data.frame(
    Parameter = model$data.name,
    stringsAsFactors = FALSE
  )

  if (model$method == "Pearson's Chi-squared test") {
    out$Estimate <- model$statistic
  } else {
    out$Estimate <- model$estimate
  }
  out
}




# extract htest ranktest ----------------------


.extract_htest_ranktest <- function(model) {
  out <- data.frame(
    Parameter = model$data.name,
    stringsAsFactors = FALSE
  )

  if (grepl("Wilcoxon", model$method, fixed = TRUE)) {
    out$Estimate <- model$statistic
  } else if (grepl("Kruskal-Wallis", model$method, fixed = TRUE)) {
    out$Estimate <- model$statistic
  }
  out
}




# extract htest leveneTest ----------------------


.extract_htest_levenetest <- function(model) {
  data.frame(
    Parameter = "Parameter",
    Estimate = model$`F value`[1],
    stringsAsFactors = FALSE
  )
}




# extract htest ttest ----------------------


.extract_htest_ttest <- function(model, standardized_d = NULL, hedges_g = NULL) {
  out <- data.frame(
    Parameter = model$data.name,
    stringsAsFactors = FALSE
  )
  if (length(model$estimate) == 1) {
    out$Estimate <- model$estimate
  } else {
    out$Estimate <- model$estimate[1] - model$estimate[2]
  }
  out
}




# extract htest oneway ----------------------


.extract_htest_oneway <- function(model) {
  NULL
}




# extract htest chi2 ----------------------


.extract_htest_chi2 <- function(model) {
  out <- data.frame(
    Parameter = model$data.name,
    stringsAsFactors = FALSE
  )

  if (!is.null(model$estimate) && identical(names(model$estimate), "odds ratio")) {
    out$Estimate <- model$estimate
  } else {
    out$Estimate <- model$statistic
  }
  out
}




# extract htest prop ----------------------


.extract_htest_prop <- function(model) {
  ## TODO for proportion
  NULL
}




# extract htest binom ----------------------


.extract_htest_binom <- function(model) {
  out <- data.frame(
    Parameter = "p",
    Estimate = model$estimate,
    stringsAsFactors = FALSE
  )
  out
}
