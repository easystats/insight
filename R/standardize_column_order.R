#' Standardize column order
#'
#' Standardizes order of columns for dataframes and other objects from
#' *easystats* and *broom* ecosystem packages.
#'
#' @return A data frame, with standardized column order.
#'
#' @inheritParams standardize_names
#'
#' @examples
#' # easystats conventions
#' df1 <- cbind.data.frame(
#'   CI_low      = -2.873,
#'   t           = 5.494,
#'   CI_high     = -1.088,
#'   p           = 0.00001,
#'   Parameter   = -1.980,
#'   CI          = 0.95,
#'   df          = 29.234,
#'   Method      = "Student's t-test"
#' )
#'
#' standardize_column_order(df1, style = "easystats")
#'
#' # broom conventions
#' df2 <- cbind.data.frame(
#'   conf.low   = -2.873,
#'   statistic  = 5.494,
#'   conf.high  = -1.088,
#'   p.value    = 0.00001,
#'   estimate   = -1.980,
#'   conf.level = 0.95,
#'   df         = 29.234,
#'   method     = "Student's t-test"
#' )
#'
#' standardize_column_order(df2, style = "broom")
#' @export

standardize_column_order <- function(data, ...) {
  UseMethod("standardize_column_order")
}

#' @export
standardize_names.default <- function(data, ...) {
  print_color(sprintf("Objects of class '%s' are currently not supported.\n", class(data)[1]), "red")
}

#' @rdname standardize_column_order
#' @export
standardize_column_order.parameters_model <- function(data,
                                                      style = c("easystats", "broom"),
                                                      ...) {
  style <- match.arg(style)

  # easystats --------------------------------

  if (style == "easystats") {
    col_order <- c(
      # estimate
      "Parameter1", "Parameter2", "Parameter", "Mean_Parameter1", "Mean_Parameter2",
      "Mean_Group1", "Mean_Group2", "Coefficient", "r", "rho", "tau", "Estimate",
      "Median", "Mean", "MAP", "MAD", "Dxy", "Difference", "Psihat",
      "Trimmed_Mean", "R2", "Mu",
      # type of estimate
      "Group", "Component", "Response", "Effects", "Weight",
      # uncertainty
      "SE", "Std. Error", "SD", "Deviance_error",
      "CI", "CI_low", "CI_high", "Difference_CI_low", "Difference_CI_high",
      "CI_Method", "CI_Distribution", "CI_Iterations",
      "Sum_Squares", "Mean_Square",
      "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Rhat", "ESS",
      # prior details
      "Prior_Distribution", "Prior_Location", "Prior_Scale",
      # test details
      "Method", "method",
      # statistic
      "t", "t value", "z", "z value", "F", "F value", "Chi2", "Chi2 value",
      "chisq", "Chisq", "chi-sq", "t / F", "z / Chisq", "z / Chi2", "W", "S",
      # degrees of freedom
      "df", "df_error", "df_residual",
      # p-value
      "p", "BF", "log_BF",
      # other details
      "Alternative", "n_Obs",
      # effectsize details
      "Effectsize",
      "d", "Cohens_d", "d_CI_low", "d_CI_high",
      "g", "Hedges_g", "g_CI_low", "g_CI_high",
      "Eta2", "Eta2_CI_low", "Eta2_CI_high",
      "Omega2", "Omega2_CI_low", "Omega2_CI_high",
      "Epsilon2", "Epsilon2_CI_low", "Epsilon2_CI_high",
      "Cramers_v", "Cramers_v_adjusted", "Cramers_CI_low", "Cramers_CI_high",
      "phi", "phi_adjusted", "phi_CI_low", "phi_CI_high",
      "r_rank_biserial", "rank_biserial_CI_low", "rank_biserial_CI_high",
      "rank_epsilon_squared", "rank_epsilon_squared_CI_low", "rank_epsilon_squared_CI_high",
      "Kendalls_W", "Kendalls_W_CI_low", "Kendalls_W_CI_high"
    )
  }

  # broom ------------------------------------

  if (style == "broom") {
    col_order <- c(
      # estimate
      "estimate", "mean.group1", "mean.group2",
      # type of estimate
      "group", "component", "response", "effects", "weight",
      # uncertainty
      "std.error", "std.dev",
      "conf.level", "conf.low", "conf.high", "conf.method", "conf.distribution", "conf.iterations",
      "sum.squares", "mean.square",
      "pd", "rope.percentage", "rhat", "ess",
      # prior details
      "prior.distribution", "prior.location", "prior.scale",
      # test details
      "method",
      # statistic
      "statistic",
      # degrees of freedom
      "df", "df.error", "df.residual",
      # p-value
      "p.value", "bayes.factor", "log(bayes.factor)",
      # other details
      "alternative", "n.obs",
      # effectsize details
      "effectsize",
      "d", "cohens.d", "d.conf.low", "d.conf.high",
      "g", "Hedges.g", "g.conf.low", "g.conf.high",
      "eta2", "eta2.conf.low", "eta2.conf.high",
      "omega2", "omega2.conf.low", "omega2.conf.high",
      "epsilon2", "epsilon2.conf.low", "epsilon2.conf.high",
      "cramers.v", "cramers.v.adjusted", "cramers.conf.low", "cramers.conf.high",
      "phi", "phi.adjusted", "phi.conf.low", "phi.conf.high",
      "r.rank.biserial", "rank.biserial.conf.low", "rank.biserial.conf.high",
      "rank.epsilon.squared", "rank.epsilon.squared.conf.low", "rank.epsilon.squared.conf.high",
      "kendalls.w", "kendalls.w.conf.low", "kendalls.w.conf.high"
    )
  }

  data[union(intersect(col_order, names(data)), names(data))]
}

#' @export
standardize_column_order.effectsize_table <- standardize_column_order.parameters_model

#' @export
standardize_column_order.data.frame <- standardize_column_order.parameters_model

#' @export
standardize_column_order.parameters_distribution <- standardize_column_order.parameters_model
