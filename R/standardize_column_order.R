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
standardize_column_order.default <- function(data, ...) {
  print_color(sprintf("Objects of class '%s' are currently not supported.\n", class(data)[1]), "red")
  invisible(data)
}

#' @rdname standardize_column_order
#' @export
standardize_column_order.parameters_model <- function(data,
                                                      style = "easystats",
                                                      ...) {
  style <- validate_argument(style, c("easystats", "broom"))

  col_order <- switch(style,
    easystats = easystats_columns(),
    broom_columns()
  )

  data[union(intersect(col_order, names(data)), names(data))]
}

#' @export
standardize_column_order.effectsize_table <- standardize_column_order.parameters_model

#' @export
standardize_column_order.data.frame <- standardize_column_order.parameters_model

#' @export
standardize_column_order.parameters_distribution <- standardize_column_order.parameters_model


# column definitions ---------------------------------------------------------

# here we should have all valid column names that are used or defined across
# easystats packages...

#' Easystats columns
#'
#' Returns all valid column names that are used or defined across easystats
#' packages as character vector.
#'
#' @param select String, indicating which columns to return.
#'
#' @return A character vector with all (or selected) column names that are
#' in use across the easystats-ecosystem, or broom-alike column names for
#' `broom_columns()`.
#'
#' @examples
#' easystats_columns("uncertainty")
#'
#' @export
easystats_columns <- function(select = "all") {
  select <- validate_argument(
    select,
    c(
      "all", "parameter", "estimate", "type", "uncertainty", "prior", "method",
      "statistic", "df", "p", "other", "effectsize"
    )
  )
  # Parameter names or levels
  cols_parameter <- c(
    "Level1", "Level2", "Parameter1", "Parameter2", "Parameter",
    "Mean_Parameter1", "Mean_Parameter2", "Mean_Group1", "Mean_Group2"
  )
  # estimate
  cols_estimate <- c(
    "Coefficient", "r", "rho", "tau", "Estimate",
    "Median", "Mean", "MAP", "MAD", "Dxy", "Difference", "Predicted", "Psihat",
    "Trimmed_Mean", "R2", "Mu", "Ratio", "Probability", "Slope"
  )
  # type of estimate
  cols_esttype <- c("Group", "Component", "Response", "Response_Level", "Effects", "Weight")
  # uncertainty
  cols_uncertainty <- c(
    "SE", "Std. Error", "SD", "Deviance_error",
    "CI", "CI_low", "CI_high", "Difference_CI_low", "Difference_CI_high",
    "CI_Method", "CI_Distribution", "CI_Iterations",
    "Sum_Squares", "Mean_Square"
  )
  # prior details
  cols_prior <- c("Prior_Distribution", "Prior_Location", "Prior_Scale")
  # test details
  cols_method <- c("Method", "method")
  # statistic
  cols_statistic <- c(
    "t", "t value", "z", "z value", "F", "F value", "Chi2", "Chi2 value",
    "chisq", "Chisq", "chi-sq", "t / F", "z / Chisq", "z / Chi2", "W", "S",
    "Statistic"
  )
  # degrees of freedom
  cols_df <- c("df", "df_error", "df_residual")
  # p-value
  cols_p <- c("p", "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "BF", "log_BF")
  # other details
  cols_other <- c("Alternative", "n_Obs", "Rhat", "ESS")
  # effectsize details
  cols_effsize <- c(
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

  out <- NULL

  if (select %in% c("all", "parameter")) {
    out <- c(out, cols_parameter)
  }
  if (select %in% c("all", "estimate")) {
    out <- c(out, cols_estimate)
  }
  if (select %in% c("all", "type")) {
    out <- c(out, cols_esttype)
  }
  if (select %in% c("all", "uncertainty")) {
    out <- c(out, cols_uncertainty)
  }
  if (select %in% c("all", "prior")) {
    out <- c(out, cols_prior)
  }
  if (select %in% c("all", "method")) {
    out <- c(out, cols_method)
  }
  if (select %in% c("all", "statistic")) {
    out <- c(out, cols_statistic)
  }
  if (select %in% c("all", "df")) {
    out <- c(out, cols_df)
  }
  if (select %in% c("all", "p")) {
    out <- c(out, cols_p)
  }
  if (select %in% c("all", "other")) {
    out <- c(out, cols_other)
  }
  if (select %in% c("all", "effectsize")) {
    out <- c(out, cols_effsize)
  }
  out
}

# here we should have all valid column names that are used or defined across
# easystats packages, in broom-style

#' @rdname easystats_columns
#' @export
broom_columns <- function(select = "all") {
  select <- validate_argument(
    select,
    c(
      "all", "parameter", "estimate", "type", "uncertainty", "prior", "method",
      "statistic", "df", "p", "other", "effectsize"
    )
  )
  # Parameter names or levels
  cols_parameter <- "term"
  # estimate
  cols_estimate <- c("estimate", "mean.group1", "mean.group2", "predicted")
  # type of estimate
  cols_esttype <- c("group", "component", "response", "response.level", "effects", "weight")
  # uncertainty
  cols_uncertainty <- c(
    "std.error", "std.dev",
    "conf.level", "conf.low", "conf.high", "conf.method", "conf.distribution", "conf.iterations",
    "sum.squares", "mean.square"
  )
  # prior details
  cols_prior <- c("prior.distribution", "prior.location", "prior.scale")
  # test details
  cols_method <- "method"
  # statistic
  cols_statistic <- "statistic"
  # degrees of freedom
  cols_df <- c("df", "df.error", "df.residual")
  # p-value
  cols_p <- c("p.value", "pd", "rope.percentage", "bayes.factor", "log(bayes.factor)")
  # other details
  cols_other <- c("alternative", "n.obs", "rhat", "ess")
  # effectsize details
  cols_effsize <- c(
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

  out <- NULL

  if (select %in% c("all", "parameter")) {
    out <- c(out, cols_parameter)
  }
  if (select %in% c("all", "estimate")) {
    out <- c(out, cols_estimate)
  }
  if (select %in% c("all", "type")) {
    out <- c(out, cols_esttype)
  }
  if (select %in% c("all", "uncertainty")) {
    out <- c(out, cols_uncertainty)
  }
  if (select %in% c("all", "prior")) {
    out <- c(out, cols_prior)
  }
  if (select %in% c("all", "method")) {
    out <- c(out, cols_method)
  }
  if (select %in% c("all", "statistic")) {
    out <- c(out, cols_statistic)
  }
  if (select %in% c("all", "df")) {
    out <- c(out, cols_df)
  }
  if (select %in% c("all", "p")) {
    out <- c(out, cols_p)
  }
  if (select %in% c("all", "other")) {
    out <- c(out, cols_other)
  }
  if (select %in% c("all", "effectsize")) {
    out <- c(out, cols_effsize)
  }
  out
}
