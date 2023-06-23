test_that("get_predicted", {
  # easystats conventions
  df1 <- cbind.data.frame(
    CI_low      = -2.873,
    t           = 5.494,
    CI_high     = -1.088,
    p           = 0.00001,
    Parameter   = -1.980,
    CI          = 0.95,
    df          = 29.234,
    Method      = "Student's t-test"
  )

  expect_named(
    standardize_column_order(df1, style = "easystats"),
    c("Parameter", "CI", "CI_low", "CI_high", "Method", "t", "df", "p")
  )

  # broom conventions
  df2 <- cbind.data.frame(
    conf.low   = -2.873,
    statistic  = 5.494,
    conf.high  = -1.088,
    p.value    = 0.00001,
    estimate   = -1.980,
    conf.level = 0.95,
    df         = 29.234,
    method     = "Student's t-test"
  )

  expect_named(
    standardize_column_order(df2, style = "broom"),
    c(
      "estimate", "conf.level", "conf.low", "conf.high", "method",
      "statistic", "df", "p.value"
    )
  )

  # deliberately misspecify column names
  # the misspecified columns should be pushed to the end
  df3 <- cbind.data.frame(
    CI_Low      = -2.873,
    t           = 5.494,
    CI_High     = -1.088,
    p           = 0.00001,
    Parameter   = -1.980,
    CI          = 0.95,
    df          = 29.234,
    Method      = "Student's t-test"
  )

  expect_named(
    standardize_column_order(df3, style = "easystats"),
    c("Parameter", "CI", "Method", "t", "df", "p", "CI_Low", "CI_High")
  )
})


## FIXME: requires "cmdstanr" package?

# test_that("reorder columns BF", {
#   skip_on_cran()
#   skip_on_os(c("mac", "linux"))
#   brms_bf <- suppressWarnings(download_model("brms_bf_1"))
#   skip_if(is.null(brms_bf))
#   skip_if_not_installed("parameters")
#   skip_if_not_installed("bayestestR")
#   out <- suppressWarnings(parameters::model_parameters(brms_bf, test = c("pd", "BF", "rope")))

#   expect_named(
#     standardize_column_order(out),
#     c(
#       "Parameter", "Median", "Component", "CI", "CI_low", "CI_high",
#       "pd", "ROPE_Percentage", "log_BF", "Rhat", "ESS"
#     )
#   )
# })
