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


test_that("reorder columns BF", {
  # brms_bf <- suppressWarnings(download_model("brms_bf_1"))
  out <- data.frame(
    Parameter = c("b_Intercept", "b_wt", "sigma"),
    Component = c("conditional", "conditional", "sigma"),
    Median = c(32.22175, -3.755645, 3.461165),
    CI = c(0.95, 0.95, 0.95),
    CI_low = c(27.2244525, -4.9688055, 2.6517275),
    CI_high = c(35.75887, -2.21074025, 4.69652725),
    pd = c(1, 1, 1),
    ROPE_Percentage = c(0, 0, 0),
    log_BF = c(14.4924732349718, 5.79962753110103, 8.89383915455679),
    Rhat = c(1.00438747198895, 1.00100407213689, 0.992006699276081),
    ESS = c(88.3152312142069, 91.7932788446396, 167.822262320689),
    stringsAsFactors = FALSE
  )

  expect_named(
    standardize_column_order(out),
    c(
      "Parameter", "Median", "Component", "CI", "CI_low", "CI_high",
      "pd", "ROPE_Percentage", "log_BF", "Rhat", "ESS"
    )
  )
})


test_that("easystats_columns", {
  expect_identical(
    easystats_columns("uncertainty"),
    c(
      "SE", "Std. Error", "SD", "Deviance_error", "CI", "CI_low",
      "CI_high", "Difference_CI_low", "Difference_CI_high", "CI_Method",
      "CI_Distribution", "CI_Iterations", "Sum_Squares", "Mean_Square"
    )
  )
  expect_identical(
    easystats_columns("p"),
    c(
      "p", "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
      "BF", "log_BF"
    )
  )
})
