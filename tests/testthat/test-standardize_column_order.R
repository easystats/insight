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

  expect_equal(
    names(standardize_column_order(df1, style = "easystats")),
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

  expect_equal(
    names(standardize_column_order(df2, style = "broom")),
    c(
      "estimate", "conf.level", "conf.low", "conf.high", "method",
      "statistic", "df", "p.value"
    )
  )
})
