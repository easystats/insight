skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("marginaleffects", {
  m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  x <- marginaleffects::slopes(m,
    variables = "Petal.Length",
    newdata = insight::get_datagrid(m, by = "Species")
  )
  # Equivalent in emmeans
  x2 <- emmeans::emtrends(m, var = "Petal.Length", specs = ~ Species + Petal.Length)

  # Get parameters
  p1 <- insight::get_parameters(x)
  p2 <- insight::get_parameters(x2)

  expect_true("Estimate" %in% colnames(p1))
  expect_true("Species" %in% colnames(p1))
  expect_true("Petal.Length" %in% colnames(p1))
  expect_equal(p1$Estimate, p2$Estimate, tolerance = 0.001)

  # Find parameters
  expect_identical(insight::find_parameters(x)$marginaleffects, "Species")

  # Find statistic
  expect_identical(insight::find_statistic(x), "z-statistic")

  # standardize names - "s.value" becomes "S"
  skip_if_not_installed("parameters", minimum_version = "0.24.0")
  skip_if_not_installed("marginaleffects", minimum_version = "0.24.0.6")

  expect_named(
    parameters::model_parameters(x),
    c(
      "rowid", "Parameter", "Comparison", "Coefficient", "SE", "Statistic", "p",
      "S", "CI", "CI_low", "CI_high", "Predicted", "Species",
      "Petal.Length", "Sepal.Width"
    )
  )
})
