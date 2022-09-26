testthat::skip_on_cran()

if (requiet("marginaleffects") && requiet("insight") && requiet("emmeans") && requiet("testthat")) {
  test_that("marginaleffects", {
    m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    x <- marginaleffects(m,
      variables = "Petal.Length",
      newdata = insight::get_datagrid(m, at = "Species")
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
    expect_equal(insight::find_parameters(x)$marginaleffects, "Species")

    # Find statistic
    expect_equal(insight::find_statistic(x), "z-statistic")
  })
}
