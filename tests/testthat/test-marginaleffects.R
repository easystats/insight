if (requiet("marginaleffects") && requiet("insight") && requiet("testthat")) {
  test_that("marginaleffects", {
    m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    x <- marginaleffects(m,
                         variables = "Petal.Length",
                         newdata = insight::get_datagrid(m, at = "Species"))
    # Equivalent in emmeans
    x2 <- emmeans::emtrends(m, var = "Petal.Length", specs = ~Species + Petal.Length)

    # Get parameters
    p1 <- insight::get_parameters(x)
    p2 <- insight::get_parameters(x2)

    expect_equal(names(p1), names(p2))
    expect_equal(max(p1$Estimate - p2$Estimate), 0, tolerance = 0.001)

    # Find parameters
    expect_equal(insight::find_parameters(x)$Species, unique(iris$Species))
  })
}
