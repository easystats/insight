skip_if_not_installed("ggeffects")

test_that("get_parameters, hypothesis_test", {
  data(iris)
  mgg <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)

  out <- ggeffects::hypothesis_test(
    ggeffects::ggpredict(mgg, c("Sepal.Length", "Species")),
    test = NULL
  )

  param <- get_parameters(out)
  expect_named(param, c("Sepal.Length", "Species", "Estimate"))
  expect_identical(
    param$Sepal.Length,
    c("slope", "slope", "slope")
  )
  expect_identical(
    param$Species,
    structure(1:3, levels = c("setosa", "versicolor", "virginica"), class = "factor")
  )
  expect_equal(
    param$Estimate,
    c(0.79853, 0.31972, 0.23189),
    tolerance = 1e-3
  )

  param <- get_parameters(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Estimate"))
  expect_identical(
    param$Parameter,
    c(
      "Sepal.Length [slope], Species [setosa]", "Sepal.Length [slope], Species [versicolor]",
      "Sepal.Length [slope], Species [virginica]"
    )
  )
  expect_equal(
    param$Estimate,
    c(0.79853, 0.31972, 0.23189),
    tolerance = 1e-3
  )

  out <- ggeffects::hypothesis_test(ggeffects::ggpredict(mgg, c("Sepal.Length", "Species")))

  param <- get_parameters(out)
  expect_named(param, c("Sepal.Length", "Species", "Estimate"))
  expect_identical(
    param$Sepal.Length,
    c("slope", "slope", "slope")
  )
  expect_equal(
    param$Estimate,
    c(0.47881, 0.56664, 0.08783),
    tolerance = 1e-3
  )

  param <- get_parameters(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Estimate"))
})
