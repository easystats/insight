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


test_that("get_statistic, hypothesis_test", {
  data(iris)
  mgg <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)

  out <- ggeffects::hypothesis_test(
    ggeffects::ggpredict(mgg, c("Sepal.Length", "Species")),
    test = NULL
  )

  param <- get_statistic(out)
  expect_named(param, c("Sepal.Length", "Species", "Statistic"))
  expect_identical(
    param$Sepal.Length,
    c("slope", "slope", "slope")
  )
  expect_identical(
    param$Species,
    structure(1:3, levels = c("setosa", "versicolor", "virginica"), class = "factor")
  )
  expect_equal(
    param$Statistic,
    c(7.23551, 4.24211, 3.79015),
    tolerance = 1e-3
  )

  param <- get_statistic(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Statistic"))
  expect_identical(
    param$Parameter,
    c(
      "Sepal.Length [slope], Species [setosa]", "Sepal.Length [slope], Species [versicolor]",
      "Sepal.Length [slope], Species [virginica]"
    )
  )
  expect_equal(
    param$Statistic,
    c(7.23551, 4.24211, 3.79015),
    tolerance = 1e-3
  )

  out <- ggeffects::hypothesis_test(ggeffects::ggpredict(mgg, c("Sepal.Length", "Species")))

  param <- get_statistic(out)
  expect_named(param, c("Sepal.Length", "Species", "Statistic"))
  expect_identical(
    param$Sepal.Length,
    c("slope", "slope", "slope")
  )
  expect_equal(
    param$Statistic,
    c(3.58262, 4.48915, 0.90475),
    tolerance = 1e-3
  )

  param <- get_statistic(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Statistic"))
})
