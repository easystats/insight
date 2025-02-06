skip_if_not_installed("ggeffects", minimum_version = "1.3.2")

test_that("get_parameters, hypothesis_test", {
  data(iris)
  mgg <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)

  out <- ggeffects::test_predictions(mgg, "Sepal.Length", by = "Species")

  param <- get_parameters(out)
  expect_named(param, c("Level1", "Level2", "Estimate"))
  expect_identical(
    as.character(param$Level1),
    c("versicolor", "virginica", "virginica")
  )
  expect_equal(
    param$Estimate,
    c(-0.47881, -0.56664, -0.08783),
    tolerance = 1e-3
  )

  param <- get_parameters(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Estimate"))
  expect_identical(
    as.character(param$Parameter),
    c(
      "Level1 [versicolor], Level2 [setosa]", "Level1 [virginica], Level2 [setosa]",
      "Level1 [virginica], Level2 [versicolor]"
    )
  )
  expect_equal(
    param$Estimate,
    c(-0.47881, -0.56664, -0.08783),
    tolerance = 1e-3
  )
})


test_that("get_statistic, hypothesis_test", {
  data(iris)
  mgg <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)

  out <- ggeffects::hypothesis_test(mgg, "Sepal.Length", by = "Species")

  param <- get_statistic(out)
  expect_named(param, c("Level1", "Level2", "Statistic"))
  expect_equal(
    param$Statistic,
    c(-3.58238, -4.48999, -0.90468),
    tolerance = 1e-3
  )
  expect_identical(
    as.character(param$Level1),
    c("versicolor", "virginica", "virginica")
  )

  param <- get_statistic(out, merge_parameters = TRUE)
  expect_named(param, c("Parameter", "Statistic"))
  expect_identical(
    as.character(param$Parameter),
    c(
      "Level1 [versicolor], Level2 [setosa]", "Level1 [virginica], Level2 [setosa]",
      "Level1 [virginica], Level2 [versicolor]"
    )
  )
  expect_equal(
    param$Statistic,
    c(-3.58238, -4.48999, -0.90468),
    tolerance = 1e-3
  )
})
