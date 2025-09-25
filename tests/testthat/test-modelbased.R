skip_if_not_installed("modelbased")
skip_if_not_installed("marginaleffects")


test_that("modelbased get_parameters", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  params <- get_parameters(out)
  expect_identical(params$Parameter, c(3, 4, 5))
  expect_equal(params$Estimate, out$Mean, tolerance = 1e-5)

  out <- modelbased::estimate_slopes(mod, "wt")
  params <- get_parameters(out)
  expect_identical(params$Parameter, "wt")
  expect_equal(params$Estimate, out$Slope, tolerance = 1e-5)

  out <- modelbased::estimate_slopes(mod, "wt", by = "gear")
  params <- get_parameters(out)
  expect_identical(params$Parameter, c(3, 4, 5))
  expect_equal(params$Estimate, out$Slope, tolerance = 1e-5)

  out <- modelbased::estimate_contrasts(mod, "gear")
  params <- get_parameters(out)
  expect_identical(params$Parameter, c("4 - 3", "5 - 3", "5 - 4"))
  expect_equal(params$Estimate, out$Difference, tolerance = 1e-5)
})


test_that("modelbased get_statistic", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  params <- get_statistic(out)
  expect_identical(params$Parameter, c(3, 4, 5))
  expect_equal(params$Statistic, out[["t"]], tolerance = 1e-5)

  out <- modelbased::estimate_slopes(mod, "wt")
  params <- get_statistic(out)
  expect_identical(params$Parameter, "wt")
  expect_equal(params$Statistic, out[["t"]], tolerance = 1e-5)

  out <- modelbased::estimate_slopes(mod, "wt", by = "gear")
  params <- get_statistic(out)
  expect_identical(params$Parameter, c(3, 4, 5))
  expect_equal(params$Statistic, out[["t"]], tolerance = 1e-5)

  out <- modelbased::estimate_contrasts(mod, "gear")
  params <- get_statistic(out)
  expect_identical(params$Parameter, c("4 - 3", "5 - 3", "5 - 4"))
  expect_equal(params$Statistic, out[["t"]], tolerance = 1e-5)
})
