skip_on_cran()
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


test_that("modelbased find_parameters", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  params <- find_parameters(out)
  expect_identical(params, list(conditional = c(3, 4, 5)))

  out <- modelbased::estimate_slopes(mod, "wt")
  params <- find_parameters(out)
  expect_identical(params, list(conditional = "wt"))

  out <- modelbased::estimate_slopes(mod, "wt", by = "gear")
  params <- find_parameters(out)
  expect_identical(params, list(conditional = c(3, 4, 5)))

  out <- modelbased::estimate_contrasts(mod, "gear")
  params <- find_parameters(out)
  expect_identical(params, list(conditional = c("4 - 3", "5 - 3", "5 - 4")))
})


test_that("modelbased find_formula", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  expect_equal(
    find_formula(out),
    list(conditional = mpg ~ as.factor(gear) + wt),
    ignore_attr = TRUE
  )
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


test_that("modelbased get_df", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  params <- get_df(out)
  expect_identical(params, c(28L, 28L, 28L))
})


test_that("modelbased get_vcov", {
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) + wt, data = mtcars)

  out <- modelbased::estimate_means(mod, "gear")
  params <- get_varcov(out)
  expect_equal(
    params,
    matrix(
      c(
        0.797266841554893,
        -0.20517609342839,
        -0.199732557238491,
        -0.20517609342839,
        0.890647337964126,
        0.177620673860557,
        -0.199732557238491,
        0.177620673860558,
        1.87255360193076
      ),
      nrow = 3
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
