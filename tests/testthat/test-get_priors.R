test_that("get_priors", {
  skip_on_os(os = c("mac", "windows"))
  skip_on_cran()
  skip_if_not_installed("brms")

  set.seed(123)

  model <- suppressMessages(brms::brm(mpg ~ wt, data = mtcars, seed = 1, refresh = 0))
  priors <- insight::get_priors(model)

  expect_equal(priors$Location, c(19.2, NA, 0), tolerance = 1e-3)
  expect_equal(priors$Distribution, c("student_t", "uniform", "student_t"))
  expect_equal(priors$Parameter, c("b_Intercept", "b_wt", "sigma"))
})
