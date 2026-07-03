skip_on_cran()
skip_on_os(os = "mac")

test_that("get_priors, brms", {
  skip_on_os(os = "mac")
  skip_if_not_installed("brms")

  set.seed(123)

  model <- suppressMessages(brms::brm(mpg ~ wt, data = mtcars, seed = 1, refresh = 0))
  priors <- insight::get_priors(model)

  expect_equal(priors$Location, c(19.2, NA, 0), tolerance = 1e-3)
  expect_equal(priors$Distribution, c("student_t", "uniform", "student_t"))
  expect_equal(priors$Parameter, c("b_Intercept", "b_wt", "sigma"))
})

test_that("get_priors, rstanarm", {
  skip_if_not_installed("rstanarm")
  set.seed(123)

  model <- suppressMessages(rstanarm::stan_glm(
    mpg ~ wt,
    data = mtcars,
    prior = rstanarm::student_t(5, 1, 2),
    seed = 123,
    refresh = 0,
    open_progress = FALSE
  ))
  priors <- insight::get_priors(model)

  expect_equal(priors$Location, c(20.09062, 1), tolerance = 1e-3)
  expect_equal(priors$Distribution, c("normal", "student_t"))
  expect_equal(priors$df, c(NA, 5))
})

test_that("get_priors, stanmvref", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")

  model <- insight::download_model("stanmvreg_1")
  skip_if(is.null(model))

  priors <- get_priors(model)
  expect_named(
    priors,
    c("Parameter", "Distribution", "Location", "Scale", "Adjusted_Scale", "Response")
  )
  expect_equal(
    priors$Adjusted_Scale,
    c(10.86201, 5.57448, 0.7581, 1.39362, 0.38906),
    tolerance = 1e-3
  )
  expect_identical(
    priors$Parameter,
    c("y1|(Intercept)", "y2|(Intercept)", "y1|year", "y2|sexf", "y2|year")
  )
  expect_identical(priors$Response, c("y1", "y2", "y1", "y2", "y2"))
})
