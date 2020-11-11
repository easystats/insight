if (require("testthat") &&
  require("insight") &&
  require("crch")) {

  data("RainIbk")
  RainIbk$sqrtensmean <- apply(sqrt(RainIbk[, grep("^rainfc", names(RainIbk))]), 1, mean)
  RainIbk$sqrtenssd <- apply(sqrt(RainIbk[, grep("^rainfc", names(RainIbk))]), 1, sd)

  m1 <- crch(sqrt(rain) ~ sqrtensmean, data = RainIbk, dist = "gaussian")

  test_that("model_info", {
    expect_false(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_censored)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("sqrtensmean")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("sqrtensmean"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "rain")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), RainIbk$rain)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("sqrtensmean"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 4971)
    expect_equal(colnames(get_data(m1)), c("rain", "sqrtensmean"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("sqrt(rain) ~ sqrtensmean")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "sqrt(rain)",
        conditional = c("sqrtensmean")
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("sqrt(rain)", "sqrtensmean")
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "rain",
        conditional = c("sqrtensmean")
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("rain", "sqrtensmean")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 4971)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "sqrtensmean", "(scale)_(Intercept)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "sqrtensmean", "(scale)_(Intercept)")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })
}
