if (requiet("testthat") &&
  requiet("insight") &&
  requiet("estimatr")) {
  data(mtcars)
  m1 <- iv_robust(mpg ~ gear + cyl | carb + wt, data = mtcars)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(
        conditional = c("gear", "cyl"),
        instruments = c("carb", "wt")
      )
    )
    expect_identical(
      find_predictors(m1, component = "instruments"),
      list(instruments = c("carb", "wt"))
    )
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("gear", "cyl", "carb", "wt")
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "mpg")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), mtcars$mpg)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("gear", "cyl", "carb", "wt"))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 32)
    expect_equal(
      colnames(get_data(m1)),
      c("mpg", "carb + wt", "gear", "cyl", "carb", "wt")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("mpg ~ gear + cyl"),
        instruments = as.formula("~carb + wt")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "mpg",
        conditional = c("gear", "cyl"),
        instruments = c("carb", "wt")
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("mpg", "gear", "cyl", "carb", "wt")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 32)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c("(Intercept)", "gear", "cyl"))
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "gear", "cyl")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_warning(expect_null(find_algorithm(m1)))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })

  if (requiet("ivreg")) {
    data("CigaretteDemand", package = "ivreg")
    m2 <- iv_robust(
      log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
      data = CigaretteDemand
    )
    m3 <- iv_robust(
      packs ~ log(rprice) + log(rincome) | salestax + log(rincome),
      data = CigaretteDemand
    )

    m4 <- lm_robust(
      log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
      data = CigaretteDemand
    )
    m5 <- lm_robust(
      packs ~ log(rprice) + log(rincome) | salestax + log(rincome),
      data = CigaretteDemand
    )

    test_that("get_loglikelihood", {
      expect_equal(as.numeric(get_loglikelihood(m2)), -286.56173, tolerance = 1e-3)
      expect_equal(as.numeric(get_loglikelihood(m3)), -206.39546, tolerance = 1e-3)
      expect_equal(as.numeric(get_loglikelihood(m4)), -286.55949, tolerance = 1e-3)
      expect_equal(as.numeric(get_loglikelihood(m5)), -205.63306, tolerance = 1e-3)
    })
  }

}
