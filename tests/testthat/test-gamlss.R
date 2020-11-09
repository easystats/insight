if (require("testthat") &&
  require("insight") &&
  require("gamlss")) {
  data(abdom)
  m1 <-
    gamlss(
      y ~ pb(x),
      sigma.formula =  ~ pb(x),
      family = BCT,
      data = abdom,
      method = mixed(1, 20)
    )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "x", sigma = "x"))
    expect_identical(find_predictors(m1, flatten = TRUE), "x")
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), abdom$y)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), "x")
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 610)
    expect_equal(colnames(get_data(m1)), c("y", "x"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 4)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ pb(x)"),
        sigma = as.formula("~pb(x)"),
        nu = as.formula("~1"),
        tau = as.formula("~1")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "y",
        conditional = "x",
        sigma = "x"
      )
    )
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "x"))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = "pb(x)",
        sigma = "pb(x)",
        nu = "1",
        tau = "1"
      )
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 610)
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
      list(
        conditional = c("(Intercept)", "pb(x)"),
        sigma = c("(Intercept)", "pb(x)"),
        nu = "(Intercept)",
        tau = "(Intercept)"
      )
    )
    expect_equal(nrow(get_parameters(m1)), 6)
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "mixed"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
