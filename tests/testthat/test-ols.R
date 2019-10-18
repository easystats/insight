if (require("testthat") &&
  require("insight") &&
  require("rms")) {
  context("insight, model_info")

  data(mtcars)
  m1 <- ols(mpg ~ rcs(hp, 3) * cyl + wt, data = mtcars)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("hp", "cyl", "wt")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("hp", "cyl", "wt"))
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
    expect_equal(colnames(get_predictors(m1)), c("hp", "cyl", "wt"))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 32)
    expect_equal(colnames(get_data(m1)), c("mpg", "cyl", "wt", "hp"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("mpg ~ rcs(hp, 3) * cyl + wt"))
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(
      response = "mpg",
      conditional = c("hp", "cyl", "wt")
    ))
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("mpg", "hp", "cyl", "wt")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 32)
  })

  test_that("linkfun", {
    expect_equal(link_function(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("Intercept", "hp", "hp'", "cyl", "wt", "hp * cyl", "hp' * cyl")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("Intercept", "hp", "hp'", "cyl", "wt", "hp * cyl", "hp' * cyl")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "OLS"))
  })

  # TO DO
  # test_that("find_statistic", {
  #   expect_null(find_statistic(m1))
  # })
}
