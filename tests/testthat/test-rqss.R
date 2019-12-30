if (require("testthat") &&
  require("insight") &&
  require("quantreg") &&
  require("tripack")) {
  context("insight, model_info")

  data("CobarOre")
  set.seed(123)
  CobarOre$w <- rnorm(nrow(CobarOre))

  # model
  m1 <- rqss(z ~ w + qss(cbind(x, y), lambda = .08), data = CobarOre)


  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(conditional = c("w", "x", "y"))
    )
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("w", "x", "y")
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
    expect_identical(find_response(m1), "z")
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("z ~ w + qss(cbind(x, y), lambda = 0.08)"))
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(response = "z", conditional = c("w", "qss(cbind(x, y), lambda = 0.08)"))
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("z", "w", "qss(cbind(x, y), lambda = 0.08)")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 38)
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
      list(conditional = c("(Intercept)", "w"), smooth_terms = "cbind(x, y)")
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "w", "cbind(x, y)")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "sfn"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
