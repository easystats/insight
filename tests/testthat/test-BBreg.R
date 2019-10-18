if (require("testthat") &&
  require("insight") &&
  require("HRQoL")) {
  context("insight, BBreg")

  set.seed(18)
  k <- 1000
  m <- 10
  x <- rnorm(k, 5, 3)
  x2 <- rnorm(k, 7, 3.5)
  j <- runif(k, 0, 5)
  fac <- sample(letters[1:4], k, TRUE)

  beta <- c(-10, 2)
  p <- 1 / (1 + exp(-(beta[1] + beta[2] * x)))
  phi <- 1.2

  y <- HRQoL::rBB(k, m, p, phi)
  dat <- data.frame(y, x, x2, j, fac)

  m1 <- BBreg(y ~ x + x2 + j + fac, m, data = dat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
    expect_true(model_info(m1)$is_betabinomial)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("x", "x2", "j", "fac")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("x", "x2", "j", "fac"))
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
    expect_equal(get_response(m1), dat$y)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("x", "x2", "j", "fac"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), qlogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 1000)
    expect_equal(colnames(get_data(m1)), c("y", "x", "x2", "j", "fac"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("y ~ x + x2 + j + fac"))
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(
      response = "y",
      conditional = c("x", "x2", "j", "fac")
    ))
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("y", "x", "x2", "j", "fac")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 1000)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("Intercept", "x", "x2", "j", "facb", "facc", "facd")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("Intercept", "x", "x2", "j", "facb", "facc", "facd")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = c("x", "x2", "j", "fac")
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "ML"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
