if (require("testthat") && require("insight") && require("HRQoL")) {
  context("insight, BBmm")

  set.seed(18)
  k <- 100
  m <- 10
  x <- rnorm(k, 5, 3)
  j <- runif(k, 0, 5)
  fac <- sample(letters[1:4], k, TRUE)

  beta <- c(-10, 2)
  p <- 1 / (1 + exp(-(beta[1] + beta[2] * x)))
  phi <- 1.2

  y <- rBB(k, m, p, phi)
  z <- as.factor(rBI(k,4,0.5,2))

  dat <- data.frame(y, x, j, fac, z)

  m1 <- BBmm(
    fixed.formula = y ~ x,
    random.formula = ~ z,
    m = m,
    data = dat
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
    expect_true(model_info(m1)$is_betabinomial)
    expect_true(model_info(m1)$is_mixed)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "x"))
    expect_identical(find_predictors(m1, flatten = TRUE), "x")
    expect_identical(find_predictors(m1, effects = "random"), list(random = "z"))
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(
        conditional = "x",
        random = "z"
      )
    )
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = "z"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), dat[, "z", drop = FALSE])
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dat$y)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), "x")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), qlogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 100)
    expect_equal(colnames(get_data(m1)),  c("y", "x", "z"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ x"),
        random = as.formula("~z")
      )
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "y", conditional = "x", random = "z"))
    expect_equal(find_terms(m1, flatten = TRUE), c("y", "x", "z"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 100)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "x"),
        random = "z"
      )
    )
    expect_equal(nrow(get_parameters(m1)), 2)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "x"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "y",
        conditional = "x",
        random = "z"
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "extended likelihood", optimizer = "BB-NR"))
  })
}