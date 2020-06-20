.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && require("testthat") && require("insight") && require("PROreg")) {
  set.seed(123)

  # defining the parameters
  k <- 100
  m <- 10
  phi <- 0.5
  beta <- c(1.5, -1.1)
  sigma <- 0.5

  # simulating the covariate and random effects
  x <- runif(k, 0, 10)
  X <- model.matrix(~x)
  z <- as.factor(rBI(k, 4, 0.5, 2))
  Z <- model.matrix(~ z - 1)
  u <- rnorm(5, 0, sigma)

  # the linear predictor and simulated response variable
  eta <- beta[1] + beta[2] * x + crossprod(t(Z), u)
  p <- 1 / (1 + exp(-eta))
  y <- rBB(k, m, p, phi)
  dat <- data.frame(cbind(y, x, z))
  dat$z <- as.factor(dat$z)

  # apply the model
  m1 <- BBmm(
    fixed.formula = y ~ x,
    random.formula = ~z,
    m = m,
    data = dat
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_betabinomial)
    expect_true(model_info(m1)$is_binomial)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "x"))
    expect_identical(find_predictors(m1, effects = "random"), list(random = "z"))
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = "z"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), dat["z"])
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
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-3)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 100)
    expect_equal(colnames(get_data(m1)), c("y", "x", "z"))
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

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "y",
        conditional = "x",
        random = "z"
      )
    )
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "x", "z"))
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
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "x"))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = "x",
        random = "z"
      )
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
