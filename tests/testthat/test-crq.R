if (require("testthat") &&
  require("insight") &&
  require("quantreg")) {
  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- 5 + x + rnorm(n)
  c <- 4 + x + rnorm(n)
  d <- (y > c)

  dat <- data.frame(y, x, c, d)

  # model
  m1 <- crq(survival::Surv(pmax(y, c), d, type = "left") ~ x, method = "Portnoy", data = dat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_censored)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(conditional = "x")
    )
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })


  test_that("find_response", {
    expect_identical(find_response(m1), "Surv(pmax(y, c), d, type = \"left\")")
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), "x")
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 200)
    expect_equal(
      colnames(get_data(m1)),
      c("y", "x", "c", "d")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("survival::Surv(pmax(y, c), d, type = \"left\") ~ x")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "Surv(pmax(y, c), d, type = \"left\")",
        conditional = "x"
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("Surv(pmax(y, c), d, type = \"left\")", "x")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 200)
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
      list(conditional = c("(Intercept)", "x"))
    )
    expect_equal(nrow(get_parameters(m1)), 8)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x")
    )
    expect_equal(
      get_parameters(m1)$Component,
      c("tau (0.2)", "tau (0.2)", "tau (0.4)", "tau (0.4)", "tau (0.6)", "tau (0.6)", "tau (0.8)", "tau (0.8)")
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })

  test_that("get_statistic", {
    expect_equal(
      get_statistic(m1)$Parameter,
      c("(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x")
    )
    expect_equal(
      get_statistic(m1)$Statistic,
      c(67.64633, 5.88482, 56.8453, 10.05249, 76.86565, 9.78366, 53.05556, 12.83912),
      tolerance = 1e-3
    )
  })
}
