if (require("testthat") &&
  require("insight") &&
  require("mgcv")) {

  # model 1
  set.seed(123)
  dat <<- gamSim(1, n = 400, dist = "normal", scale = 2)
  m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

  # model 2
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x) exp(2 * x)
  f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
  n <- 500
  set.seed(5)
  x0 <- runif(n)
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)

  eta1 <- f0(x0) + f1(x1) - 3
  p <- binomial()$linkinv(eta1)
  y <- as.numeric(runif(n) < p) ## 1 for presence, 0 for absence

  ind <- y > 0
  eta2 <- f2(x2[ind]) / 3
  y[ind] <- rpois(exp(eta2), exp(eta2))
  m2 <- gam(list(y ~ s(x2) + s(x3),  ~ s(x0) + s(x1)), family = ziplss())

  # model 3
  set.seed(123)
  V <- matrix(c(2, 1, 1, 2), 2, 2)
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x) exp(2 * x)
  f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10

  n <- 300
  x0 <- runif(n)
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  y <- matrix(0, n, 2)

  for (i in 1:n) {
    mu <- c(f0(x0[i]) + f1(x1[i]), f2(x2[i]))
    y[i, ] <- rmvn(1, mu, V)
  }

  dat3 <<- data.frame(y0 = y[, 1], y1 = y[, 2], x0 = x0, x1 = x1, x2 = x2, x3 = x3)
  m3 <- gam(list(y0 ~ s(x0) + s(x1), y1 ~ s(x2) + s(x3)), family = mvn(d = 2), data = dat3)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
    expect_true(model_info(m2)$is_count)
    expect_true(model_info(m3)$is_multivariate)
  })

  test_that("clean_names", {
    expect_equal(clean_names(m1), c("y", "x0", "x1", "x2", "x3"))
    expect_equal(clean_names(m2), c("y", "x2", "x3", "x0", "x1"))
    expect_equal(clean_names(m3), c("y0", "y1", "x0", "x1", "x2", "x3"))
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2", "x3")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("x0", "x1", "x2", "x3")
    )
    expect_null(find_predictors(m1, effects = "random"))

    expect_identical(find_predictors(m2), list(conditional = c("x2", "x3"), zero_inflated = c("x0", "x1")))
    expect_identical(find_predictors(m2, flatten = TRUE), c("x2", "x3", "x0", "x1"))
    expect_null(find_predictors(m2, effects = "random"))

    expect_identical(find_predictors(m3), list(y0 = list(conditional = c("x0", "x1")), y1 = list(conditional = c("x2", "x3"))))
    expect_identical(find_predictors(m3, flatten = TRUE), c("x0", "x1", "x2", "x3"))
    expect_null(find_predictors(m3, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
    expect_identical(find_response(m2), "y")
    expect_identical(find_response(m3), c(y0 = "y0", y1 = "y1"))
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dat$y)
    expect_equal(length(get_response(m2)), 500)
    expect_equal(ncol(get_response(m3)), 2)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
    expect_equal(link_inverse(m2)(.2), .2, tolerance = 1e-5)
    expect_equal(link_inverse(m3)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 400)
    expect_equal(colnames(get_data(m1)), c("y", "x0", "x1", "x2", "x3"))
    expect_equal(nrow(get_data(m2)), 500)
    expect_equal(colnames(get_data(m2)), c("y", "x2", "x3", "x0", "x1"))
    expect_equal(nrow(get_data(m3)), 300)
    expect_equal(colnames(get_data(m3)), c("y0", "x0", "x1", "x2", "x3", "y1"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("y ~ s(x0) + s(x1) + s(x2) + s(x3)"))
    )
    expect_length(find_formula(m2), 2)
    expect_equal(
      find_formula(m2),
      list(
        conditional = as.formula("y ~ s(x2) + s(x3)"),
        zero_inflated = as.formula("~s(x0) + s(x1)")
      )
    )
    expect_length(find_formula(m3), 2)
    expect_equal(
      find_formula(m3),
      structure(list(
        y0 = list(conditional = as.formula("y0 ~ s(x0) + s(x1)")),
        y1 = list(conditional = as.formula("y1 ~ s(x2) + s(x3)"))
      ),
      is_mv = "1"
      )
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(response = "y", conditional = c("x0", "x1", "x2", "x3")))
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2", "x3"))
    expect_equal(find_variables(m2), list(response = "y", conditional = c("x2", "x3"), zero_inflated = c("x0", "x1")))
    expect_equal(find_variables(m2, flatten = TRUE), c("y", "x2", "x3", "x0", "x1"))
    expect_equal(find_variables(m3), list(response = c(y0 = "y0", y1 = "y1"), y0 = list(conditional = c("x0", "x1")), y1 = list(conditional = c("x2", "x3"))))
    expect_equal(find_variables(m3, flatten = TRUE), c("y0", "y1", "x0", "x1", "x2", "x3"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 400)
    expect_equal(n_obs(m2), 500)
    expect_equal(n_obs(m3), 300)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = "(Intercept)",
        smooth_terms = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 5)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)")
    )
    expect_equal(nrow(get_parameters(m1, "smooth_terms")), 4)

    expect_equal(
      find_parameters(m2),
      list(
        conditional = c("(Intercept)", "(Intercept).1"),
        smooth_terms = c("s(x2)", "s(x3)", "s.1(x0)", "s.1(x1)")
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
    expect_false(is_multivariate(m2))
    expect_true(is_multivariate(m3))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
      )
    )
    expect_equal(
      find_terms(m2),
      list(
        response = "y",
        conditional = c("s(x2)", "s(x3)"),
        zero_inflated = c("s(x0)", "s(x1)")
      )
    )
    expect_equal(
      find_terms(m3),
      list(
        y0 = list(response = "y0", conditional = c("s(x0)", "s(x1)")),
        y1 = list(response = "y1", conditional = c("s(x2)", "s(x3)"))
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(
      find_algorithm(m1),
      list(algorithm = "GCV", optimizer = "magic")
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
