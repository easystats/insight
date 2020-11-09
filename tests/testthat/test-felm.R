if (require("testthat") &&
  require("insight") &&
  require("lfe")) {
  x <- rnorm(1000)
  x2 <- rnorm(length(x))
  id <- factor(sample(20, length(x), replace = TRUE))
  firm <- factor(sample(13, length(x), replace = TRUE))
  id.eff <- rnorm(nlevels(id))
  firm.eff <- rnorm(nlevels(firm))
  u <- rnorm(length(x))
  y <- x + 0.5 * x2 + id.eff[id] + firm.eff[firm] + u

  x3 <- rnorm(length(x))
  x4 <- sample(12, length(x), replace = TRUE)

  Q <- 0.3 * x3 + x + 0.2 * x2 + id.eff[id] + 0.3 * log(x4) - 0.3 * y + rnorm(length(x), sd = 0.3)
  W <- 0.7 * x3 - 2 * x + 0.1 * x2 - 0.7 * id.eff[id] + 0.8 * cos(x4) - 0.2 * y + rnorm(length(x), sd = 0.6)

  # add them to the outcome
  y <- y + Q + W
  dat <- data.frame(y, x, x2, x3, x4, id, firm, Q, W)

  m1 <- felm(y ~ x + x2 | id + firm | (Q | W ~ x3 + factor(x4)), data = dat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(
        conditional = c("x", "x2"),
        instruments = c("Q", "W", "x3", "x4")
      )
    )
    expect_identical(find_predictors(m1, effects = "random"), list(random = c("id", "firm")))
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("x", "x2", "id", "firm", "Q", "W", "x3", "x4")
    )
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = c("id", "firm")))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m1)), c("id", "firm"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dat$y)
  })

  test_that("get_predictors", {
    expect_equal(
      colnames(get_predictors(m1)),
      c("x", "x2", "Q", "W", "x3", "x4")
    )
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 1000)
    expect_equal(
      colnames(get_data(m1)),
      c("y", "x", "x2", "id", "firm", "Q", "W", "x3", "x4")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 3)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ x + x2"),
        random = as.formula("~id + firm"),
        instruments = as.formula("~(Q | W ~ x3 + factor(x4))")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = c("x", "x2"),
        random = c("id", "firm"),
        instruments = c("(Q", "W  x3", "factor(x4))")
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("y", "x", "x2", "id", "firm", "(Q", "W  x3", "factor(x4))")
    )
  })


  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "y",
        conditional = c("x", "x2"),
        random = c("id", "firm"),
        instruments = c("Q", "W", "x3", "x4")
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("y", "x", "x2", "id", "firm", "Q", "W", "x3", "x4")
    )
  })


  test_that("n_obs", {
    expect_equal(n_obs(m1), 1000)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c("x", "x2", "Q(fit)", "W(fit)"))
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("x", "x2", "Q(fit)", "W(fit)")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
