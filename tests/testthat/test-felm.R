if (require("testthat") && require("insight") && require("lfe")) {
  context("insight, model_info")

  x <- rnorm(1000)
  x2 <- rnorm(length(x))
  id <- factor(sample(20, length(x), replace = TRUE))
  firm <- factor(sample(13, length(x), replace = TRUE))
  id.eff <- rnorm(nlevels(id))
  firm.eff <- rnorm(nlevels(firm))
  u <- rnorm(length(x))
  y <- x + 0.5 * x2 + id.eff[id] + firm.eff[firm] + u
  dat <- data.frame(y, x, x2, id, firm)

  m1 <- felm(y ~ x + x2 | id + firm, data = dat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("x", "x2")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("x", "x2"))
    expect_identical(find_predictors(m1, effects = "random"), list(random = c("id", "firm")))
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = c("id", "firm")))
  })

  test_that("get_random", {
    expect_equal(colnames(get_random(m1)), c("id", "firm"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dat$y)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("x", "x2"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 1000)
    expect_equal(colnames(get_data(m1)), c("y", "x", "x2", "id", "firm"))
    expect_equal(colnames(get_data(m1, effects = "fixed")), c("y", "x", "x2"))
    expect_equal(colnames(get_data(m1, effects = "random")), c("id", "firm"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ x + x2"),
        random = as.formula("~id + firm")
      )
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "y", conditional = c("x", "x2"), random = c("id", "firm")))
    expect_equal(find_terms(m1, flatten = TRUE), c("y", "x", "x2", "id", "firm"))
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
      list(
        conditional = c("x", "x2")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 2)
    expect_equal(get_parameters(m1)$parameter, c("x", "x2"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })
}
