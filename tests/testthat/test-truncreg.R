if (require("testthat") &&
  require("insight") &&
  require("truncreg") &&
  require("survival")) {
  context("insight, truncreg")

  data("tobin", package = "survival")
  m1 <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("age", "quant")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("age", "quant"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "durable")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 7)
    expect_equal(colnames(get_data(m1)), c("durable", "age", "quant"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("durable ~ age + quant"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "durable",
      conditional = c("age", "quant")
    ))
    expect_equal(find_terms(m1, flatten = TRUE), c("durable", "age", "quant"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 7)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c(
        "(Intercept)", "age", "quant", "sigma"
      ))
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$parameter,
      c("(Intercept)", "age", "quant", "sigma")
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
