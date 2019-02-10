if (require("testthat") && require("insight") && require("betareg")) {
  context("insight, model_info")

  data("GasolineYield")
  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)

  test_that("model_info", {
    expect_true(model_info(m1)$is_beta)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("batch", "temp")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("batch", "temp"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "yield")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), GasolineYield$yield)
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(.2), plogis(.2))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 32)
    expect_equal(colnames(get_data(m1)), c("yield", "batch", "temp"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("yield ~ batch + temp"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "yield", conditional = c("batch", "temp")))
    expect_equal(find_terms(m1, flatten = TRUE), c("yield", "batch", "temp"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 32)
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "batch1", "batch2", "batch3", "batch4", "batch5", "batch6", "batch7", "batch8", "batch9", "temp", "(phi)")
      ))
    expect_equal(nrow(get_parameters(m1)), 12)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "batch1", "batch2", "batch3", "batch4", "batch5", "batch6", "batch7", "batch8", "batch9", "temp", "(phi)"))
  })

}
