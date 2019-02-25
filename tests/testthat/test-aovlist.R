if (require("testthat") && require("insight") && require("stats")) {
  context("insight, model_info")

  data(npk)
  m1 <- aov(yield ~  N*P*K + Error(block), data = npk)
  m2 <- aov(yield ~  N*P*K, data = npk)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_anova)
    expect_true(model_info(m2)$is_linear)
    expect_true(model_info(m2)$is_anova)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("N", "P", "K", "block")))
    expect_identical(find_predictors(m2), list(conditional = c("N", "P", "K")))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "yield")
    expect_identical(find_response(m2), "yield")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), npk$yield)
    expect_equal(get_response(m2), npk$yield)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("N", "P", "K", "block"))
    expect_equal(colnames(get_predictors(m2)), c("N", "P", "K"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
    expect_equal(link_inverse(m2)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 24)
    expect_equal(nrow(get_data(m2)), 24)
    expect_equal(colnames(get_data(m1)), c("yield", "N", "P", "K", "block"))
    expect_equal(colnames(get_data(m1)), c("yield", "N", "P", "K"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("yield ~ N * P * K + Error(block)"))
    )
    expect_length(find_formula(m2), 1)
    expect_equal(
      find_formula(m2),
      list(conditional = as.formula("yield ~ N * P * K"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "yield", conditional = c("N", "P", "K", "block")))
    expect_equal(find_terms(m1, flatten = TRUE), c("yield", "N", "P", "K", "block"))
    expect_equal(find_terms(m2), list(response = "yield", conditional = c("N", "P", "K")))
    expect_equal(find_terms(m2, flatten = TRUE), c("yield", "N", "P", "K"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 24)
    expect_equal(n_obs(m2), 24)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
    expect_false(is.null(link_function(m2)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = "(Intercept)",
        between = "N1:P1:K1",
        within = c("N1", "P1", "K1", "N1:P1", "N1:K1", "P1:K1")
      )
    )
    expect_equal(length(get_parameters(m1)), 3)
    expect_equal(
      find_parameters(m2),
      list(
        conditional = c("(Intercept)", "N1", "P1", "K1", "N1:P1", "N1:K1", "P1:K1", "N1:P1:K1")
      )
    )
    expect_equal(nrow(get_parameters(m2)), 8)
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
    expect_false(is_multivariate(m2))
  })
}
