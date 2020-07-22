if (require("testthat") &&
  require("insight") &&
  require("quantreg")) {

  data(stackloss)
  m1 <-
    rq(stack.loss ~ Air.Flow + Water.Temp,
      data = stackloss,
      tau = .25
    )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(conditional = c("Air.Flow", "Water.Temp"))
    )
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("Air.Flow", "Water.Temp")
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "stack.loss")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), stackloss$stack.loss)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("Air.Flow", "Water.Temp"))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 21)
    expect_equal(
      colnames(get_data(m1)),
      c("stack.loss", "Air.Flow", "Water.Temp")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("stack.loss ~ Air.Flow + Water.Temp"))
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "stack.loss",
        conditional = c("Air.Flow", "Water.Temp")
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("stack.loss", "Air.Flow", "Water.Temp")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 21)
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
      list(conditional = c(
        "(Intercept)", "Air.Flow", "Water.Temp"
      ))
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "Air.Flow", "Water.Temp")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "br"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
