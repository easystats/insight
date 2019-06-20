if (require("testthat") && require("insight") && require("robustbase")) {
  context("insight, model_info")

  data(carrots)

  m1 <- glmrob(
    cbind(success, total - success) ~ logdose + block,
    family = binomial,
    data = carrots,
    method = "Mqle",
    control = glmrobMqle.control(tcc = 1.2)
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("logdose", "block")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("logdose", "block"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "cbind(success, total - success)")
    expect_identical(find_response(m1, combine = FALSE), c("success", "total"))
  })

  test_that("get_response", {
    expect_equal(get_response(m1), carrots[, c("success", "total")])
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("logdose", "block"))
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 24)
    expect_equal(
      colnames(get_data(m1)),
      c("cbind(success, total - success)", "logdose", "block", "success", "total")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("cbind(success, total - success) ~ logdose + block"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "cbind(success, total - success)", conditional = c("logdose", "block")))
    expect_equal(find_terms(m1, flatten = TRUE), c("cbind(success, total - success)", "logdose", "block"))
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(response = c("success", "total"), conditional = c("logdose", "block")))
    expect_equal(find_variables(m1, flatten = TRUE), c("success", "total", "logdose", "block"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 24)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), qlogis(.2), tolerance = 1e-5)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "logdose", "blockB2", "blockB3")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "logdose", "blockB2", "blockB3"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "Mqle"))
  })
}
