if (require("testthat") && require("insight") && require("pscl")) {
  context("insight, model_info")

  data("bioChemists")

  m1 <- hurdle(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("model_info", {
    expect_true(model_info(m1)$is_poisson)
    expect_true(model_info(m1)$is_zeroinf)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("fem", "mar", "kid5", "ment"), zero_inflated = c("kid5", "phd")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("fem", "mar", "kid5", "ment", "phd"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "art")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), bioChemists$art)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 915)
    expect_equal(colnames(get_data(m1)), c("art", "fem", "mar", "kid5", "ment", "phd"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("art ~ fem + mar + kid5 + ment"),
        zero_inflated = as.formula("~kid5 + phd")
      )
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "art", conditional = c("fem", "mar", "kid5", "ment"), zero_inflated = c("kid5", "phd")))
    expect_equal(find_terms(m1, flatten = TRUE), c("art", "fem", "mar", "kid5", "ment", "phd"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 915)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("count_(Intercept)", "count_femWomen", "count_marMarried", "count_kid5", "count_ment"),
        zero_inflated = c("zero_(Intercept)", "zero_kid5", "zero_phd")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 8)
    expect_equal(nrow(get_parameters(m1, component = "zi")), 3)
    expect_equal(get_parameters(m1)$parameter, c("count_(Intercept)", "count_femWomen", "count_marMarried", "count_kid5", "count_ment", "zero_(Intercept)", "zero_kid5", "zero_phd"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "art",
        conditional = c("fem", "mar", "kid5", "ment"),
        zero_inflated = c("kid5", "phd")
      )
    )
  })
}
