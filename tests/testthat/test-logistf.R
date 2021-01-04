if (require("testthat") &&
  require("insight") &&
  require("logistf")) {
  data(sex2)
  m1 <- logistf(case ~ age + oc + vic + vicl + vis + dia, data = sex2)

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
    expect_true(model_info(m1)$is_logit)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c(
      "age", "oc", "vic", "vicl", "vis", "dia"
    )))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("age", "oc", "vic", "vicl", "vis", "dia")
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
    expect_identical(find_response(m1), "case")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), sex2$case)
  })

  test_that("get_predictors", {
    expect_equal(
      colnames(get_predictors(m1)),
      c("age", "oc", "vic", "vicl", "vis", "dia")
    )
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 239)
    expect_equal(
      colnames(get_data(m1)),
      c("case", "age", "oc", "vic", "vicl", "vis", "dia")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equivalent(
      find_formula(m1),
      list(conditional = as.formula("case ~ age + oc + vic + vicl + vis + dia")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "case",
      conditional = c("age", "oc", "vic", "vicl", "vis", "dia")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("case", "age", "oc", "vic", "vicl", "vis", "dia")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 239)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("linkinverse", {
    expect_false(is.null(link_inverse(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "age", "oc", "vic", "vicl", "vis", "dia")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "age", "oc", "vic", "vicl", "vis", "dia")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "Penalized ML"))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "chi-squared statistic")
  })
}
