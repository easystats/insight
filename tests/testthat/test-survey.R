if (requiet("testthat") &&
  requiet("insight") &&
  requiet("survey")) {
  data(api)
  dstrat <-
    svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    )

  m1 <- svyglm(api00 ~ ell + meals + mobility, design = dstrat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("ell", "meals", "mobility")))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "api00")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), apistrat$api00)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 200)
    expect_equal(
      colnames(get_data(m1)),
      c("api00", "ell", "meals", "mobility", "(weights)")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("api00 ~ ell + meals + mobility")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "api00",
      conditional = c("ell", "meals", "mobility")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("api00", "ell", "meals", "mobility")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 200)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c(
        "(Intercept)", "ell", "meals", "mobility"
      ))
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "ell", "meals", "mobility")
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
