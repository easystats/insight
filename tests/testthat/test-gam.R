if (require("testthat") && require("insight") && require("mgcv")) {
  context("insight, model_info")

  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
  m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("clean_names", {
    expect_equal(clean_names(m1), c("y", "x0", "x1", "x2", "x3"))
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2", "x3")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("x0", "x1", "x2", "x3"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dat$y)
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 400)
    expect_equal(colnames(get_data(m1)), c("y", "x0", "x1", "x2", "x3"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("y ~ s(x0) + s(x1) + s(x2) + s(x3)"))
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(response = "y", conditional = c("x0", "x1", "x2", "x3")))
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2", "x3"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 400)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = "(Intercept)",
        smooth_terms = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 5)
    expect_equal(
      get_parameters(m1)$parameter, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)")
    )
    expect_equal(nrow(get_parameters(m1, "smooth_terms")), 4)
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "y",
        conditional = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(
      algorithm = "GCV", optimizer = "magic"
    ))
  })
}
