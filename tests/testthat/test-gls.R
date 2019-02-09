if (require("testthat") && require("insight") && require("nlme")) {
  context("insight, model_info")

  data(Ovary)
  m1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
             correlation = corAR1(form = ~ 1 | Mare))

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("pi", "Time")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("pi", "Time"))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "follicles")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 308)
    expect_equal(colnames(get_data(m1)), c("Mare", "Time", "follicles"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time)"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = "follicles", conditional = c("pi", "Time")))
    expect_equal(find_terms(m1, flatten = TRUE), c("follicles", "pi", "Time"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 308)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "sin(2 * pi * Time)",  "cos(2 * pi * Time)")
      ))
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "sin(2 * pi * Time)",  "cos(2 * pi * Time)"))
  })

}
