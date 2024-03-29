skip_if_not_installed("estimatr")

m1 <- estimatr::lm_robust(mpg ~ gear + wt + cyl, data = mtcars)

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("gear", "wt", "cyl")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("gear", "wt", "cyl"))
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m1))
})

test_that("get_random", {
  expect_warning(get_random(m1))
})

test_that("find_response", {
  expect_identical(find_response(m1), "mpg")
})

test_that("get_response", {
  expect_equal(get_response(m1), mtcars$mpg)
})

test_that("get_predictors", {
  expect_equal(colnames(get_predictors(m1)), c("gear", "wt", "cyl"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1)), 32)
  expect_equal(colnames(get_data(m1)), c("mpg", "gear", "wt", "cyl"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("mpg ~ gear + wt + cyl")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(find_terms(m1), list(
    response = "mpg",
    conditional = c("gear", "wt", "cyl")
  ))
  expect_equal(find_terms(m1, flatten = TRUE), c("mpg", "gear", "wt", "cyl"))
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 32)
})

test_that("link_function", {
  expect_equal(link_function(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(conditional = c("(Intercept)", "gear", "wt", "cyl"))
  )
  expect_equal(nrow(get_parameters(m1)), 4)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "gear", "wt", "cyl")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_algorithm", {
  expect_warning(expect_null(find_algorithm(m1)))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})


test_that("multivariate", {
  mod1 <- estimatr::lm_robust(cbind(mpg, qsec) ~ cyl + disp, data = mtcars)
  mod2 <- estimatr::lm_robust(mpg ~ cyl + disp, data = mtcars)
  expect_true(is_multivariate(mod1))
  expect_false(is_multivariate(mod2))
  expect_equal(dim(get_parameters(mod1)), c(6, 3))
  expect_equal(dim(get_parameters(mod2)), c(3, 2))
  expect_equal(dim(get_statistic(mod1)), c(6, 3))
  expect_equal(dim(get_statistic(mod2)), c(3, 2))
  expect_equal(colnames(get_parameters(mod1)), c("Parameter", "Estimate", "Response"))
  expect_equal(colnames(get_statistic(mod1)), c("Parameter", "Statistic", "Response"))
})
