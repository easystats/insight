skip_on_cran()
skip_if_not_installed("parsnip")

data(mtcars)
m <- parsnip::linear_reg()
m <- parsnip::set_engine(m, "lm")
m <- parsnip::set_mode(m, "regression")
m <- parsnip::fit(m, mpg ~ am + vs, data = mtcars)

test_that("find_formula", {
  expect_equal(
    find_formula(m),
    list(conditional = as.formula("mpg ~ am + vs")),
    ignore_attr = TRUE
  )
})

test_that("model_info", {
  expect_true(model_info(m)$is_linear)
})

test_that("loglik", {
  expect_equal(
    get_loglikelihood(m),
    -83.8397585518224,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("get_df", {
  expect_equal(get_df(m), 29, ignore_attr = TRUE)
  expect_equal(get_df(m, type = "model"), 4, ignore_attr = TRUE)
})


test_that("find_predictors", {
  expect_identical(find_predictors(m), list(conditional = c("am", "vs")))
  expect_identical(
    find_predictors(m, flatten = TRUE),
    c("am", "vs")
  )
  expect_null(find_predictors(m, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m))
})

test_that("get_random", {
  expect_warning(get_random(m))
})

test_that("find_response", {
  expect_identical(find_response(m), "mpg")
})

test_that("get_response", {
  expect_equal(get_response(m), mtcars$mpg)
})

test_that("get_predictors", {
  expect_equal(colnames(get_predictors(m)), c("am", "vs"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m)(0.2), 0.2, tolerance = 1e-5)
})

test_that("linkfun", {
  expect_equal(link_function(m)(0.2), -1.609438, tolerance = 1e-4)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m)), 32)
  expect_named(get_data(m), c("mpg", "am", "vs"))
})
