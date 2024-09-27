skip_if_not_installed("cgam")

data(cubic, package = "cgam")
incr.conv <- cgam::incr.conv
m <- cgam::cgam(y ~ incr.conv(x), data = cubic)

mi <- insight::model_info(m)
test_that("model_info", {
  expect_false(mi$is_binomial)
  expect_true(mi$is_linear)
  expect_false(mi$is_censored)
})

test_that("n_obs", {
  expect_equal(n_obs(m), 50)
})

test_that("find_formula", {
  expect_length(find_formula(m), 1)
  expect_equal(
    find_formula(m),
    list(conditional = as.formula("y ~ incr.conv(x)")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(find_terms(m), list(
    response = "y",
    conditional = "incr.conv(x)"
  ))
  expect_equal(
    find_terms(m, flatten = TRUE),
    c("y", "incr.conv(x)")
  )
})

test_that("get_data", {
  expect_equal(nrow(get_data(m)), 50)
  expect_equal(colnames(get_data(m)), c("y", "x"))
})

test_that("get_response", {
  expect_equal(get_response(m), cubic$y)
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m))
})

test_that("is_model", {
  expect_true(is_model(m))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m), "t-statistic")
})

test_that("get_df", {
  expect_equal(get_df(m), 39.5, tolerance = 1e-3)
  expect_equal(get_df(m, type = "wald"), 39.5, tolerance = 1e-3)
  expect_equal(get_df(m, type = "model"), 2, tolerance = 1e-3)
})

test_that("get_sigma", {
  expect_equal(get_sigma(m), 2.159464, tolerance = 1e-3, ignore_attr = TRUE)
})
