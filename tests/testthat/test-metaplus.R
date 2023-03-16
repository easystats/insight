skip_if_not_or_load_if_installed("metaplus")

data(mag)
m <- metaplus(yi, sei, slab = study, data = mag)

test_that("find_parameters", {
  expect_identical(
    find_parameters(m),
    list(conditional = c("(Intercept)", "tau2"))
  )
})

test_that("get_parameters", {
  expect_equal(
    get_parameters(m)$Estimate,
    c(-0.7463, 0.254),
    tolerance = 1e-3
  )
})

test_that("get_statistic", {
  expect_equal(
    get_statistic(m)$Statistic,
    c(-3.195532, NA),
    tolerance = 1e-3
  )
})

test_that("find_formula", {
  expect_null(find_formula(m))
})
