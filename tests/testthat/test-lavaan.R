skip_on_cran()
skip_if_not_installed("lavaan")

test_that("is_converged", {
  model <- lavaan::cfa(" visual  =~ x1 ", data = lavaan::HolzingerSwineford1939[1:100, ])
  expect_true(is_converged(model))
})
