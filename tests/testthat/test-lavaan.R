skip_on_cran()
skip_if_not_installed("lavaan")

test_that("is_converged", {
  model <- lavaan::cfa(" visual  =~ x1 ", data = lavaan::HolzingerSwineford1939[1:100, ])
  expect_true(is_converged(model))
})

test_that("get_varcov", {
  model <- lavaan::cfa(" visual  =~ x1 ", data = lavaan::HolzingerSwineford1939[1:100, ])
  expect_equal(
    get_varcov(model),
    lavaan::vcov(model),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  # Create simple example data
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- 0.3 * x + rnorm(n, 0, 0.8)
  data <- data.frame(x = x, y = y)

  # Fit simple lavaan model
  model <- 'y ~ x'
  fit <- lavaan::sem(model, data = data)
  expect_equal(
    get_varcov(fit),
    lavaan::vcov(fit),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})
