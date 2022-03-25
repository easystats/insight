pkgs <- c("insight", "estimatr", "ivreg")
invisible(sapply(pkgs, requiet))


# iv_robust --------------------------------------------------------------
# =========================================================================

test_that("get_modelmatrix - iv_robust", {

  data(Kmenta, package = "ivreg")
  x <- iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(terms(x), data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, at = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "P")), nrow(out2))
})



# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - ivreg", {

  data(Kmenta, package = "ivreg")
  x <- ivreg(Q ~ P + D | D + F + A, data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "P"))
  out2 <- model.matrix(terms(x), data = get_datagrid(x, at = "P", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "P")), nrow(out2))
})



# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - lm_robust", {

  set.seed(15)
  N = 1:40
  dat <- data.frame(
    N = N,
    y = rpois(N, lambda = 4),
    x = rnorm(N),
    z = rbinom(N, 1, prob = 0.4)
  )

  x <- lm_robust(y ~ x + z, data = dat)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = dat)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, at = "x"))
  out2 <- model.matrix(x, data = get_datagrid(x, at = "x", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(nrow(get_datagrid(x, at = "x")), nrow(out2))
})
