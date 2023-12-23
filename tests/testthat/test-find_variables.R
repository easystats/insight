test_that("`find_variables` works with `mgcv::gam`", {
  skip_if_not_installed("mgcv")
  set.seed(2) ## simulate some data...
  dat <- mgcv::gamSim(1, n = 50, dist = "normal", scale = 2, verbose = FALSE)

  b1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2), family = stats::gaussian(), data = dat)
  b2 <- mgcv::gam(list(y ~ s(x0) + s(x1) + s(x2), ~ s(x3)), family = mgcv::gaulss(), data = dat)

  f_b1 <- find_variables(b1)
  f_b2 <- find_variables(b2)

  results <- list(response = "y", conditional = c("x0", "x1", "x2"))

  expect_identical(f_b1, results)
  expect_identical(f_b2, c(results, list(scale = "x3")))
})
