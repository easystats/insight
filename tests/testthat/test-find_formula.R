test_that("`find_formula` works with `mgcv::gam()`", {
  skip_if_not_installed("mgcv")
  set.seed(2) ## simulate some data...
  dat <- mgcv::gamSim(1, n = 50, dist = "normal", scale = 2, verbose = FALSE)
  b <- mgcv::gam(list(y ~ s(x0) + s(x1) + s(x2), ~ s(x3)), family = mgcv::gaulss(), data = dat)

  f <- find_formula(b)
  expect_named(f, c("conditional", "scale"))
  expect_identical(f$conditional, formula("y ~ s(x0) + s(x1) + s(x2)"))
  expect_identical(f$scale, formula("~s(x3)"))
})
