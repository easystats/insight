if (require("testthat") && require("insight") && require("mgcv")) {
  set.seed(2) ## simulate some data...
  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)

  bt <- gam(y ~ te(x0, x1, k = 7) + s(x2) + s(x3),
            data = dat,
            method = "REML")

  test_that("find_smooth", {
    expect_equal(find_smooth(bt), list(conditional = c("te(x0, x1, k = 7)", "s(x2)", "s(x3)")))
    expect_equal(find_smooth(bt, flatten = TRUE), c("te(x0, x1, k = 7)", "s(x2)", "s(x3)"))
  })
}
