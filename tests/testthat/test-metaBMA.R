if (require("testthat") && require("insight") && require("metaBMA")) {
  data(towels)
  set.seed(123)
  mf <- meta_fixed(logOR, SE, study, data = towels, d = prior("norm", c(mean = 0, sd = .3), lower = 0))

  test_that("get_priors-metaBMA", {
    priors <- get_priors(mf)
    expect_equal(priors$Distribution, "norm")
    expect_equal(priors$Scale, 0.3, tolerance = 1e-2)
  })
}
