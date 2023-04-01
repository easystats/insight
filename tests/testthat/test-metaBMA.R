if (skip_if_not_or_load_if_installed("metaBMA")) {
  data(towels)
  set.seed(123)
  mf <- meta_fixed(logOR,
    SE,
    study,
    data = towels,
    d = prior("norm", c(mean = 0, sd = 0.3), lower = 0)
  )

  test_that("get_priors-metaBMA", {
    priors <- get_priors(mf)
    expect_identical(priors$Distribution, "Normal")
    expect_equal(priors$Scale, 0.3, tolerance = 1e-2)
  })


  set.seed(123)
  mr <- suppressWarnings(
    meta_random(logOR, SE, study,
      data = towels,
      d = prior("cauchy", c(location = 0, scale = 0.707)),
      tau = prior("invgamma", c(shape = 1, scale = 0.15))
    )
  )

  test_that("get_priors-metaBMA", {
    priors <- get_priors(mr)
    expect_identical(priors$Distribution, c("Student's t", "Inverse gamma"))
    expect_equal(priors$Scale, c(0.707, 0.15), tolerance = 1e-2)
  })
}
