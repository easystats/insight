if (requiet("testthat") && requiet("insight") && requiet("lme4")) {
  set.seed(123)
  startvec <- c(Asym = 200, xmid = 725, scal = 350)
  nm1 <-
    lme4::nlmer(
      formula = circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym | Tree,
      data = Orange,
      start = startvec
    )

  test_that("model_info", {
    expect_true(model_info(nm1)$is_linear)
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(nm1), "t-statistic")
  })
}
