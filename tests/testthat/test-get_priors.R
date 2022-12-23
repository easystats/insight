skip_on_os(os = "mac")

is_dev_version <- length(strsplit(packageDescription("insight")$Version, "\\.")[[1]]) > 3
run_stan <- .Platform$OS.type == "unix" && is_dev_version

if (run_stan &&  requiet("insight") && requiet("brms")) {
  data(mtcars)
  set.seed(123)

  model <- brms::brm(mpg ~ wt, data = mtcars, seed = 1, refresh = 0)
  priors <- insight::get_priors(model)

  test_that("get_priors", {
    expect_equal(priors$Location, c(19.2, NA, 0), tolerance = 1e-3)
    expect_equal(priors$Distribution, c("student_t", "uniform", "student_t"))
    expect_equal(priors$Parameter, c("b_Intercept", "b_wt", "sigma"))
  })
}
