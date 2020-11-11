if (require("testthat") && require("insight") && require("BFBayesFactor")) {
  model <- BayesFactor::proportionBF(15, 25, p = 0.5)
  mi <- insight::model_info(model)
  test_that("model_info-BF-proptest", {
    expect_true(mi$is_binomial)
    expect_false(mi$is_linear)
  })

  model <- prop.test(15, 25, p = 0.5)
  mi <- insight::model_info(model)
  test_that("model_info-BF-proptest", {
    expect_true(mi$is_binomial)
    expect_false(mi$is_linear)
    expect_false(mi$is_correlation)
  })
}
