if (requiet("testthat") && requiet("insight") && requiet("BayesFactor")) {
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


if (requiet("testthat") && requiet("insight") && requiet("tweedie") && requiet("statmod")) {
  d <- data.frame(x = 1:20, y = rgamma(20, shape = 5))
  # Fit a poisson generalized linear model with identity link
  model <- glm(y ~ x, data = d, family = tweedie(var.power = 1, link.power = 1))
  mi <- insight::model_info(model)
  test_that("model_info-tweedie", {
    expect_true(mi$is_tweedie)
    expect_false(mi$is_poisson)
    expect_equal(mi$family, "Tweedie")
  })
}