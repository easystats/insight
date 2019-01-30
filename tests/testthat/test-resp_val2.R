if (suppressWarnings(
  require("testthat") &&
  require("insight") &&
  require("lme4")
)) {
  context("insight, model_response")

  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence

  m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
  m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)
  m3 <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
  m4 <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  test_that("model_response", {
    expect_equal(model_response(m1, combine = TRUE), "cbind(incidence, trials)")
    expect_equal(model_response(m2, combine = TRUE), "cbind(incidence, size - incidence)")
    expect_equal(model_response(m3, combine = TRUE), "cbind(incidence, trials)")
    expect_equal(model_response(m4, combine = TRUE), "cbind(incidence, size - incidence)")
    expect_equal(model_response(m1, combine = FALSE), c("incidence", "trials"))
    expect_equal(model_response(m2, combine = FALSE), c("incidence", "size"))
    expect_equal(model_response(m3, combine = FALSE), c("incidence", "trials"))
    expect_equal(model_response(m4, combine = FALSE), c("incidence", "size"))
  })

  test_that("resp_val", {
    expect_equal(colnames(resp_val(m1)), c("incidence", "trials"))
    expect_equal(colnames(resp_val(m2)), c("incidence", "size"))
    expect_equal(colnames(resp_val(m3)), c("incidence", "trials"))
    expect_equal(colnames(resp_val(m4)), c("incidence", "size"))
  })

  test_that("model_frame", {
    model_frame(m1)
    model_frame(m2)
    model_frame(m3)
    model_frame(m4)
  })

  set.seed(123)
  data(mtcars)
  m5 <-
    stats::aov(
      formula = mpg ~ wt + qsec + Error(disp / am),
      data = mtcars
    )

  test_that("mod-info", {
    model_frame(m5)
    model_response(m5)
    resp_val(m5)
  })

}
