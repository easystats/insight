if (suppressWarnings(
  require("testthat") &&
    require("insight") &&
    require("lme4")
)) {
  context("insight, find_response")

  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence

  m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
  m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)
  m3 <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
  m4 <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  test_that("find_response", {
    expect_equal(find_response(m1, combine = TRUE), "cbind(incidence, trials)")
    expect_equal(find_response(m2, combine = TRUE), "cbind(incidence, size - incidence)")
    expect_equal(find_response(m3, combine = TRUE), "cbind(incidence, trials)")
    expect_equal(find_response(m4, combine = TRUE), "cbind(incidence, size - incidence)")
    expect_equal(find_response(m1, combine = FALSE), c("incidence", "trials"))
    expect_equal(find_response(m2, combine = FALSE), c("incidence", "size"))
    expect_equal(find_response(m3, combine = FALSE), c("incidence", "trials"))
    expect_equal(find_response(m4, combine = FALSE), c("incidence", "size"))
  })

  test_that("get_response", {
    expect_equal(colnames(get_response(m1)), c("incidence", "trials"))
    expect_equal(colnames(get_response(m2)), c("incidence", "size"))
    expect_equal(colnames(get_response(m3)), c("incidence", "trials"))
    expect_equal(colnames(get_response(m4)), c("incidence", "size"))
  })

  test_that("get_data", {
    get_data(m1)
    get_data(m2)
    get_data(m3)
    get_data(m4)
  })

  set.seed(123)
  data(mtcars)
  m5 <-
    stats::aov(
      formula = mpg ~ wt + qsec + Error(disp / am),
      data = mtcars
    )

  test_that("mod-info", {
    get_data(m5)
    find_response(m5)
    get_response(m5)
  })
}
