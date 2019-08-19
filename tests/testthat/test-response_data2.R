if (suppressWarnings(require("testthat") &&
  require("insight") &&
  require("lme4"))) {
  context("insight, find_response")

  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence

  m1 <-
    glmer(cbind(incidence, trials) ~ period + (1 |
      herd),
    data = cbpp,
    family = binomial
    )
  m2 <-
    glmer(
      cbind(incidence, size - incidence) ~ period + (1 |
        herd),
      data = cbpp,
      family = binomial
    )
  m3 <-
    glm(cbind(incidence, trials) ~ period,
      data = cbpp,
      family = binomial
    )
  m4 <-
    glm(cbind(incidence, size - incidence) ~ period,
      data = cbpp,
      family = binomial
    )
  m5 <-
    glmer(cbind(incidence, size - incidence) ~ (1 |
      herd),
    data = cbpp,
    family = binomial
    )

  test_that("find_response", {
    expect_equal(
      find_response(m1, combine = TRUE),
      "cbind(incidence, trials)"
    )
    expect_equal(
      find_response(m2, combine = TRUE),
      "cbind(incidence, size - incidence)"
    )
    expect_equal(
      find_response(m3, combine = TRUE),
      "cbind(incidence, trials)"
    )
    expect_equal(
      find_response(m4, combine = TRUE),
      "cbind(incidence, size - incidence)"
    )
    expect_equal(
      find_response(m5, combine = TRUE),
      "cbind(incidence, size - incidence)"
    )
    expect_equal(
      find_response(m1, combine = FALSE),
      c("incidence", "trials")
    )
    expect_equal(find_response(m2, combine = FALSE), c("incidence", "size"))
    expect_equal(
      find_response(m3, combine = FALSE),
      c("incidence", "trials")
    )
    expect_equal(find_response(m4, combine = FALSE), c("incidence", "size"))
    expect_equal(find_response(m5, combine = FALSE), c("incidence", "size"))
  })

  test_that("get_response", {
    expect_equal(colnames(get_response(m1)), c("incidence", "trials"))
    expect_equal(colnames(get_response(m2)), c("incidence", "size"))
    expect_equal(colnames(get_response(m3)), c("incidence", "trials"))
    expect_equal(colnames(get_response(m4)), c("incidence", "size"))
    expect_equal(colnames(get_response(m5)), c("incidence", "size"))
  })

  test_that("get_data", {
    expect_equal(
      colnames(get_data(m1)),
      c(
        "cbind(incidence, trials)",
        "period",
        "herd",
        "incidence",
        "trials"
      )
    )
    expect_equal(
      colnames(get_data(m2)),
      c(
        "cbind(incidence, size - incidence)",
        "period",
        "herd",
        "incidence",
        "size"
      )
    )
    get_data(m3)
    get_data(m4)
    expect_equal(
      colnames(get_data(m5)),
      c(
        "cbind(incidence, size - incidence)",
        "herd",
        "incidence",
        "size"
      )
    )
  })

  set.seed(123)
  data(mtcars)
  m6 <-
    stats::aov(
      formula = mpg ~ wt + qsec + Error(disp / am),
      data = mtcars
    )

  # TO DO
  # test_that("mod-info", {
  #   get_data(m6)
  #   find_response(m6)
  #   get_response(m6)
  #   find_formula(m6)
  # })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
    expect_identical(find_statistic(m2), "z-statistic")
    expect_identical(find_statistic(m3), "z-statistic")
    expect_identical(find_statistic(m4), "z-statistic")
    expect_identical(find_statistic(m5), "z-statistic")
    expect_identical(find_statistic(m6), "F-statistic")
  })
}
