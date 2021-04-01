if (require("testthat") &&
  require("insight") &&
  require("survival")) {
  m1 <- survreg(Surv(futime, fustat) ~ ecog.ps + rx,
    data = ovarian,
    dist = "exponential"
  )
  m2 <- survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), data = lung)

  test_that("model_info", {
    expect_false(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_exponential)
    expect_true(model_info(m2)$is_survival)
    expect_false(model_info(m2)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("ecog.ps", "rx")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("ecog.ps", "rx"))
    expect_null(find_predictors(m1, effects = "random"))
    expect_identical(find_predictors(m2), list(conditional = c("ph.ecog", "age", "sex")))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Surv(futime, fustat)")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), ovarian[, c("futime", "fustat")])
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("ecog.ps", "rx"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 26)
    expect_equal(
      colnames(get_data(m1)),
      c("futime", "fustat", "Surv(futime, fustat)", "ecog.ps", "rx")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Surv(futime, fustat) ~ ecog.ps + rx")),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(
      response = c("futime", "fustat"),
      conditional = c("ecog.ps", "rx")
    ))
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("futime", "fustat", "ecog.ps", "rx")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 26)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c("(Intercept)", "ecog.ps", "rx"))
    )
    expect_equal(
      find_parameters(m2),
      list(conditional = c("(Intercept)", "ph.ecog", "age", "sex=1", "sex=2"))
    )
  })

  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "ecog.ps", "rx"))
    expect_equal(get_parameters(m1)$Estimate, c(6.96184, -0.43313, 0.5815), tolerance = 1e-3)
    expect_equal(nrow(get_parameters(m2)), 5)
    expect_equal(get_parameters(m2)$Parameter, c("(Intercept)", "ph.ecog", "age", "sex=1", "sex=2"))
    expect_equal(get_parameters(m2)$Estimate, c(6.73235, -0.32443, -0.00581, -0.24408, -0.42345), tolerance = 1e-3)
  })

  test_that("get_statistic", {
    expect_equal(nrow(get_statistic(m1)), 3)
    expect_equal(nrow(get_statistic(m2)), 5)
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })
}
