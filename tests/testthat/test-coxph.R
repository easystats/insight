if (require("testthat") && require("insight") && require("survival")) {
  context("insight, model_info")

  data("lung")
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

  m1 <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

  test_that("model_info", {
    expect_true(model_info(m1)$is_logit)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("sex", "age", "ph.ecog")))
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Surv(time, status)")
    expect_identical(find_response(m1, combine = FALSE), c("time", "status"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 226)
    expect_equal(colnames(get_data(m1)), c("time", "status", "Surv(time, status)", "sex", "age", "ph.ecog"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("Surv(time, status) ~ sex + age + ph.ecog"))
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(response = c("time", "status"), conditional = c("sex", "age", "ph.ecog")))
    expect_equal(find_terms(m1, flatten = TRUE), c("time", "status", "sex", "age", "ph.ecog"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 226)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("sexfemale", "age", "ph.ecogok", "ph.ecoglimited")
      ))
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(get_parameters(m1)$parameter, c("sexfemale", "age", "ph.ecogok", "ph.ecoglimited"))
  })

}
