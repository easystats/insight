if (requiet("testthat") &&
  requiet("insight") &&
  requiet("survival") &&
  requiet("lme4") &&
  requiet("nlme") &&
  requiet("bdsmatrix") &&
  requiet("coxme")) {
  set.seed(1234)
  lung$inst2 <- sample(1:10, size = nrow(lung), replace = TRUE)
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

  m1 <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
  m2 <- coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst) + (1 | inst2), lung)

  test_that("model_info", {
    expect_true(model_info(m1)$is_logit)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("ph.ecog", "age")))
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "inst")
    )
    expect_identical(find_predictors(m2), list(conditional = c("ph.ecog", "age")))
    expect_identical(find_predictors(m2, effects = "random"), list(random = c("inst", "inst2")))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Surv(time, status)")
    expect_identical(find_response(m1, combine = FALSE), c("time", "status"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
    expect_equal(link_inverse(m2)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 226)
    expect_equal(
      colnames(get_data(m1)),
      c(
        "time",
        "status",
        "Surv(time, status)",
        "ph.ecog",
        "age",
        "inst"
      )
    )
    expect_equal(
      colnames(get_data(m2)),
      c(
        "time",
        "status",
        "Surv(time, status)",
        "ph.ecog",
        "age",
        "inst",
        "inst2"
      )
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("Surv(time, status) ~ ph.ecog + age"),
        random = as.formula("~1 | inst")
      ),
      ignore_attr = TRUE
    )

    expect_length(find_formula(m2), 2)
    expect_equal(
      find_formula(m2),
      list(
        conditional = as.formula("Surv(time, status) ~ ph.ecog + age"),
        random = list(as.formula("~1 | inst"), as.formula("~1 | inst2"))
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "Surv(time, status)",
        conditional = c("ph.ecog", "age"),
        random = "inst"
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("Surv(time, status)", "ph.ecog", "age", "inst")
    )
    expect_equal(
      find_terms(m2),
      list(
        response = "Surv(time, status)",
        conditional = c("ph.ecog", "age"),
        random = c("inst", "inst2")
      )
    )
    expect_equal(
      find_terms(m2, flatten = TRUE),
      c("Surv(time, status)", "ph.ecog", "age", "inst", "inst2")
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = c("time", "status"),
        conditional = c("ph.ecog", "age"),
        random = "inst"
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("time", "status", "ph.ecog", "age", "inst")
    )
    expect_equal(
      find_variables(m2),
      list(
        response = c("time", "status"),
        conditional = c("ph.ecog", "age"),
        random = c("inst", "inst2")
      )
    )
    expect_equal(
      find_variables(m2, flatten = TRUE),
      c("time", "status", "ph.ecog", "age", "inst", "inst2")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 225)
    expect_equal(n_obs(m2), 225)
  })

  test_that("get_response", {
    expect_equal(colnames(get_response(m1)), c("time", "status"))
    expect_equal(nrow(get_response(m1)), 226)
    expect_equal(colnames(get_response(m1)), c("time", "status"))
    expect_equal(nrow(get_response(m2)), 226)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
    expect_false(is.null(link_function(m2)))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("ph.ecogok", "ph.ecoglimited", "age"),
        random = "inst"
      )
    )
    expect_equal(
      find_parameters(m2),
      list(
        conditional = c("ph.ecogok", "ph.ecoglimited", "age"),
        random = c("inst", "inst2")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("ph.ecogok", "ph.ecoglimited", "age")
    )

    expect_equal(nrow(get_parameters(m2)), 3)
    expect_equal(
      get_parameters(m2)$Parameter,
      c("ph.ecogok", "ph.ecoglimited", "age")
    )

    expect_length(get_parameters(m2, effects = "random"), 2)
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
    expect_identical(find_statistic(m2), "z-statistic")
  })
}
