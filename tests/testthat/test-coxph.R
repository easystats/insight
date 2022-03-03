requiet("survival")

lung <- subset(survival::lung, subset = ph.ecog %in% 0:2)
lung$sex <- factor(lung$sex, labels = c("male", "female"))
lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

m1 <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

# get_data: regression test for previous bug
# this does not seem to work when I put it inside a `test_that()` call (VAB)
dat_regression_test <- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                                  status = c(1, 1, 1, 0, 1, 1, 0),
                                  x = c(0, 2, 1, 1, 1, 0, 0),
                                  sex = c(0, 0, 0, 0, 1, 1, 1))
mod <- coxph(Surv(time, status) ~ x + strata(sex),
             data = dat_regression_test,
             ties = "breslow")

test_that("model_info", {
  expect_true(model_info(m1)$is_logit)
  expect_false(model_info(m1)$is_linear)
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
  expect_s3_class(get_data(m1), "data.frame")
  expect_equal(dim(get_data(m1)), c(166, 10))
})

test_that("get_data: regression test for previous bug", {
  expect_equal(get_data(mod), dat_regression_test)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula(
      "Surv(time, status) ~ sex + age + ph.ecog"
    )),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_equal(find_variables(m1), list(
    response = c("time", "status"),
    conditional = c("sex", "age", "ph.ecog")
  ))
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("time", "status", "sex", "age", "ph.ecog")
  )
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
    )
  )
  expect_equal(nrow(get_parameters(m1)), 4)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("sexfemale", "age", "ph.ecogok", "ph.ecoglimited")
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "Surv(time, status)",
      conditional = c("sex", "age", "ph.ecog")
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})

test_that("JM", {
  data("aids", package = "JM")
  m <- coxph(Surv(start, stop, event) ~ CD4, data = aids)
  d <- get_data(m)
  expect_equal(dim(d), c(1405, 12))
  expect_equal(find_variables(m), list(response = c("start", "stop", "event"), conditional = "CD4"))
})
