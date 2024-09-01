skip_on_cran()
skip_if_not_installed("WeightIt")
skip_if_not_installed("cobalt")
skip_if_not_installed("fwb")

data("lalonde", package = "cobalt")

# Logistic regression ATT weights
w.out <- WeightIt::weightit(
  treat ~ age + educ + married + re74,
  data = lalonde,
  method = "glm",
  estimand = "ATT"
)
set.seed(123)
fit3 <- WeightIt::lm_weightit(
  re78 ~ treat + age + educ,
  data = lalonde,
  weightit = w.out,
  vcov = "FWB",
  R = 50, # should use way more
  fwb.args = list(wtype = "mammen")
)

# Multinomial logistic regression outcome model
# that adjusts for estimation of weights
lalonde$re78_3 <- factor(findInterval(lalonde$re78, c(0, 5e3, 1e4)))

fit4 <- WeightIt::multinom_weightit(
  re78_3 ~ treat + age + educ,
  data = lalonde,
  weightit = w.out
)

# Ordinal probit regression that adjusts for estimation
# of weights
fit5 <- WeightIt::ordinal_weightit(
  ordered(re78_3) ~ treat + age + educ,
  data = lalonde,
  link = "probit",
  weightit = w.out
)


test_that("model_info", {
  expect_true(model_info(fit3)$is_linear)
  expect_true(model_info(fit4)$is_multinomial)
  expect_true(model_info(fit5)$is_ordinal)
})

test_that("get_residuals", {
  expect_equal(
    head(get_residuals(fit3)),
    head(stats::residuals(fit3)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    head(get_residuals(fit4)),
    head(stats::residuals(fit4)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    head(get_residuals(fit5)),
    head(stats::residuals(fit5)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("get_sigma", {
  expect_equal(get_sigma(fit3), 5391.306, tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(get_sigma(fit4), 0.4720903, tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(get_sigma(fit5), 0.4753789, tolerance = 1e-2, ignore_attr = TRUE)
})

test_that("find_predictors", {
  expect_identical(find_predictors(fit3), list(conditional = c("treat", "age", "educ")))
  expect_null(find_predictors(fit3, effects = "random"))
  expect_identical(find_predictors(fit4), list(conditional = c("treat", "age", "educ")))
  expect_null(find_predictors(fit4, effects = "random"))
  expect_identical(find_predictors(fit5), list(conditional = c("treat", "age", "educ")))
  expect_null(find_predictors(fit5, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(fit3), "re78")
  expect_identical(find_response(fit4), "re78_3")
  expect_identical(find_response(fit5), "re78_3")
})

test_that("link_inverse", {
  expect_equal(link_inverse(fit3)(0.2), 0.2, tolerance = 1e-3)
  expect_equal(link_inverse(fit4)(0.2), plogis(0.2), tolerance = 1e-3)
  expect_equal(link_inverse(fit5)(0.2), 0.5792597, tolerance = 1e-3) # probit
})

test_that("link_function", {
  expect_equal(link_function(fit3)(0.2), 0.2, tolerance = 1e-3)
  expect_equal(link_function(fit4)(0.2), qlogis(0.2), tolerance = 1e-3)
  expect_equal(link_function(fit5)(0.2), -0.8416212, tolerance = 1e-3) # probit
})

test_that("loglik", {
  expect_equal(get_loglikelihood(fit3), -6361.52, tolerance = 1e-2, ignore_attr = TRUE)
})

test_that("get_df", {
  expect_equal(get_df(fit3), df.residual(fit3), ignore_attr = TRUE)
  expect_equal(get_df(fit4), df.residual(fit4), ignore_attr = TRUE)
  expect_equal(get_df(fit5), df.residual(fit5), ignore_attr = TRUE)
  expect_equal(get_df(fit3, type = "model"), 5, ignore_attr = TRUE)
  expect_equal(get_df(fit4, type = "model"), 4, ignore_attr = TRUE)
  expect_equal(get_df(fit5, type = "model"), 5, ignore_attr = TRUE)
})

test_that("get_data", {
  expect_equal(nrow(get_data(fit3)), 614, ignore_attr = TRUE)
  expect_named(get_data(fit3), c("re78", "treat", "age", "educ"))
  expect_equal(nrow(get_data(fit4)), 614, ignore_attr = TRUE)
  expect_named(get_data(fit5), c("re78_3", "treat", "age", "educ"))
})

test_that("get_intercept", {
  expect_equal(get_intercept(fit3), as.vector(stats::coef(fit3)[1]), ignore_attr = TRUE)
  expect_equal(get_intercept(fit4), as.vector(stats::coef(fit4)[c(1, 5)]), ignore_attr = TRUE)
  expect_true(is.na(get_intercept(fit5)))
})

test_that("find_formula", {
  expect_length(find_formula(fit3), 1)
  expect_equal(
    find_formula(fit3),
    list(conditional = as.formula("re78 ~ treat + age + educ")),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(fit4),
    list(conditional = as.formula("re78_3 ~ treat + age + educ")),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(fit5),
    list(conditional = as.formula("ordered(re78_3) ~ treat + age + educ")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(fit3),
    list(
      response = "re78",
      conditional = c("treat", "age", "educ")
    )
  )
  expect_identical(
    find_terms(fit4),
    list(
      response = "re78_3",
      conditional = c("treat", "age", "educ")
    )
  )
  expect_identical(
    find_terms(fit5),
    list(
      response = "ordered(re78_3)",
      conditional = c("treat", "age", "educ")
    )
  )
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(fit3),
    list(conditional = c("(Intercept)", "treat", "age", "educ"))
  )
  expect_identical(
    find_parameters(fit4),
    list(conditional = c("(Intercept)", "treat", "age", "educ"))
  )
  expect_identical(
    find_parameters(fit5),
    list(conditional = c("treat", "age", "educ", "1|2", "2|3"))
  )
  expect_identical(nrow(get_parameters(fit3)), 4L)
  expect_identical(nrow(get_parameters(fit4)), 8L)
  expect_identical(nrow(get_parameters(fit5)), 5L)
})

test_that("is_model", {
  expect_true(is_model(fit3))
  expect_true(is_model(fit4))
  expect_true(is_model(fit5))
})

test_that("get_varcov", {
  expect_equal(diag(get_varcov(fit3)), diag(vcov(fit3)), tolerance = 1e-5)
  expect_equal(diag(get_varcov(fit4)), diag(vcov(fit4)), tolerance = 1e-5)
  expect_equal(diag(get_varcov(fit5)), diag(vcov(fit5)), tolerance = 1e-5)
})

test_that("get_statistic", {
  expect_equal(
    get_statistic(fit3)$Statistic,
    c(0.17184, 1.33867, 0.06674, 3.15887),
    tolerance = 1e-3
  )
  expect_equal(
    get_statistic(fit4)$Statistic,
    c(0.00271, 0.31156, -2.37521, -0.33413, -4.22954, 0.67165, -0.01399, 3.51396),
    tolerance = 1e-3
  )
  expect_equal(
    get_statistic(fit5)$Statistic,
    c(0.60512, -0.65645, 2.98951, 2.54289, 4.76061),
    tolerance = 1e-3
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(fit3), "t-statistic")
  expect_identical(find_statistic(fit4), "z-statistic")
  expect_identical(find_statistic(fit5), "z-statistic")
})
