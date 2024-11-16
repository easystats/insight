skip_if_not_installed("survival")
skip_if_not_installed("withr")

lung <- survival::lung
Surv <- survival::Surv

mod <- survival::coxph(
  Surv(time, status) ~ ph.ecog + tt(age),
  data = lung,
  tt = function(x, t, ...) survival::pspline(x + t / 365.25)
)

test_that("model_info", {
  expect_true(model_info(mod)$is_logit)
  expect_false(model_info(mod)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(mod), list(conditional = c("ph.ecog", "age")))
  expect_null(find_predictors(mod, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(mod), "Surv(time, status)")
  expect_identical(find_response(mod, combine = FALSE), c("time", "status"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(mod)(0.2), plogis(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  ## NOTE check back every now and then and see if tests still work
  skip("works interactively")
  expect_s3_class(get_data(mod), "data.frame")
  expect_identical(dim(get_data(mod)), c(227L, 4L))
})


test_that("find_formula", {
  expect_length(find_formula(mod), 1)
  expect_equal(
    find_formula(mod),
    list(conditional = as.formula("Surv(time, status) ~ ph.ecog + tt(age)")),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(find_variables(mod), list(
    response = c("time", "status"),
    conditional = c("ph.ecog", "age")
  ))

  expect_identical(
    find_variables(mod, flatten = TRUE),
    c("time", "status", "ph.ecog", "age")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(mod), 227L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(mod)))
})

test_that("is_multivariate", {
  expect_false(is_multivariate(mod))
})

withr::with_package(
  "survival",
  test_that("find_parameters", {
    expect_identical(
      find_parameters(mod),
      list(conditional = c("ph.ecog", "tt(age), linear", "tt(age), nonlin"))
    )
    expect_identical(nrow(get_parameters(mod)), 3L)
    expect_identical(
      get_parameters(mod)$Parameter,
      c("ph.ecog", "tt(age), linear", "tt(age), nonlin")
    )
  })
)

test_that("find_terms", {
  expect_identical(
    find_terms(mod),
    list(
      response = "Surv(time, status)",
      conditional = c("ph.ecog", "tt(age)")
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(mod), "chi-squared statistic")
})
