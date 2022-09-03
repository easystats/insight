if (requiet("testthat") && requiet("insight") && requiet("survival")) {
  f1 <- as.formula("Surv(time, status) ~ strata(sex) * x + x2")
  f2 <- as.formula("Surv(time, status) ~ x * strata(sex) * x2")
  f3 <- as.formula("Surv(time, status) ~ x + x2 * strata(sex)")
  f4 <- as.formula("Surv(time, status) ~ strata(sex) + x + x2")
  f5 <- as.formula("Surv(time, status) ~ x * strata(sex) + x2")
  f6 <- as.formula("Surv(time, status) ~ x + x2 + strata(sex)")

  dat <- list(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    x2 = c(0, 2, 1, 0, 1, 1, 0),
    sex = c(0, 0, 0, 0, 1, 1, 1)
  )

  test_that("find_predictors strata1", {
    mod <- suppressWarnings(coxph(f1, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })

  test_that("find_predictors strata2", {
    mod <- suppressWarnings(coxph(f2, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })

  test_that("find_predictors strata3", {
    mod <- suppressWarnings(coxph(f3, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })

  test_that("find_predictors strata4", {
    mod <- suppressWarnings(coxph(f4, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })

  test_that("find_predictors strata5", {
    mod <- suppressWarnings(coxph(f5, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })

  test_that("find_predictors strata6", {
    mod <- suppressWarnings(coxph(f6, data = dat, ties = "breslow"))
    expect_equal(find_predictors(mod), list(conditional = c("x", "x2"), strata = "sex"))
  })
}
