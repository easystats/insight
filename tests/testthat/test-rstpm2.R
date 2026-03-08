skip_if_not_installed("rstpm2")
skip_if_not_installed("survival")

# rstpm2 must be attached (not just loaded) because stpm2/pstpm2
# use eval() in the parent frame and need internal functions on the search path
require(rstpm2, quietly = TRUE)
require(survival, quietly = TRUE)

data(cancer, package = "survival")
lung2 <- subset(lung, complete.cases(time, status, age, sex, ph.ecog))
lung2$sex <- factor(lung2$sex, levels = c(1, 2), labels = c("male", "female"))

mod_aft <- aft(
  Surv(time, status == 2) ~ age + sex + ph.ecog,
  data = lung2,
  df = 4
)

mod_stpm2 <- stpm2(
  Surv(time, status == 2) ~ age + sex + ph.ecog,
  data = lung2,
  df = 4
)

mod_pstpm2 <- pstpm2(
  Surv(time, status == 2) ~ age + sex + ph.ecog,
  data = lung2,
  df = 4
)

# aft -------------------------------------------------------

test_that("aft: model_info", {
  mi <- model_info(mod_aft)
  expect_true(mi$is_survival)
  expect_false(mi$is_linear)
})

test_that("aft: find_formula", {
  f <- find_formula(mod_aft)
  expect_length(f, 1)
  expect_equal(
    f,
    list(conditional = as.formula("Surv(time, status == 2) ~ age + sex + ph.ecog")),
    ignore_attr = TRUE
  )
})

test_that("aft: find_variables", {
  v <- find_variables(mod_aft)
  expect_identical(v$response, c("time", "status"))
  expect_identical(v$conditional, c("age", "sex", "ph.ecog"))
})

test_that("aft: find_response", {
  expect_identical(find_response(mod_aft, combine = FALSE), c("time", "status"))
})

test_that("aft: find_predictors", {
  expect_identical(
    find_predictors(mod_aft),
    list(conditional = c("age", "sex", "ph.ecog"))
  )
  expect_identical(
    find_predictors(mod_aft, flatten = TRUE),
    c("age", "sex", "ph.ecog")
  )
  expect_null(find_predictors(mod_aft, effects = "random"))
})

test_that("aft: get_data", {
  d <- get_data(mod_aft)
  expect_identical(nrow(d), 227L)
  expect_true(all(c("time", "status", "age", "sex", "ph.ecog") %in% colnames(d)))
})

test_that("aft: find_parameters", {
  p <- find_parameters(mod_aft)
  expect_true("conditional" %in% names(p))
  expect_true(all(c("age", "sexfemale", "ph.ecog") %in% p$conditional))
})

test_that("aft: is_model", {
  expect_true(is_model(mod_aft))
  expect_true(is_regression_model(mod_aft))
})

# stpm2 -----------------------------------------------------

test_that("stpm2: model_info", {
  mi <- model_info(mod_stpm2)
  expect_true(mi$is_survival)
  expect_false(mi$is_linear)
})

test_that("stpm2: find_formula", {
  f <- find_formula(mod_stpm2)
  expect_length(f, 1)
  expect_equal(
    f,
    list(conditional = as.formula("Surv(time, status == 2) ~ age + sex + ph.ecog")),
    ignore_attr = TRUE
  )
})

test_that("stpm2: find_variables", {
  v <- find_variables(mod_stpm2)
  expect_identical(v$response, c("time", "status"))
  expect_identical(v$conditional, c("age", "sex", "ph.ecog"))
})

test_that("stpm2: get_data", {
  d <- get_data(mod_stpm2)
  expect_identical(nrow(d), 227L)
  expect_true(all(c("time", "status", "age", "sex", "ph.ecog") %in% colnames(d)))
})

test_that("stpm2: is_model", {
  expect_true(is_model(mod_stpm2))
  expect_true(is_regression_model(mod_stpm2))
})

# pstpm2 ----------------------------------------------------

test_that("pstpm2: model_info", {
  mi <- model_info(mod_pstpm2)
  expect_true(mi$is_survival)
  expect_false(mi$is_linear)
})

test_that("pstpm2: find_formula", {
  f <- find_formula(mod_pstpm2)
  expect_length(f, 1)
  expect_equal(
    f,
    list(conditional = as.formula("Surv(time, status == 2) ~ age + sex + ph.ecog")),
    ignore_attr = TRUE
  )
})

test_that("pstpm2: find_variables", {
  v <- find_variables(mod_pstpm2)
  expect_identical(v$response, c("time", "status"))
  expect_identical(v$conditional, c("age", "sex", "ph.ecog"))
})

test_that("pstpm2: get_data", {
  d <- get_data(mod_pstpm2)
  expect_identical(nrow(d), 227L)
  expect_true(all(c("time", "status", "age", "sex", "ph.ecog") %in% colnames(d)))
})

test_that("pstpm2: is_model", {
  expect_true(is_model(mod_pstpm2))
  expect_true(is_regression_model(mod_pstpm2))
})
