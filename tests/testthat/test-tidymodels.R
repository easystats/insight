skip_on_cran()
skip_if_not_installed("parsnip")

data(mtcars)
m <- parsnip::linear_reg()
m <- parsnip::set_engine(m, "lm")
m <- parsnip::set_mode(m, "regression")
m <- parsnip::fit(m, mpg ~ am + vs, data = mtcars)

test_that("find_formula", {
  expect_equal(
    find_formula(m),
    list(conditional = as.formula("mpg ~ am + vs")),
    ignore_attr = TRUE
  )
})

test_that("model_info", {
  expect_true(model_info(m)$is_linear)
})

test_that("loglik", {
  expect_equal(
    get_loglikelihood(m),
    -83.8397585518224,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("get_df", {
  expect_equal(get_df(m), 29, ignore_attr = TRUE)
  expect_equal(get_df(m, type = "model"), 4, ignore_attr = TRUE)
})


test_that("find_predictors", {
  expect_identical(find_predictors(m), list(conditional = c("am", "vs")))
  expect_identical(
    find_predictors(m, flatten = TRUE),
    c("am", "vs")
  )
  expect_null(find_predictors(m, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m))
})

test_that("get_random", {
  expect_warning(get_random(m))
})

test_that("find_response", {
  expect_identical(find_response(m), "mpg")
})

test_that("get_response", {
  expect_equal(get_response(m), mtcars$mpg)
})

test_that("get_predictors", {
  expect_named(get_predictors(m), c("am", "vs"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m)(0.2), 0.2, tolerance = 1e-5)
})

test_that("linkfun", {
  expect_equal(link_function(m)(0.2), 0.2, tolerance = 1e-4)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m)), 32)
  expect_named(get_data(m), c("mpg", "am", "vs"))
})


# Workflow tests ----------------------------------------------------------

skip_if_not_installed("workflows")
skip_if_not_installed("recipes")

test_that("workflow with recipe - find_predictors", {
  rec <- recipes::recipe(mpg ~ am + vs + cyl, data = mtcars)
  rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())

  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, rec)
  wf <- workflows::add_model(wf, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  wf_fit <- parsnip::fit(wf, data = mtcars)

  expect_identical(find_predictors(wf_fit), list(conditional = c("am", "vs", "cyl")))
  expect_identical(find_predictors(wf_fit, flatten = TRUE), c("am", "vs", "cyl"))
})

test_that("workflow with recipe - find_response, find_variables, get_data", {
  rec <- recipes::recipe(mpg ~ am + vs, data = mtcars)
  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, rec)
  wf <- workflows::add_model(wf, parsnip::set_engine(parsnip::linear_reg(), "lm"))
  wf_fit <- parsnip::fit(wf, data = mtcars)

  expect_identical(find_response(wf_fit), "mpg")

  vars <- find_variables(wf_fit)
  expect_identical(vars$response, "mpg")
  expect_identical(vars$conditional, c("am", "vs"))

  data <- get_data(wf_fit)
  expect_identical(nrow(data), 32L)
  expect_true(all(c("mpg", "am", "vs") %in% colnames(data)))
})

test_that("workflow with formula - find_predictors, find_variables, get_data", {
  wf <- workflows::workflow()
  wf <- workflows::add_formula(wf, mpg ~ am + vs + cyl)
  wf <- workflows::add_model(wf, parsnip::set_engine(parsnip::linear_reg(), "lm"))
  wf_fit <- parsnip::fit(wf, data = mtcars)

  expect_identical(find_predictors(wf_fit), list(conditional = c("am", "vs", "cyl")))
  expect_identical(find_predictors(wf_fit, flatten = TRUE), c("am", "vs", "cyl"))

  vars <- find_variables(wf_fit)
  expect_identical(vars$response, "mpg")
  expect_identical(vars$conditional, c("am", "vs", "cyl"))

  data <- get_data(wf_fit)
  expect_identical(nrow(data), 32L)
  expect_true(all(c("mpg", "am", "vs", "cyl") %in% colnames(data)))
})

test_that("workflow with complex recipe transformations", {
  # Create a dataset with numeric and categorical variables
  test_data <- mtcars
  test_data$cyl <- factor(test_data$cyl)
  test_data$gear <- factor(test_data$gear)

  rec <- recipes::recipe(mpg ~ ., data = test_data)
  rec <- recipes::step_log(rec, mpg, base = 10)
  rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())
  rec <- recipes::step_dummy(
    rec,
    recipes::all_nominal_predictors(),
    contrasts = "contr.treatment"
  )

  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, rec)
  wf <- workflows::add_model(wf, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  wf_fit <- parsnip::fit(wf, data = test_data)

  # Test find_predictors
  predictors <- find_predictors(wf_fit, flatten = TRUE)
  expect_true(length(predictors) > 0)
  expect_true("disp" %in% predictors)
  expect_true("hp" %in% predictors)
  expect_true("cyl" %in% predictors)
  expect_true("gear" %in% predictors)
  expect_false("mpg" %in% predictors) # Response should not be in predictors

  # Test find_response
  expect_identical(find_response(wf_fit), "mpg")

  # Test find_variables
  vars <- find_variables(wf_fit)
  expect_identical(vars$response, "mpg")
  expect_true(length(vars$conditional) >= 10) # All predictor variables

  # Test get_data
  data <- get_data(wf_fit)
  expect_equal(nrow(data), nrow(test_data))
  expect_true("mpg" %in% colnames(data))
  expect_true("disp" %in% colnames(data))
  expect_true("cyl" %in% colnames(data))
})
