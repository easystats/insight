test_that("`find_formula` works with `mgcv::gam()`", {
  skip_if_not_installed("mgcv")
  set.seed(2) ## simulate some data...
  dat <- mgcv::gamSim(1, n = 50, dist = "normal", scale = 2, verbose = FALSE)
  b <- mgcv::gam(list(y ~ s(x0) + s(x1) + s(x2), ~ s(x3)), family = mgcv::gaulss(), data = dat)

  f <- find_formula(b)
  expect_named(f, c("conditional", "scale"))
  expect_identical(f$conditional, formula("y ~ s(x0) + s(x1) + s(x2)"))
  expect_identical(f$scale, formula("~s(x3)"))
})


test_that("formula warns for badly formulated response", {
  data(iris)
  model <- lm(iris[, 2] ~ Species, data = iris)
  expect_warning(formula_ok(model), regex = "Using indexed")
})

test_that("formula warns when using backticks", {
  data(mtcars)
  colnames(mtcars)[1] <- "1_mpg"
  m <- lm(`1_mpg` ~ gear, data = mtcars)
  expect_warning(expect_false(formula_ok(m)), regex = "syntactically")
  expect_silent(expect_true(formula_ok(m, checks = "dollar")))
  expect_error(formula_ok(m, checks = c("dollar", "test")), regex = "invalid options")
  # null model checks formula
  expect_warning(null_model(m), regex = "Looks like")
  expect_silent(null_model(m, verbose = FALSE))
  # message types
  expect_message(expect_false(formula_ok(m, action = "message")), regex = "syntactically")
  expect_error(expect_false(formula_ok(m, action = "error")), regex = "syntactically")
  # add own message
  expect_message(expect_false(
    formula_ok(m, action = "message", prefix_msg = "Test-Unit failed.")
  ), regex = "Test-Unit failed")
})
