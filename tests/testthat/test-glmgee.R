skip_if_not_installed("glmtoolbox")

data(spruces, package = "glmtoolbox")
m1 <- glmtoolbox::glmgee(
  size ~ days + treat,
  id = tree,
  family = Gamma(log),
  corstr = "AR-M-dependent(1)",
  data = spruces
)

test_that("model_info", {
  expect_true(model_info(m1)$is_exponential)
  expect_identical(model_info(m1)$family, "Gamma")
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("days", "treat")))
  expect_identical(
    find_predictors(m1, effects = "random"),
    list(random = "tree")
  )
  expect_identical(
    find_predictors(m1, effects = "all", flatten = TRUE),
    c("days", "treat", "tree")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), "size")
})

test_that("get_response", {
  expect_equal(get_response(m1), spruces$size, ignore_attr = TRUE)
})

test_that("find_random", {
  expect_identical(find_random(m1), list(random = "tree"))
})

test_that("get_random", {
  expect_equal(get_random(m1), spruces[, "tree", drop = FALSE], ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_equal(get_predictors(m1), spruces[c("days", "treat")], tolerance = 1e-4)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 1.221403, tolerance = 1e-3)
})

test_that("link_fun", {
  expect_equal(link_function(m1)(0.2), -1.609438, tolerance = 1e-3)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1)), 1027L)
  expect_named(get_data(m1), c("size", "days", "treat", "tree"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(conditional = size ~ days + treat, random = ~tree),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(response = "size", conditional = c("days", "treat"), random = "tree")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 1027L)
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c("(Intercept)", "days", "treatozone-enriched"),
      dispersion = "Dispersion"
    )
  )
  expect_identical(
    find_parameters(m1, component = "conditional"),
    list(conditional = c("(Intercept)", "days", "treatozone-enriched"))
  )
  expect_identical(nrow(get_parameters(m1)), 4L)
  expect_identical(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "days", "treatozone-enriched", "Dispersion")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})

test_that("get_varcov", {
  out <- get_varcov(m1)
  expect_equal(out[, 1], c(0.0122241, 0, -0.0110037), tolerance = 1e-3, ignore_attr = TRUE)
  out <- get_varcov(m1, vcov = "model")
  expect_equal(out[, 1], c(0.0223564, -5.4e-06, -0.0201031), tolerance = 1e-3, ignore_attr = TRUE)
})
