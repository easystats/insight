skip_if_not_installed("gam")

s <- gam::s

data(kyphosis, package = "rpart")
void <- capture.output(m1 <- gam::gam(
  Kyphosis ~ s(Age, 4) + Number,
  family = binomial,
  data = kyphosis,
  trace = TRUE
))

test_that("model_info", {
  expect_true(model_info(m1)$is_binomial)
  expect_false(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("Age", "Number")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("Age", "Number"))
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(m1), "Kyphosis")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1)), 81)
  expect_equal(colnames(get_data(m1)), c("Kyphosis", "Age", "Number"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("Kyphosis ~ s(Age, 4) + Number")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "Kyphosis",
      conditional = c("s(Age, 4)", "Number")
    )
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("Kyphosis", "s(Age, 4)", "Number")
  )
})

test_that("find_variables", {
  expect_equal(
    find_variables(m1),
    list(
      response = "Kyphosis",
      conditional = c("Age", "Number")
    )
  )
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("Kyphosis", "Age", "Number")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 81)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      conditional = c("(Intercept)", "Number"),
      smooth_terms = "s(Age, 4)"
    )
  )
  expect_equal(nrow(get_parameters(m1)), 3)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "Number", "s(Age, 4)")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_algorithm", {
  expect_equal(find_algorithm(m1), list(algorithm = "IWLS"))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "F-statistic")
})
