skip_if_not_installed("pscl")

data(bioChemists, package = "pscl")

m1 <- pscl::zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

test_that("model_info", {
  expect_true(model_info(m1)$is_poisson)
  expect_true(model_info(m1)$is_zero_inflated)
  expect_false(model_info(m1)$is_linear)
})

test_that("n_parameters", {
  expect_identical(n_parameters(m1), 8L)
  expect_identical(n_parameters(m1, component = "conditional"), 5L)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(
      conditional = c("fem", "mar", "kid5", "ment"),
      zero_inflated = c("kid5", "phd")
    )
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("fem", "mar", "kid5", "ment", "phd")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(m1), "art")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), exp(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1)), 915L)
  expect_named(
    get_data(m1),
    c("art", "fem", "mar", "kid5", "ment", "phd")
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("art ~ fem + mar + kid5 + ment"),
      zero_inflated = as.formula("~kid5 + phd")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "art",
      conditional = c("fem", "mar", "kid5", "ment"),
      zero_inflated = c("kid5", "phd")
    )
  )
  expect_identical(
    find_terms(m1, flatten = TRUE),
    c("art", "fem", "mar", "kid5", "ment", "phd")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 915L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c(
        "count_(Intercept)",
        "count_femWomen",
        "count_marMarried",
        "count_kid5",
        "count_ment"
      ),
      zero_inflated = c("zero_(Intercept)", "zero_kid5", "zero_phd")
    )
  )
  expect_identical(nrow(get_parameters(m1)), 8L)
  expect_identical(nrow(get_parameters(m1, component = "zi")), 3L)
  expect_identical(
    get_parameters(m1)$Parameter,
    c(
      "count_(Intercept)",
      "count_femWomen",
      "count_marMarried",
      "count_kid5",
      "count_ment",
      "zero_(Intercept)",
      "zero_kid5",
      "zero_phd"
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})

test_that("get_statistic", {
  expect_equal(
    get_statistic(m1)$Statistic,
    c(8.26297, -3.90986, 2.07134, -3.43156, 10.05389, -2.143, 0.21384, -1.84259),
    tolerance = 1e-3
  )
  expect_equal(
    get_statistic(m1)$Component,
    c(
      "conditional", "conditional", "conditional", "conditional",
      "conditional", "zero_inflated", "zero_inflated", "zero_inflated"
    ),
    tolerance = 1e-3
  )
})

test_that("get_varcov", {
  # needs to be loaded
  suppressPackageStartupMessages({
    suppressWarnings(suppressMessages(library(sandwich, quietly = TRUE, warn.conflicts = FALSE))) # nolint
  })

  set.seed(123)
  vc1 <- get_varcov(m1, component = "all", vcov = "BS", vcov_args = list(R = 50))
  set.seed(123)
  vc2 <- sandwich::vcovBS(m1, R = 50)
  expect_equal(vc1, vc2, ignore_attr = TRUE)

  set.seed(123)
  vc1 <- get_varcov(m1, component = "conditional", vcov = "BS", vcov_args = list(R = 50))
  count_col <- startsWith(colnames(vc2), "count_")
  expect_equal(vc1, vc2[count_col, count_col], ignore_attr = TRUE)

  set.seed(123)
  vc1 <- get_varcov(m1, component = "zero_inflated", vcov = "BS", vcov_args = list(R = 50))
  zero_col <- startsWith(colnames(vc2), "zero_")
  expect_equal(vc1, vc2[zero_col, zero_col], ignore_attr = TRUE)
})

# fixes in calculation of test-statistic in pscl 1.5.9+
skip_if(packageVersion("pscl") < "1.5.9")
m2 <- pscl::zeroinfl(formula = art ~ . | 1, data = bioChemists, dist = "negbin")

test_that("get_statistic", {
  expect_equal(
    get_statistic(m2)$Statistic,
    c(1.8486, -2.97799, 1.83288, -3.32486, 0.42372, 8.38053, -0.21241),
    tolerance = 1e-3
  )
  expect_equal(
    get_statistic(m2)$Component,
    c(
      "conditional", "conditional", "conditional", "conditional",
      "conditional", "conditional", "zero_inflated"
    ),
    tolerance = 1e-3
  )
})
