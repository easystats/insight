skip_if_not_installed("gee")

data(warpbreaks)
void <- capture.output(suppressMessages({
  m1 <- gee::gee(breaks ~ tension, id = wool, data = warpbreaks)
}))

set.seed(123)
n <- 600
dat <- data.frame(
  depression = rbinom(n, 1, prob = 0.15),
  drug = rbinom(n, 1, prob = 0.5),
  time = rep(1:3, n / 3),
  id = rep(1:200, each = 3)
)

# test for #770
junk <- capture.output({
  dep_gee <- suppressMessages(gee::gee(depression ~ drug * time,
    data = dat,
    id = id,
    family = binomial,
    corstr = "independence"
  ))
})

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
  expect_true(model_info(dep_gee)$is_binomial)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = "tension"))
  expect_identical(find_predictors(m1, flatten = TRUE), "tension")
  expect_identical(
    find_predictors(m1, effects = "random"),
    list(random = "wool")
  )
  expect_identical(
    find_predictors(m1, effects = "all", flatten = TRUE),
    c("tension", "wool")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), "breaks")
})

test_that("get_response", {
  expect_equal(get_response(m1), warpbreaks$breaks, ignore_attr = TRUE)
})

test_that("find_random", {
  expect_identical(find_random(m1), list(random = "wool"))
})

test_that("get_random", {
  expect_equal(get_random(m1), warpbreaks[, "wool", drop = FALSE], ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_equal(get_predictors(m1), warpbreaks[, "tension", drop = FALSE], tolerance = 1e-4)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1)), 54L)
  expect_named(get_data(m1), c("breaks", "tension", "wool"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("breaks ~ tension"),
      random = as.formula("~wool")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "breaks",
      conditional = "tension",
      random = "wool"
    )
  )
  expect_identical(
    find_terms(m1, flatten = TRUE),
    c("breaks", "tension", "wool")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 54L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(conditional = c(
      "(Intercept)", "tensionM", "tensionH"
    ))
  )
  expect_identical(nrow(get_parameters(m1)), 3L)
  expect_identical(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "tensionM", "tensionH")
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_algorithm", {
  expect_identical(find_algorithm(m1), list(algorithm = "ML"))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})
