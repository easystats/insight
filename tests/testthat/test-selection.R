skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("sampleSelection")
skip_if_not_installed("mvtnorm")

set.seed(0)
vc <- diag(3)
vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)
vc[upper.tri(vc)] <- vc[lower.tri(vc)]
eps <- rmvnorm(500, c(0, 0, 0), vc)
xs <- runif(500)
ys <- xs + eps[, 1] > 0
xo1 <- runif(500)
yo1 <- xo1 + eps[, 2]
xo2 <- runif(500)
yo2 <- xo2 + eps[, 3]
yo <- ifelse(ys, yo2, yo1)
ys <- as.numeric(ys) + 1
dat <- data.frame(ys, yo, yo1, yo2, xs, xo1, xo2)
m1 <- sampleSelection::selection(ys ~ xs, list(yo1 ~ xo1, yo2 ~ xo2), data = dat)

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = list(selection = "xs", outcome = c("xo1", "xo2")))
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), c("ys", "yo1", "yo2"))
})

test_that("get_response", {
  expect_equal(get_response(m1), dat[c("ys", "yo1", "yo2")], ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_named(get_predictors(m1), c("xs", "xo1", "xo2"))
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 500)
  expect_named(get_data(m1, verbose = FALSE), c("ys", "yo1", "yo2", "xs", "xo1", "xo2"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(
      conditional = list(
        selection = ys ~ xs,
        outcome = list(yo1 ~ xo1, yo2 ~ xo2)
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(conditional = list(selection = "ys  xs", outcome = c("yo1  xo1", "yo2  xo2")))
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("ys  xs", "yo1  xo1", "yo2  xo2")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 500)
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      selection = c("(Intercept)", "xs"),
      auxiliary = c("(Intercept)", "xo1", "sigma1", "rho1", "(Intercept)", "xo2", "sigma2", "rho2")
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})
