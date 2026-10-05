skip_if_not_installed("gamlss")
skip_if_not_installed("gamlss.data")

pb <- gamlss::pb

data(abdom, package = "gamlss.data")
data(usair, package = "gamlss.data")

void <- capture.output({
  m_gamlss1 <- gamlss::gamlss(
    y ~ pb(x),
    sigma.formula = ~ pb(x),
    family = "BCT",
    data = abdom,
    method = mixed(1, 20)
  )
})

void <- capture.output({
  m_gamlss2 <- gamlss::gamlss(
    y ~ x1 + x2 + x3,
    sigma.formula = ~ x4 + x5 + x6 + x4:x5,
    nu.formula = ~ x2 + x5,
    tau.formula = ~ x1 + x4 + x5 + x6 + x1:x4,
    family = "ZIBNB",
    data = usair
  )
})

test_that("model_info", {
  expect_true(model_info(m_gamlss1)$is_linear)
  expect_true(model_info(m_gamlss2)$is_zero_inflated)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m_gamlss1), list(conditional = "x", sigma = "x"))
  expect_identical(find_predictors(m_gamlss1, flatten = TRUE), "x")
  expect_null(find_predictors(m_gamlss1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m_gamlss1))
})

test_that("get_random", {
  expect_warning(get_random(m_gamlss1))
})

test_that("find_response", {
  expect_identical(find_response(m_gamlss1), "y")
})

test_that("get_response", {
  expect_identical(get_response(m_gamlss1), abdom$y)
})

test_that("get_predictors", {
  expect_identical(colnames(get_predictors(m_gamlss1)), "x")
})

test_that("get_data", {
  expect_identical(nrow(get_data(m_gamlss1)), 610L)
  expect_identical(colnames(get_data(m_gamlss1)), c("y", "x"))
})

test_that("find_formula", {
  expect_length(find_formula(m_gamlss1), 4)
  expect_equal(
    find_formula(m_gamlss1),
    list(
      conditional = as.formula("y ~ pb(x)"),
      sigma = as.formula("~pb(x)"),
      nu = as.formula("~1"),
      tau = as.formula("~1")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m_gamlss1),
    list(
      response = "y",
      conditional = "x",
      sigma = "x"
    )
  )
  expect_identical(find_variables(m_gamlss1, flatten = TRUE), c("y", "x"))
})

test_that("find_terms", {
  expect_identical(
    find_terms(m_gamlss1),
    list(
      response = "y",
      conditional = "pb(x)",
      sigma = "pb(x)",
      nu = "1",
      tau = "1"
    )
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m_gamlss1), 610L)
})

test_that("link_function", {
  expect_equal(link_function(m_gamlss1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m_gamlss1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m_gamlss1),
    list(
      conditional = c("(Intercept)", "pb(x)"),
      sigma = c("(Intercept)", "pb(x)"),
      nu = "(Intercept)",
      tau = "(Intercept)"
    )
  )
  expect_identical(nrow(get_parameters(m_gamlss1)), 6L)
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m_gamlss1))
})

test_that("find_algorithm", {
  expect_identical(find_algorithm(m_gamlss1), list(algorithm = "mixed"))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m_gamlss1), "t-statistic")
})

test_that("find_formula works with namespace colons", {
  data(iris)
  m <- gamlss::gamlss(
    Sepal.Length ~ Sepal.Width + gamlss::random(Species),
    sigma.formula = ~Sepal.Width,
    data = iris
  )
  expect_equal(
    find_formula(m),
    list(
      conditional = Sepal.Length ~ Sepal.Width,
      random = ~ 1 | Species,
      sigma = ~Sepal.Width
    ),
    ignore_attr = TRUE
  )
})

test_that("link_inv for LOGNO", {
  data(abdom, package = "gamlss.data")
  m1 <- gamlss::gamlss(y ~ x, family = "LOGNO", data = abdom)
  expect_equal(link_inverse(m1)(0.2), exp(0.2), tolerance = 1e-4)
  expect_equal(link_function(m1)(0.2), log(0.2), tolerance = 1e-4)
})

test_that("find_parameters", {
  set.seed(123)
  dat <<- data.frame(
    Y = sample(20:50, 100, replace = TRUE),
    date = sample(seq(as.Date("1999/01/01"), as.Date("2000/01/01"), by = "day"), 10),
    cont1 = rchisq(100, df = 2),
    cont2 = runif(100),
    cat1 = sample(LETTERS[1:3], 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  m <- gamlss::gamlss(
    Y ~ date + scale(cont1) + scale(cont2) + I(scale(cont2)^2) * cat1,
    data = dat
  )
  expect_named(find_parameters(m), c("conditional", "sigma"))
  expect_identical(
    find_parameters(m)$conditional,
    c(
      "(Intercept)",
      "date",
      "scale(cont1)",
      "scale(cont2)",
      "I(scale(cont2)^2)",
      "cat1B",
      "cat1C",
      "I(scale(cont2)^2):cat1B",
      "I(scale(cont2)^2):cat1C"
    )
  )
})

test_that("get_residuals, type = 'response'", {
  counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  d_po <- data.frame(
    counts = counts,
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )
  void <- capture.output({
    m_po <- gamlss::gamlss(counts ~ outcome + treatment, family = "PO", data = d_po)
  })
  m_glm <- stats::glm(counts ~ outcome + treatment, family = poisson(), data = d_po)
  expect_equal(
    unname(get_residuals(m_po, type = "response")),
    unname(get_residuals(m_glm, type = "response")),
    tolerance = 1e-4
  )

  # default residuals are still the normalized quantile residuals
  expect_equal(get_residuals(m_gamlss1), m_gamlss1$residuals, ignore_attr = TRUE)

  # for zero-inflated families, the mean is not mu
  set.seed(123)
  d_zip <- data.frame(x = rnorm(200))
  d_zip$y <- ifelse(runif(200) < 0.3, 0, rpois(200, exp(1 + 0.3 * d_zip$x)))
  void <- capture.output({
    m_zip <- gamlss::gamlss(y ~ x, family = "ZIP", data = d_zip)
  })
  expected <- (1 - stats::fitted(m_zip, "sigma")) * stats::fitted(m_zip, "mu")
  expect_equal(
    unname(get_residuals(m_zip, type = "response")),
    unname(d_zip$y - expected),
    tolerance = 1e-4
  )

  # families without a mean function fall back to mu, with a warning
  expect_warning(
    out <- get_residuals(m_gamlss1, type = "response"),
    regexp = "BCT"
  )
  expect_equal(
    unname(out),
    unname(abdom$y - stats::fitted(m_gamlss1, "mu")),
    tolerance = 1e-4
  )
  expect_silent(get_residuals(m_gamlss1, type = "response", verbose = FALSE))
})
