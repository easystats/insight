skip_if_not_installed("survey")

data(api, package = "survey")
dstrat <- survey::svydesign(
  id = ~1,
  strata = ~stype,
  weights = ~pw,
  data = apistrat,
  fpc = ~fpc
)

m1 <- survey::svyglm(api00 ~ ell + meals + mobility, design = dstrat)

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = c("ell", "meals", "mobility"), design = c("stype", "fpc"))
  )
  expect_identical(
    find_predictors(m1, component = "conditional"),
    list(conditional = c("ell", "meals", "mobility"))
  )
  expect_identical(
    find_predictors(m1, component = "design"),
    list(design = c("stype", "fpc"))
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("ell", "meals", "mobility", "stype", "fpc")
  )
  expect_identical(
    find_predictors(dstrat),
    list(design = c("stype", "fpc"))
  )
  expect_identical(
    find_predictors(dstrat, flatten = TRUE),
    c("stype", "fpc")
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "api00",
      conditional = c("ell", "meals", "mobility"),
      design = c("stype", "fpc")
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("api00", "ell", "meals", "mobility", "stype", "fpc")
  )
  expect_identical(
    find_variables(dstrat),
    list(design = c("stype", "fpc"))
  )
  expect_identical(
    find_variables(dstrat, flatten = TRUE),
    c("stype", "fpc")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), "api00")
})

test_that("get_response", {
  expect_equal(get_response(m1), apistrat$api00)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 200)
  expect_named(
    get_data(m1, source = "mf", verbose = FALSE),
    c("stype", "api00", "meals", "ell", "mobility", "pw", "fpc")
  )
  expect_named(
    get_data(m1, verbose = FALSE),
    c("api00", "ell", "meals", "mobility", "(weights)")
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("api00 ~ ell + meals + mobility")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "api00",
      conditional = c("ell", "meals", "mobility")
    )
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("api00", "ell", "meals", "mobility")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 200)
})

test_that("find_weights", {
  expect_identical(find_weights(m1, source = "mf"), "pw")
  expect_identical(find_weights(dstrat, source = "mf"), "pw")
  expect_identical(find_weights(m1), "(weights)")
  expect_identical(find_weights(dstrat), "(weights)")
})

test_that("get_weights", {
  expect_equal(get_weights(m1, source = "mf"), apistrat$pw, tolerance = 1e-4)
  expect_equal(get_weights(dstrat, source = "mf"), apistrat$pw, tolerance = 1e-4)
  expect_equal(get_weights(m1), m1$weights, tolerance = 1e-4, ignore_attr = TRUE)
  expect_null(get_weights(dstrat))
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      conditional = c(
        "(Intercept)",
        "ell",
        "meals",
        "mobility"
      )
    )
  )
  expect_equal(nrow(get_parameters(m1)), 4)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "ell", "meals", "mobility")
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})

test_that("get_data for svyglm and survey.design", {
  set.seed(123)
  n <- 5
  x <- runif(n)
  y <- runif(n)
  z <- runif(n)
  w <- rnorm(n, 1, 0.05)

  dat <- data.frame(w, x, y, z)

  des <- survey::svydesign(~1, weights = ~w, data = dat)
  svy_fit <- survey::svyglm(y ~ poly(x, 2), design = des)

  out <- get_data(svy_fit, source = "mf")
  expect_equal(out$x, dat$x)
  expect_named(out, c("w", "x", "y"))

  out <- get_data(des, source = "mf")
  expect_named(out, c("w", "x", "y", "z"))
  expect_equal(out$x, dat$x)

  expect_identical(find_weights(svy_fit, source = "environment"), "(weights)")
  expect_identical(find_weights(svy_fit, source = "mf"), "w")

  expect_named(get_data(svy_fit, source = "environment"), c("y", "(weights)", "x", "x.1"))
  expect_named(get_data(svy_fit, source = "mf"), c("w", "x", "y"))

  expect_equal(
    get_weights(svy_fit, source = "environment"),
    c(1.0204, 0.87617, 1.04004, 0.98315, 1.08023),
    tolerance = 1e-3
  )
  expect_equal(
    get_weights(svy_fit, source = "mf"),
    c(1.06403, 0.91364, 1.08451, 1.02519, 1.12642),
    tolerance = 1e-3
  )
})

test_that("data and weights from different sources", {
  data("fpc", package = "survey")
  svyd <- survey::svydesign(
    weights = ~weight,
    ids = ~psuid,
    strata = ~stratid,
    fpc = ~Nh,
    variables = ~ x + nh,
    data = fpc,
    nest = TRUE
  )
  mod <- survey::svyglm(x ~ nh, design = svyd)

  expect_identical(find_weights(mod, source = "environment"), "(weights)")
  expect_equal(
    get_weights(mod, source = "environment"),
    c(0.88889, 0.88889, 0.88889, 0.88889, 0.88889, 1.18519, 1.18519, 1.18519),
    tolerance = 1e-3
  )
  expect_named(get_data(mod, source = "environment"), c("x", "nh", "(weights)"))

  expect_identical(find_weights(mod, source = "mf"), "weight")
  expect_null(get_weights(mod, source = "mf"))
  expect_named(get_data(mod, source = "mf"), c("x", "nh"))
})
