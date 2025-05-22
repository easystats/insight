skip_if_not_installed("sdmTMB")

set.seed(1)
predictor_dat <- data.frame(
  X = runif(300), Y = runif(300),
  b1 = rnorm(300), b2 = rnorm(300),
  year = rep(1:6, each = 50)
)
mesh <- sdmTMB::make_mesh(predictor_dat, xy_cols = c("X", "Y"), cutoff = 0.1)

sim_dat <- sdmTMB::sdmTMB_simulate(
  formula = ~ 1 + b1 + b2,
  data = predictor_dat,
  time = "year",
  mesh = mesh,
  family = gaussian(),
  range = 0.5,
  sigma_E = 0.2,
  sigma_O = 0.7,
  phi = 0.9,
  seed = 123,
  B = c(0.2, -0.4, 0.3)
)
m1 <- sdmTMB::sdmTMB(observed ~ 1 + b1 + b2, data = sim_dat, mesh = mesh, time = "year")

data(pcod_2011, package = "sdmTMB")
mesh <- sdmTMB::make_mesh(pcod_2011, c("X", "Y"), cutoff = 20)

pcod_2011$count <- round(pcod_2011$density)
m2 <- sdmTMB::sdmTMB(
  count ~ s(depth),
  data = pcod_2011, mesh = mesh,
  spatial = "off",
  family = sdmTMB::delta_truncated_nbinom2()
)

pcod_2011$fyear <- as.factor(pcod_2011$year)
m3 <- sdmTMB::sdmTMB(
  density ~ s(depth) + (1 | fyear), #<
  data = pcod_2011, mesh = mesh,
  family = sdmTMB::tweedie(link = "log")
)

m4 <- sdmTMB(
  density ~ 1 + (depth | fyear), #<
  data = pcod_2011, mesh = mesh,
  family = tweedie(link = "log")
)


test_that("find_formula", {
  expect_equal(
    find_formula(m1),
    list(conditional = observed ~ 1 + b1 + b2),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2),
    list(conditional =  count ~ s(depth)),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m3),
    list(
      conditional = density ~ s(depth),
      random = ~1 | fyear
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m4),
    list(
      conditional = density ~ 1,
      random = ~depth | fyear
    ),
    ignore_attr = TRUE
  )
})




test_that("model_info", {
  expect_true(model_info(m1)$is_poisson)
  expect_true(model_info(m1)$is_zero_inflated)
  expect_false(model_info(m1)$is_linear)
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

test_that("get_response", {
  expect_equal(get_response(m1), bioChemists$art)
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), exp(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1)), 915)
  expect_equal(
    colnames(get_data(m1)),
    c("art", "fem", "mar", "kid5", "ment", "phd")
  )
})

test_that("get_df", {
  expect_equal(
    get_df(m1, type = "residual"),
    df.residual(m1),
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1, type = "normal"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1, type = "wald"),
    Inf,
    ignore_attr = TRUE
  )
})


test_that("find_variables", {
  expect_equal(
    find_variables(m1),
    list(
      response = "art",
      conditional = c("fem", "mar", "kid5", "ment"),
      zero_inflated = c("kid5", "phd")
    )
  )
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("art", "fem", "mar", "kid5", "ment", "phd")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 915)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_equal(
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
  expect_equal(nrow(get_parameters(m1)), 8)
  expect_equal(nrow(get_parameters(m1, component = "zi")), 3)
  expect_equal(
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

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "art",
      conditional = c("fem", "mar", "kid5", "ment"),
      zero_inflated = c("kid5", "phd")
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})
