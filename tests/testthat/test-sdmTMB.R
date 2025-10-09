skip_on_cran()
skip_if_not_installed("sdmTMB")

set.seed(1)
predictor_dat <- data.frame(
  X = runif(300),
  Y = runif(300),
  b1 = rnorm(300),
  b2 = rnorm(300),
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
  data = pcod_2011,
  mesh = mesh,
  spatial = "off",
  family = sdmTMB::delta_truncated_nbinom2()
)

m5 <- sdmTMB::sdmTMB(
  count ~ depth,
  data = pcod_2011,
  mesh = mesh,
  spatial = "off",
  family = sdmTMB::delta_truncated_nbinom2()
)

pcod_2011$fyear <- as.factor(pcod_2011$year)
m3 <- sdmTMB::sdmTMB(
  density ~ s(depth) + (1 | fyear), #<
  data = pcod_2011,
  mesh = mesh,
  family = sdmTMB::tweedie(link = "log")
)

m4 <- suppressWarnings(sdmTMB::sdmTMB(
  density ~ 1 + (depth | fyear), #<
  data = pcod_2011,
  mesh = mesh,
  family = sdmTMB::tweedie(link = "log")
))


test_that("find_formula", {
  expect_equal(
    find_formula(m1),
    list(conditional = observed ~ 1 + b1 + b2),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2),
    list(conditional = count ~ s(depth)),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m3),
    list(
      conditional = density ~ s(depth),
      random = ~ 1 | fyear
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m4),
    list(
      conditional = density ~ 1,
      random = ~ depth | fyear
    ),
    ignore_attr = TRUE
  )
})


test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = c("b1", "b2"), time = "year")
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("b1", "b2", "year")
  )
  expect_null(find_predictors(m1, effects = "random"))

  expect_identical(
    find_predictors(m2),
    list(conditional = "depth")
  )
  expect_identical(
    find_predictors(m2, flatten = TRUE),
    "depth"
  )
  expect_null(find_predictors(m2, effects = "random"))

  expect_identical(
    find_predictors(m3),
    list(conditional = "depth")
  )
  expect_identical(
    find_predictors(m3, "all"),
    list(conditional = "depth", random = "fyear")
  )
  expect_identical(
    find_predictors(m3, flatten = TRUE),
    "depth"
  )
  expect_identical(
    find_predictors(m3, "random"),
    list(random = "fyear")
  )

  expect_null(find_predictors(m4))
  expect_identical(
    find_predictors(m4, "all"),
    list(random = c("depth", "fyear"))
  )
  expect_null(find_predictors(m4, flatten = TRUE))
  expect_identical(find_predictors(m4, "random", flatten = TRUE), "fyear")
})


test_that("find_response", {
  expect_identical(find_response(m1), "observed")
  expect_identical(find_response(m2), "count")
  expect_identical(find_response(m3), "density")
  expect_identical(find_response(m4), "density")
})


test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
  expect_true(model_info(m2)$is_hurdle)
  expect_true(model_info(m3)$is_tweedie)
  expect_true(model_info(m4)$is_tweedie)
})


test_that("link_function", {
  expect_equal(link_function(m1)(0.2), 0.2, tolerance = 1e-5)
  expect_equal(link_function(m2)(0.2), log(0.2), tolerance = 1e-5)
  expect_equal(link_function(m3)(0.2), log(0.2), tolerance = 1e-5)
  expect_equal(link_function(m4)(0.2), log(0.2), tolerance = 1e-5)
})


test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
  expect_equal(link_inverse(m2)(0.2), exp(0.2), tolerance = 1e-5)
  expect_equal(link_inverse(m3)(0.2), exp(0.2), tolerance = 1e-5)
  expect_equal(link_inverse(m4)(0.2), exp(0.2), tolerance = 1e-5)
})


test_that("get_data", {
  expect_equal(dim(get_data(m1)), c(300L, 4L))
  expect_equal(dim(get_data(m2)), c(969L, 2L))
  expect_equal(dim(get_data(m3)), c(969L, 3L))
  expect_equal(dim(get_data(m4)), c(969L, 3L))
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


test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "z-statistic")
  expect_identical(find_statistic(m3), "z-statistic")
  expect_identical(find_statistic(m4), "z-statistic")
})


test_that("get_vcov", {
  skip_on_os(c("mac", "linux"))
  expect_identical(dim(get_varcov(m1)), c(3L, 3L))
  expect_identical(colnames(get_varcov(m1)), c("(Intercept)", "b1", "b2"))
  expect_identical(dim(get_varcov(m2)), c(3L, 3L))
  expect_identical(colnames(get_varcov(m2)), c("(Intercept)", "(Intercept)", "sdepth"))
  expect_identical(dim(get_varcov(m3)), c(2L, 2L))
  expect_identical(colnames(get_varcov(m3)), c("(Intercept)", "sdepth"))
  expect_identical(dim(suppressWarnings(get_varcov(m4))), c(1L, 1L))
  expect_identical(colnames(suppressWarnings(get_varcov(m4))), "(Intercept)")
  expect_identical(dim(get_varcov(m5)), c(3L, 3L))
  expect_identical(colnames(get_varcov(m5)), c("(Intercept)", "depth", "(Intercept)"))
})


test_that("get_statistic", {
  skip_on_os(c("mac", "linux"))
  expect_identical(nrow(get_statistic(m1)), 3L)
  expect_identical(nrow(get_statistic(m2)), 4L)
  expect_identical(nrow(get_statistic(m3)), 2L)
  expect_identical(nrow(suppressWarnings(get_statistic(m4))), 1L)
  expect_identical(nrow(get_statistic(m5)), 4L)

  expect_equal(
    get_statistic(m1)$Statistic,
    c(-0.54984, -6.67849, 5.81354),
    tolerance = 1e-4
  )
  expect_equal(
    get_statistic(m2)$Statistic,
    c(-3.28141, 0.12244, 19.28922, -0.15622),
    tolerance = 1e-4
  )
})
