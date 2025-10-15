skip_if_not_installed("survey")
skip_if_not_installed("survival")

data(pbc, package = "survival")

pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
biasmodel <- glm(randomized ~ age * edema, data = pbc, family = binomial)
pbc$randprob <- fitted(biasmodel)

dpbc <- survey::svydesign(
  id = ~1,
  prob = ~randprob,
  strata = ~edema,
  data = subset(pbc, randomized)
)
rpbc <- survey::as.svrepdesign(dpbc)

m1 <- survey::svycoxph(
  Surv(time, status > 0) ~ log(bili) + protime + albumin,
  design = dpbc
)

test_that("model_info", {
  expect_true(model_info(m1)$is_survival)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = c("bili", "protime", "albumin"), design = c("edema", "randprob"))
  )
  expect_identical(
    find_predictors(m1, component = "conditional"),
    list(conditional = c("bili", "protime", "albumin"))
  )
  expect_identical(
    find_predictors(m1, component = "design"),
    list(design = c("edema", "randprob"))
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("bili", "protime", "albumin", "edema", "randprob")
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = c("time", "status"),
      conditional = c("bili", "protime", "albumin"),
      design = c("edema", "randprob")
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("time", "status", "bili", "protime", "albumin", "edema", "randprob")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), "Surv(time, status > 0)")
})

test_that("get_response", {
  expect_equal(
    get_response(m1, source = "mf"),
    subset(pbc, randomized)[c("time", "status")],
    ignore_attr = TRUE
  )
  expect_equal(
    head(get_response(m1)),
    data.frame(
      time = c(400, 4500, 1012, 1925, 1504, 2503),
      status = c(1, 0, 1, 1, 1, 1)
    ),
    ignore_attr = TRUE
  )
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 312)
  expect_named(
    get_data(m1, source = "mf", verbose = FALSE),
    c("time", "status", "edema", "bili", "albumin", "protime", "randprob")
  )
  expect_named(
    get_data(m1, verbose = FALSE),
    c(
      "time",
      "status",
      "Surv(time, status > 0)",
      "bili",
      "protime",
      "albumin",
      "(weights)"
    )
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("Surv(time, status > 0) ~ log(bili) + protime + albumin")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "Surv(time, status > 0)",
      conditional = c("log(bili)", "protime", "albumin")
    )
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("Surv(time, status > 0)", "log(bili)", "protime", "albumin")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 312)
})

test_that("find_weights", {
  expect_null(find_weights(m1), "pw")
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(conditional = c("log(bili)", "protime", "albumin"))
  )
  expect_identical(nrow(get_parameters(m1)), 3L)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("log(bili)", "protime", "albumin")
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})
