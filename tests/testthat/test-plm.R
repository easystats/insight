skip_if_not_installed("plm")

data(Crime, package = "plm")
m1 <- suppressWarnings(
  plm::plm(
    lcrmrte ~ lprbarr + factor(year) | . - lprbarr + lmix,
    data = Crime,
    model = "random"
  )
)

set.seed(123)
data("Produc", package = "plm")

m2 <- suppressWarnings(plm::plm(
  formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  data = Produc,
  index = c("state", "year")
))

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(
      conditional = c("lprbarr", "year"),
      instruments = c("lprbarr", "lmix")
    )
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("lprbarr", "year", "lmix")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m1))
})

test_that("get_random", {
  expect_warning(get_random(m1))
})

test_that("find_response", {
  expect_identical(find_response(m1), "lcrmrte")
})

test_that("get_response", {
  expect_equal(get_response(m1), Crime$lcrmrte, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_named(get_predictors(m1), c("lprbarr", "year", "lmix"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1)), 630L)
  expect_named(get_data(m1), c("lcrmrte", "lprbarr", "year", "lmix"))

  expect_identical(nrow(get_data(m2)), 816L)
  expect_named(get_data(m2), c("gsp", "pcap", "pc", "emp", "unemp", "state", "year"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("lcrmrte ~ lprbarr + factor(year)"),
      instruments = as.formula("~-lprbarr + lmix")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "lcrmrte",
      conditional = c("lprbarr", "year"),
      instruments = c("lprbarr", "lmix")
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("lcrmrte", "lprbarr", "year", "lmix")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 630L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c(
        "(Intercept)",
        "lprbarr",
        "factor(year)82",
        "factor(year)83",
        "factor(year)84",
        "factor(year)85",
        "factor(year)86",
        "factor(year)87"
      )
    )
  )
  expect_identical(nrow(get_parameters(m1)), 8L)
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
})

test_that("get_varcov, pgmm", {
  data("EmplUK", package = "plm")
  mar <- suppressWarnings(plm::pgmm(
    log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) + log(capital) +
      lag(log(capital), 2) + log(output) + lag(log(output), 2) | lag(log(emp), 2:99),
    data = EmplUK, effect = "twoways", model = "twosteps"
  ))
  out1 <- sqrt(diag(insight::get_varcov(mar, vcov = "HC1", component = "all")))
  validate1 <- sqrt(diag(plm::vcovHC(mar)))
  expect_equal(out1, validate1, tolerance = 1e-4, ignore_attr = TRUE)
})
