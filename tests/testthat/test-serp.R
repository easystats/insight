skip_if_not_installed("serp")

data(wine, package = "serp")
m1 <- serp::serp(
  rating ~ temp + contact,
  slope = "penalize",
  link = "logit", reverse = TRUE, tuneMethod = "user",
  lambda = 1e1, data = wine
)

test_that("model_info", {
  info <- model_info(m1)
  expect_false(info$is_poisson)
  expect_true(info$is_binomial)
  expect_false(info$is_linear)
  expect_true(info$is_ordinal)
})

test_that("loglik", {
  expect_equal(get_loglikelihood(m1), logLik(m1), ignore_attr = TRUE)
})

test_that("get_df", {
  expect_equal(get_df(m1), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "model"), 12, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "residual"), m1$rdf, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "wald"), Inf, ignore_attr = TRUE)
})


test_that("get_modelmatrix", {
  expect_identical(
    get_modelmatrix(m1),
    structure(
      c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 1, 1, 1, 1,
        0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1,
        1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0,
        0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1,
        1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1,
        0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
        0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
        1, 1, 0, 0, 1, 1, 0, 0, 1, 1
      ),
      dim = c(72L, 3L),
      dimnames = list(
        c(
          "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
          "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
          "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
          "32", "33", "34", "35", "36", "37", "38", "39", "40", "41",
          "42", "43", "44", "45", "46", "47", "48", "49", "50", "51",
          "52", "53", "54", "55", "56", "57", "58", "59", "60", "61",
          "62", "63", "64", "65", "66", "67", "68", "69", "70", "71",
          "72"
        ),
        c("(Intercept)", "tempwarm", "contactyes")
      ),
      assign = 0:2,
      contrasts = list(temp = "contr.treatment", contact = "contr.treatment")
    ),
    ignore_attr = TRUE
  )
})


test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("temp", "contact")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("temp", "contact"))
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m1))
})

test_that("get_random", {
  expect_warning(get_random(m1))
})

test_that("find_response", {
  expect_identical(find_response(m1), "rating")
})

test_that("get_response", {
  expect_identical(get_response(m1), wine$rating)
})

test_that("get_predictors", {
  expect_named(get_predictors(m1), c("temp", "contact"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
})

test_that("linkfun", {
  expect_equal(link_function(m1)(0.2), qlogis(0.2), tolerance = 1e-4)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1)), 72L)
  expect_named(get_data(m1), c("rating", "temp", "contact"))
})

test_that("get_call", {
  expect_true(inherits(get_call(m1), "call")) # nolint
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("rating ~ temp + contact")),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "rating",
      conditional = c("temp", "contact")
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("rating", "temp", "contact")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 72L)
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(conditional = c(
      "(Intercept):1", "(Intercept):2", "(Intercept):3",
      "(Intercept):4", "tempwarm:1", "tempwarm:2", "tempwarm:3", "tempwarm:4",
      "contactyes:1", "contactyes:2", "contactyes:3", "contactyes:4"
    ))
  )
  expect_identical(nrow(get_parameters(m1)), 12L)
  expect_identical(
    get_parameters(m1)$Parameter,
    c(
      "(Intercept):1", "(Intercept):2", "(Intercept):3", "(Intercept):4",
      "tempwarm:1", "tempwarm:2", "tempwarm:3", "tempwarm:4", "contactyes:1",
      "contactyes:2", "contactyes:3", "contactyes:4"
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "rating",
      conditional = c("temp", "contact")
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})

test_that("get_statistic", {
  expect_equal(
    get_statistic(m1)$Statistic,
    coef(summary(m1))[, 3],
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
