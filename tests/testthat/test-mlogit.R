skip_if_not_installed("mlogit")
skip_on_cran()

data("Fishing", package = "mlogit")
Fish <-
  mlogit::mlogit.data(Fishing,
    varying = 2:9,
    shape = "wide",
    choice = "mode"
  )

m1 <- mlogit::mlogit(mode ~ price + catch, data = Fish)
m2 <- mlogit::mlogit(mode ~ price + catch | income, data = Fish)

test_that("model_info", {
  expect_false(model_info(m1)$is_ordinal)
  expect_false(model_info(m2)$is_ordinal)
  expect_true(model_info(m1)$is_multinomial)
  expect_true(model_info(m2)$is_multinomial)
  expect_false(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("price", "catch")))
  expect_identical(find_predictors(m1, flatten = TRUE), c("price", "catch"))
  expect_null(find_predictors(m1, effects = "random"))
  expect_identical(find_predictors(m2), list(conditional = c("price", "catch", "income")))
  expect_identical(
    find_predictors(m2, flatten = TRUE),
    c("price", "catch", "income")
  )
  expect_null(find_predictors(m2, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(m1), "mode")
  expect_identical(find_response(m2), "mode")
})

test_that("get_response", {
  expect_identical(get_response(m1), as.vector(Fish$mode))
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1, verbose = FALSE)), 4728L)
  expect_identical(nrow(get_data(m2, verbose = FALSE)), 4728L)

  if (packageVersion("mlogit") <= "1.0-3.1") {
    expect_identical(
      colnames(get_data(m1, verbose = FALSE)),
      c("mode", "price", "catch", "probabilities", "linpred")
    )
    expect_identical(
      colnames(get_data(m2, verbose = FALSE)),
      c(
        "mode",
        "price",
        "catch",
        "income",
        "probabilities",
        "linpred"
      )
    )
  } else {
    expect_identical(
      colnames(get_data(m1, verbose = FALSE)),
      c("mode", "price", "catch", "idx", "probabilities", "linpred")
    )
    expect_identical(
      colnames(get_data(m2, verbose = FALSE)),
      c(
        "mode",
        "price",
        "catch",
        "income",
        "idx",
        "probabilities",
        "linpred"
      )
    )
  }
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
  expect_equal(link_inverse(m2)(0.2), plogis(0.2), tolerance = 1e-5)
})


test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_length(find_formula(m2), 1)
})

test_that("find_terms", {
  expect_identical(find_terms(m1), list(
    response = "mode",
    conditional = c("price", "catch")
  ))
  expect_identical(find_terms(m1, flatten = TRUE), c("mode", "price", "catch"))
  expect_identical(find_terms(m2), list(
    response = "mode",
    conditional = c("price", "catch", "income")
  ))
  expect_identical(
    find_terms(m2, flatten = TRUE),
    c("mode", "price", "catch", "income")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 4728L)
  expect_identical(n_obs(m2), 4728L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
  expect_false(is.null(link_function(m2)))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "z-statistic")
})
