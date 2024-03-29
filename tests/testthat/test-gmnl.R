skip_if_not_installed("gmnl")
skip_if_not_installed("mlogit")
skip_if_not_installed("MASS")

data(housing, package = "MASS")

dat <<- mlogit::mlogit.data(housing, choice = "Sat", shape = "wide")
void <- capture.output(
  m1 <- gmnl::gmnl(Sat ~ Infl + Type + Cont | 1,
    data = dat,
    model = "smnl",
    R = 100
  )
)

test_that("model_info", {
  expect_false(model_info(m1)$is_ordinal)
  expect_true(model_info(m1)$is_multinomial)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("Infl", "Type", "Cont")))
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("Infl", "Type", "Cont")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_response", {
  expect_identical(find_response(m1), "Sat")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 216)
  expect_equal(colnames(get_data(m1, verbose = FALSE)), c("Sat", "Infl", "Type", "Cont"))
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
})

test_that("find_terms", {
  expect_equal(find_terms(m1), list(
    response = "Sat",
    conditional = c("Infl", "Type", "Cont", "1")
  ))
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("Sat", "Infl", "Type", "Cont", "1")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 72)
})

test_that("find_variables", {
  expect_equal(find_variables(m1), list(
    response = "Sat",
    conditional = c("Infl", "Type", "Cont")
  ))
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("Sat", "Infl", "Type", "Cont")
  )
})


test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})
