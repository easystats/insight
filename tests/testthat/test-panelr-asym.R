skip_if_not_installed("panelr")
skip_if_not_installed("clubSandwich")
skip_if_not_installed("car")

data("teen_poverty", package = "panelr")
teen <- panelr::long_panel(teen_poverty, begin = 1, end = 5)
m1 <- panelr::asym(hours ~ lag(pov) + spouse, data = teen, use.wave = TRUE)

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("id_mixed", {
  expect_false(is_mixed_model(m1))
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = c("pov", "spouse", "wave"))
  )
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("pov", "spouse", "wave")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m1))
})

test_that("find_response", {
  expect_identical(find_response(m1), "hours")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("hours ~ lag(pov) + spouse + wave")),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "hours",
      conditional = c("pov", "spouse", "wave")
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("hours", "pov", "spouse", "wave")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(m1), 3453L)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c("(Intercept)", "+lag(pov)", "-lag(pov)", "+spouse", "-spouse", "wave")
    )
  )
  expect_identical(nrow(get_parameters(m1)), 6L)
})


test_that("get_parameters", {
  expect_equal(
    get_parameters(m1),
    data.frame(
      Parameter = c(
        "(Intercept)", "+lag(pov)", "-lag(pov)",
        "+spouse", "-spouse", "wave"
      ),
      Estimate = c(
        5.07629091766534,  -0.695146196282601, 2.74491742707041,
        -2.99740859292258, -0.399625620609947, -0.812955475294268
      ),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("get_statistic", {
  expect_equal(
    get_statistic(m1),
    data.frame(
      Parameter = c(
        "(Intercept)", "+lag(pov)", "-lag(pov)",
        "+spouse", "-spouse", "wave"
      ),
      Statistic = c(
        3.72593191668147, -0.946399537913606, 3.48400992278831,
        -2.27338697388243, -0.160677061930687, -2.3766486580145
      ),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "hours",
      conditional = c("lag(pov)", "spouse", "wave")
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})

test_that("get_vcov", {
  skip_on_cran()
  v <- get_varcov(m1)
  expect_identical(dim(v), c(6L, 6L))
  expect_identical(
    colnames(v),
    c("(Intercept)", "+lag(pov)", "-lag(pov)", "+spouse", "-spouse", "wave")
  )
})
