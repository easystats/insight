skip_if_not_installed("nestedLogit")
skip_if_not_installed("broom")
skip_if_not_installed("car")
skip_if_not_installed("carData")

data(Womenlf, package = "carData")

comparisons <- nestedLogit::logits(
  work = nestedLogit::dichotomy("not.work", working = c("parttime", "fulltime")),
  full = nestedLogit::dichotomy("parttime", "fulltime")
)

mnl1 <- nestedLogit::nestedLogit(
  partic ~ hincome + children,
  dichotomies = comparisons,
  data = Womenlf
)

mnl2 <- nestedLogit::nestedLogit(
  partic ~ hincome + children,
  dichotomies = comparisons,
  subset = "region == 'Ontario'",
  data = Womenlf
)

test_that("model_info", {
  expect_true(model_info(mnl1)$is_logit)
  expect_true(model_info(mnl2)$is_logit)
})

test_that("find_predictors", {
  expect_identical(find_predictors(mnl1), list(conditional = c("hincome", "children")))
  expect_identical(find_predictors(mnl2), list(conditional = c("hincome", "children")))
  expect_identical(find_predictors(mnl1, flatten = TRUE), c("hincome", "children"))
  expect_null(find_predictors(mnl1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(mnl1))
})

test_that("get_random", {
  expect_warning(get_random(mnl1))
})

test_that("find_response", {
  expect_identical(find_response(mnl1), "partic")
  expect_identical(find_response(mnl2), "partic")
})

test_that("get_response", {
  expect_equal(get_response(mnl1), Womenlf$partic, ignore_attr = TRUE)
  expect_equal(get_response(mnl2), Womenlf$partic[Womenlf$region == "Ontario"], ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_identical(colnames(get_predictors(mnl1)), c("hincome", "children"))
  expect_identical(colnames(get_predictors(mnl2)), c("hincome", "children"))
})

test_that("get_data", {
  expect_identical(nrow(get_data(mnl1)), 263L)
  expect_identical(nrow(get_data(mnl2)), 108L)
  expect_identical(colnames(get_data(mnl1)), c("partic", "hincome", "children", "region"))
  expect_identical(colnames(get_data(mnl2)), c("partic", "hincome", "children", "region"))
})

test_that("find_formula", {
  expect_length(find_formula(mnl1), 1)
  expect_equal(
    find_formula(mnl1),
    list(conditional = as.formula("partic ~ hincome + children")),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(find_variables(mnl1), list(
    response = "partic",
    conditional = c("hincome", "children")
  ))
  expect_identical(
    find_variables(mnl1, flatten = TRUE),
    c("partic", "hincome", "children")
  )
})

test_that("n_obs", {
  expect_identical(n_obs(mnl1), list(work = 263L, full = 108L))
  expect_identical(n_obs(mnl2), list(work = 108L, full = 44L))
})

test_that("linkfun", {
  expect_equal(link_function(mnl1)(0.2), -1.386294, tolerance = 1e-3)
  expect_equal(link_function(mnl2)(0.2), -1.386294, tolerance = 1e-3)
})

test_that("link_inverse", {
  expect_equal(link_inverse(mnl1)(0.2), 0.549834, tolerance = 1e-3)
  expect_equal(link_inverse(mnl2)(0.2), 0.549834, tolerance = 1e-3)
})

test_that("get_parameters", {
  expect_identical(
    find_parameters(mnl1),
    list(conditional = c("(Intercept)", "hincome", "childrenpresent"))
  )
  expect_identical(nrow(get_parameters(mnl1)), 6L)
  expect_identical(
    get_parameters(mnl1)$Parameter,
    c(
      "(Intercept)", "hincome", "childrenpresent", "(Intercept)",
      "hincome", "childrenpresent"
    )
  )
  expect_equal(
    get_parameters(mnl1)$Estimate,
    unname(c(coef(mnl1)[, 1], coef(mnl1)[, 2])),
    ignore_attr = TRUE
  )
  expect_equal(
    get_parameters(mnl1, component = "full")$Estimate,
    c(3.47777, -0.10727, -2.65146),
    tolerance = 1e-3
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(mnl1))
})

test_that("n_parameters", {
  expect_identical(n_parameters(mnl1), 3L)
})

test_that("find_algorithm", {
  expect_identical(find_algorithm(mnl1), list(algorithm = "ML"))
})

test_that("find_statistic", {
  expect_identical(find_statistic(mnl1), "z-statistic")
})

test_that("get_statistic", {
  expect_equal(
    get_statistic(mnl1)$Statistic,
    c(3.48087, -2.13894, -5.3912, 4.53361, -2.73976, -4.90035),
    tolerance = 1e-3
  )
  expect_identical(
    colnames(get_statistic(mnl1)),
    c("Parameter", "Statistic", "Response", "Component")
  )
  expect_equal(
    get_statistic(mnl1, component = "full")$Statistic,
    c(4.53361, -2.73976, -4.90035),
    tolerance = 1e-3
  )
  expect_message(get_statistic(mnl1, component = "msg"))
})

test_that("get_varcov", {
  skip_if_not_installed("sandwich")
  expect_equal(
    diag(get_varcov(mnl1)$work),
    c(`(Intercept)` = 0.14727, hincome = 0.00039, childrenpresent = 0.08542),
    tolerance = 1e-3
  )
  expect_equal(
    diag(get_varcov(mnl1, vcov = "HC3")$work),
    c(`(Intercept)` = 0.17421, hincome = 0.00051, childrenpresent = 0.08741),
    tolerance = 1e-3
  )
})
