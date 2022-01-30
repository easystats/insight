skip_if_not(isTRUE(Sys.getenv("RunAllinsightTests") == "yes"))
requiet("glmmTMB")
requiet("biglm")

test_that("bigglm()", {
  data(Salamanders)
  Salamanders$cover <- abs(Salamanders$cover)

  m1 <- bigglm(count ~ mined + log(cover) + sample,
               family = poisson(),
               data = Salamanders)

  # model_info
  expect_true(model_info(m1)$is_poisson)
  expect_true(model_info(m1)$is_count)
  expect_false(model_info(m1)$is_negbin)
  expect_false(model_info(m1)$is_binomial)
  expect_false(model_info(m1)$is_linear)

  # find_predictors
  expect_identical(find_predictors(m1), list(conditional = c("mined", "cover", "sample")))
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("mined", "cover", "sample"))
  expect_null(find_predictors(m1, effects = "random"))

  # find_random
  expect_null(find_random(m1))

  # get_random
  expect_warning(get_random(m1))

  # find_response
  expect_identical(find_response(m1), "count")

  # get_response
  expect_equal(get_response(m1), Salamanders$count)

  # get_predictors
  expect_equal(
    colnames(get_predictors(m1)),
    c("mined", "cover", "sample"))

  # link_inverse
  expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)

  # get_data
  expect_equal(nrow(get_data(m1)), 644)
  expect_equal(
    colnames(get_data(m1)),
    c("site",
      "mined",
      "cover",
      "sample",
      "DOP",
      "Wtemp",
      "DOY",
      "spp",
      "count"))

  # find_formula
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("count ~ mined + log(cover) + sample")),
    ignore_attr = TRUE)

  # find_variables
  expect_equal(
    find_variables(m1),
    list(
      response = "count",
      conditional = c("mined", "cover", "sample")))
  expect_equal(
    find_variables(m1, flatten = TRUE),
    c("count", "mined", "cover", "sample"))

  # n_obs
  expect_equal(n_obs(m1), 644)

  # linkfun
  expect_false(is.null(link_function(m1)))

  # find_parameters
  expect_equal(
    find_parameters(m1),
    list(conditional = c("(Intercept)", "minedno", "log(cover)", "sample")))
  expect_equal(nrow(get_parameters(m1)), 4)
  expect_equal(
    get_parameters(m1)$Parameter,
    c("(Intercept)", "minedno", "log(cover)", "sample"))

  # is_multivariate
  expect_false(is_multivariate(m1))

  # find_terms
  expect_equal(
    find_terms(m1),
    list(
      response = "count",
      conditional = c("mined", "log(cover)", "sample")))

  # find_algorithm
  expect_equal(find_algorithm(m1), list(algorithm = "ML"))

  # find_statistic
  expect_identical(find_statistic(m1), "z-statistic")
})
