if (requiet("testthat") &&
  requiet("insight") &&
  requiet("nnet") &&
  requiet("MASS")) {
  data("birthwt")
  m1 <- multinom(low ~ age + lwt + race + smoke, data = birthwt)

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("n_parameters", {
    expect_equal(n_parameters(m1), 5)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("age", "lwt", "race", "smoke")))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("age", "lwt", "race", "smoke")
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "low")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 189)
    expect_equal(
      colnames(get_data(m1)),
      c("low", "age", "lwt", "race", "smoke")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("low ~ age + lwt + race + smoke")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "low",
      conditional = c("age", "lwt", "race", "smoke")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("low", "age", "lwt", "race", "smoke")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 189)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(conditional = c(
        "(Intercept)", "age", "lwt", "race", "smoke"
      ))
    )
    expect_equal(nrow(get_parameters(m1)), 5)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "age", "lwt", "race", "smoke")
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
