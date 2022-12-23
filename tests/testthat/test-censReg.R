if (requiet("testthat") && requiet("insight") && requiet("censReg") && requiet("AER")) {
  data("Affairs", package = "AER")
  m1 <- censReg(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = Affairs
  )

  test_that("model_info", {
    expect_false(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_censored)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(
      conditional = c(
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating"
      )
    ))
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c(
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating"
      )
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_random", {
    expect_null(find_random(m1))
  })

  test_that("get_random", {
    expect_warning(get_random(m1))
  })

  test_that("get_varcov", {
    expect_equal(vcov(m1), get_varcov(m1), tolerance = 1e-3)
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "affairs")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), Affairs$affairs)
  })

  test_that("get_predictors", {
    expect_equal(
      colnames(get_predictors(m1)),
      c(
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating"
      )
    )
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 601)
    expect_equal(
      colnames(get_data(m1)),
      c(
        "affairs",
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating"
      )
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula(
          "affairs ~ age + yearsmarried + religiousness + occupation + rating"
        )
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "affairs",
        conditional = c(
          "age",
          "yearsmarried",
          "religiousness",
          "occupation",
          "rating"
        )
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c(
        "affairs",
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating"
      )
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 601)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "(Intercept)",
          "age",
          "yearsmarried",
          "religiousness",
          "occupation",
          "rating",
          "logSigma"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "(Intercept)",
        "age",
        "yearsmarried",
        "religiousness",
        "occupation",
        "rating",
        "logSigma"
      )
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "affairs",
        conditional = c(
          "age",
          "yearsmarried",
          "religiousness",
          "occupation",
          "rating"
        )
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
