if (requiet("testthat") &&
  requiet("insight") &&
  requiet("nnet") &&
  requiet("MASS")) {
  data("birthwt")
  void <- capture.output({
    m1 <- nnet::multinom(low ~ age + lwt + race + smoke, data = birthwt)
  })




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

  test_that("get_predicted", {
    void <- capture.output({
      # binary outcome
      m1 <- nnet::multinom(low ~ age + lwt + race + smoke, data = birthwt)
      # multinomial outcome
      m2 <- nnet::multinom(ftv ~ age + lwt + race + smoke, data = birthwt)
    })

    # binary outcomes produces an atomic vector
    x <- get_predicted(m1, predict = "classification")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))
    expect_true(all(levels(x) %in% c("0", "1")))
    x <- get_predicted(m1, predict = "expectation")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))
    x <- get_predicted(m1, predict = NULL, type = "class")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))
    expect_true(all(levels(x) %in% c("0", "1")))
    x <- get_predicted(m1, predict = NULL, type = "probs")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))

    # multinomial outcomes depends on predict type
    x <- get_predicted(m2, predict = "classification")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))
    expect_true(all(levels(x) %in% as.character(0:6)))
    x <- get_predicted(m2, predict = "expectation")
    expect_s3_class(x, "data.frame")
    expect_true(all(c("Row", "Response", "Predicted") %in% colnames(x)))
    x <- get_predicted(m2, predict = NULL, type = "class")
    expect_true(is.atomic(x) && !is.null(x) && is.null(dim(x)))
    expect_true(all(levels(x) %in% as.character(0:6)))
    x <- get_predicted(m2, predict = NULL, type = "probs")
    expect_s3_class(x, "data.frame")
    expect_true(all(c("Row", "Response", "Predicted") %in% colnames(x)))
  })
}
