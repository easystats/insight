if (requiet("testthat") &&
  requiet("insight") &&
  requiet("ordinal")) {
  data(wine, package = "ordinal")
  m1 <- clm(rating ~ temp * contact, data = wine)

  test_that("model_info", {
    expect_true(model_info(m1)$is_ordinal)
    expect_false(model_info(m1)$is_multinomial)
    expect_false(model_info(m1)$is_linear)
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
    expect_equal(get_response(m1), wine$rating)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("temp", "contact"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 72)
    expect_equal(colnames(get_data(m1)), c("rating", "temp", "contact"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 1)
    expect_equal(
      find_formula(m1),
      list(conditional = as.formula("rating ~ temp * contact")),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "rating",
      conditional = c("temp", "contact")
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("rating", "temp", "contact")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 72)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "1|2",
          "2|3",
          "3|4",
          "4|5",
          "tempwarm",
          "contactyes",
          "tempwarm:contactyes"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 7)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "1|2",
        "2|3",
        "3|4",
        "4|5",
        "tempwarm",
        "contactyes",
        "tempwarm:contactyes"
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })

  test_that("get_predicted", {
    nd <- wine
    nd$rating <- NULL
    x <- as.data.frame(get_predicted(m1))
    y <- as.data.frame(get_predicted(m1, predict = NULL, type = "prob"))
    z <- predict(m1, type = "prob", newdata = nd, se.fit = TRUE)
    expect_true(all(c("Row", "Response", "Predicted", "SE") %in% colnames(x)))
    expect_equal(x, y)
    for (i in 1:5) {
      expect_equal(x$Predicted[x$Response == i], unname(z$fit[, i]), ignore_attr = FALSE)
      expect_equal(x$SE[x$Response == i], unname(z$se.fit[, i]), ignore_attr = FALSE)
    }
    x <- as.data.frame(get_predicted(m1, predict = "classification"))
    y <- as.data.frame(get_predicted(m1, predict = NULL, type = "class"))
    z <- predict(m1, type = "class", newdata = nd)
    expect_equal(x, y)
    expect_equal(as.character(x$Predicted), as.character(z$fit), ignore_attr = FALSE)

    # we use a hack to handle in-formula factors
    tmp <- wine
    tmp$rating <- as.numeric(tmp$rating)
    tmp <- clm(factor(rating) ~ temp * contact, data = tmp)
    expect_s3_class(get_predicted(tmp), "get_predicted")

  })

}
