if (require("testthat") &&
  require("insight") &&
  require("feisr")) {
  context("insight, feisr")

  data(mwp)
  m1 <-
    feis(
      lnw ~ marry + enrol + as.factor(yeargr) |
        exp + I(exp^2),
      data = mwp,
      id = "id",
      robust = TRUE
    )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(
      conditional = c("marry", "enrol", "yeargr"),
      slopes = "exp"
    ))
    expect_identical(find_predictors(m1, effects = "random"), list(random = "id"))
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("marry", "enrol", "yeargr", "exp", "id")
    )
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = "id"))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m1)), "id")
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "lnw")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), mwp$lnw)
  })

  test_that("get_predictors", {
    expect_equal(
      colnames(get_predictors(m1)),
      c("marry", "enrol", "yeargr", "exp")
    )
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 3100)
    expect_equal(
      colnames(get_data(m1)),
      c("lnw", "marry", "enrol", "yeargr", "exp", "id")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 3)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("lnw ~ marry + enrol + as.factor(yeargr)"),
        slopes = as.formula("~exp + I(exp^2)"),
        random = as.formula("~id")
      )
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "lnw",
        conditional = c("marry", "enrol", "as.factor(yeargr)"),
        slopes = c("exp", "I(exp^2)"),
        random = "id"
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c(
        "lnw",
        "marry",
        "enrol",
        "as.factor(yeargr)",
        "exp",
        "I(exp^2)",
        "id"
      )
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "lnw",
        conditional = c("marry", "enrol", "yeargr"),
        slopes = "exp",
        random = "id"
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("lnw", "marry", "enrol", "yeargr", "exp", "id")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 3100)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "marry",
          "enrol",
          "as.factor(yeargr)2",
          "as.factor(yeargr)3",
          "as.factor(yeargr)4",
          "as.factor(yeargr)5"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 6)
    expect_equal(
      get_parameters(m1)$parameter,
      c(
        "marry",
        "enrol",
        "as.factor(yeargr)2",
        "as.factor(yeargr)3",
        "as.factor(yeargr)4",
        "as.factor(yeargr)5"
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
