if (require("testthat") && require("insight") && require("aod")) {
  context("insight, betabin")

  data(dja)
  m1 <- betabin(cbind(y, n - y) ~ group * trisk, ~village, data = dja)

  test_that("model_info", {
    expect_true(model_info(m1)$is_binomial)
    expect_true(model_info(m1)$is_betabinomial)
    expect_true(model_info(m1)$is_mixed)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("group", "trisk")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("group", "trisk"))
    expect_identical(find_predictors(m1, effects = "random"), list(random = "village"))
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(
        conditional = c("group", "trisk"),
        random = "village"
      )
    )
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = "village"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), dja[, "village", drop = FALSE])
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "cbind(y, n - y)")
    expect_identical(find_response(m1, combine = FALSE), c("y", "n"))
  })

  test_that("get_response", {
    expect_equal(get_response(m1), dja[, c("y", "n")])
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), c("group", "trisk"))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
  })

  test_that("link_function", {
    expect_equal(link_function(m1)(.2), qlogis(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 75)
    expect_equal(colnames(get_data(m1)), c("y", "n", "group", "trisk", "village"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("cbind(y, n - y) ~ group * trisk"),
        random = as.formula("~village")
      )
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(response = c("y", "n"), conditional = c("group", "trisk"), random = "village"))
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "n", "group", "trisk", "village"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 75)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "groupTREAT", "trisk", "groupTREAT:trisk"),
        random = c(
          "phi.villageBAK", "phi.villageBAM", "phi.villageBAN",
          "phi.villageBIJ", "phi.villageBOU", "phi.villageBYD", "phi.villageDEM",
          "phi.villageDIA", "phi.villageHAM", "phi.villageLAM", "phi.villageLAY",
          "phi.villageMAF", "phi.villageMAH", "phi.villageMAK", "phi.villageMED",
          "phi.villageNAB", "phi.villageSAG", "phi.villageSAM", "phi.villageSOU"
        )
      )
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "groupTREAT", "trisk", "groupTREAT:trisk"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "cbind(y, n - y)",
        conditional = c("group", "trisk"),
        random = "village"
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(algorithm = "ML"))
  })
}
