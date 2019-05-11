.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {

  if (require("testthat") && require("insight") && require("gbm")) {
    context("insight, gbm")

    set.seed(102)  # for reproducibility
    m1 <- gbm(mpg ~ gear + cyl + wt, data = mtcars,
              var.monotone = c(0, 0, 0),
              distribution = "gaussian", shrinkage = 0.1,
              interaction.depth = 1, bag.fraction = 0.5,
              train.fraction = 0.5,
              n.minobsinnode = 1, cv.folds = 3, keep.data = TRUE,
              verbose = FALSE, n.cores = 1)

    test_that("model_info", {
      expect_true(model_info(m1)$is_linear)
      expect_false(model_info(m1)$is_binomial)
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("gear", "cyl", "wt")))
      expect_identical(find_predictors(m1, flatten = TRUE), c("gear", "cyl", "wt"))
      expect_null(find_predictors(m1, effects = "random"))
    })

    test_that("find_random", {
      expect_null(find_random(m1))
    })

    test_that("get_random", {
      expect_warning(get_random(m1))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "mpg")
    })

    test_that("get_response", {
      expect_equal(get_response(m1), mtcars$mpg)
    })

    test_that("get_predictors", {
      expect_equal(colnames(get_predictors(m1)), c("gear", "cyl", "wt"))
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 32)
      expect_equal(colnames(get_data(m1)), c("mpg", "gear", "cyl", "wt"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 1)
      expect_equal(
        find_formula(m1),
        list(conditional = as.formula("mpg ~ gear + cyl + wt"))
      )
    })

    test_that("find_terms", {
      expect_equal(find_terms(m1), list(response = "mpg", conditional = c("gear", "cyl", "wt")))
      expect_equal(find_terms(m1, flatten = TRUE), c("mpg", "gear", "cyl", "wt"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 32)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("find_parameters", {

      skip_on_travis()

      expect_equal(
        find_parameters(m1),
        list(
          conditional = c("wt", "gear", "cyl")
        )
      )
      expect_equal(nrow(get_parameters(m1)), 3)
      expect_equal(get_parameters(m1)$parameter, c("wt", "gear", "cyl"))
    })

    test_that("find_variables", {
      expect_equal(
        find_variables(m1),
        list(
          response = "mpg",
          conditional = c("gear", "cyl", "wt")
        )
      )
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
    })

    test_that("find_algorithm", {
      expect_warning(expect_null(find_algorithm(m1)))
    })
  }
}