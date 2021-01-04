.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest || Sys.getenv("USER") == "travis") {
  if (require("testthat") && require("insight") && require("glmmTMB")) {
    m1 <- download_model("glmmTMB_spatial_1")

    test_that("find_weights", {
      expect_null(find_weights(m1))
    })

    test_that("model_info", {
      expect_true(model_info(m1)$is_linear)
    })

    test_that("clean_names", {
      expect_identical(clean_names(m1), c("calcium", "elevation", "region", "pos", "ID"))
    })

    test_that("find_predictors", {
      expect_identical(
        find_predictors(m1, effects = "all"),
        list(conditional = c("elevation", "region"), random = c("pos", "ID"))
      )
      expect_identical(
        find_predictors(m1, effects = "all", flatten = TRUE),
        c("elevation", "region", "pos", "ID")
      )

      expect_identical(
        find_predictors(m1, effects = "random"),
        list(random = "ID")
      )
      expect_identical(find_predictors(m1, effects = "random", flatten = TRUE), "ID")
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "calcium")
    })

    test_that("link_inverse", {
      expect_identical(link_inverse(m1)(.2), .2)
    })

    test_that("get_data", {
      expect_equal(
        colnames(get_data(m1)),
        c("calcium", "elevation", "region", "pos", "ID")
      )
      expect_equal(
        colnames(get_data(m1, effects = "all")),
        c("calcium", "elevation", "region", "pos", "ID")
      )
    })

    test_that("find_predictors", {
      expect_identical(
        find_predictors(m1, effects = "fixed", component = "conditional"),
        list(conditional = c("elevation", "region"))
      )
      expect_identical(
        find_predictors(m1),
        list(conditional = c("elevation", "region"))
      )
      expect_identical(
        find_predictors(m1, effects = "all"),
        list(
          conditional = c("elevation", "region"),
          random = c("pos", "ID")
        )
      )
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equivalent(
        find_formula(m1),
        list(
          conditional = as.formula("calcium ~ elevation + region"),
          random = as.formula("~pos + 0 | ID")
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_random", {
      expect_identical(
        find_random(m1),
        list(random = "ID")
      )
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = "calcium",
          conditional = c("elevation", "region"),
          random = c("pos", "ID")
        )
      )
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = "calcium",
          conditional = c("elevation", "region"),
          random = c("pos", "ID")
        )
      )
    })


    test_that("get_predictors", {
      expect_identical(
        colnames(get_predictors(m1)),
        c("elevation", "region")
      )
    })

    test_that("get_random", {
      expect_identical(colnames(get_random(m1)), c("pos", "ID"))
    })

    test_that("get_data", {
      expect_identical(
        colnames(get_data(m1)),
        c("calcium", "elevation", "region", "pos", "ID")
      )
    })


    test_that("get_paramaters", {
      expect_equal(nrow(get_parameters(m1)), 5)
      expect_equal(
        get_parameters(m1)$Parameter,
        c("(Intercept)", "elevation", "region2", "region3", "(Intercept)")
      )
    })

    test_that("find_random_slopes", {
      skip_on_cran()
      skip_on_travis()

      expect_equal(
        find_random_slopes(m1),
        list(random = "pos")
      )
    })
  }
}
