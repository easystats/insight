.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {

  if (require("testthat") && require("insight") && require("survey")) {
    context("insight, svyolr")

    data(api)
    dat <- apiclus1
    dat$mealcat <- cut(dat$meals, c(0, 25, 50, 75, 100))

    dclus1 <- svydesign(
      id =  ~ dnum,
      weights =  ~ pw,
      data = dat,
      fpc =  ~ fpc
    )

    m1 <- svyolr(mealcat ~ avg.ed + mobility + stype, design = dclus1)

    test_that("model_info", {
      expect_true(model_info(m1)$is_ordinal)
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("avg.ed", "mobility", "stype")))
      expect_null(find_predictors(m1, effects = "random"))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "mealcat")
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
    })

    test_that("link_function", {
      expect_equal(link_function(m1)(.2), qlogis(.2), tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 157)
      expect_equal(colnames(get_data(m1)), c("mealcat", "avg.ed", "mobility", "stype", "(weights)"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 1)
      expect_equal(
        find_formula(m1),
        list(conditional = as.formula("mealcat ~ avg.ed + mobility + stype"))
      )
    })

    test_that("find_terms", {
      expect_equal(find_terms(m1), list(response = "mealcat", conditional = c("avg.ed", "mobility", "stype")))
      expect_equal(find_terms(m1, flatten = TRUE), c("mealcat", "avg.ed", "mobility", "stype"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 157)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("find_parameters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = c("avg.ed", "mobility", "stypeH", "stypeM", "(0,25]|(25,50]", "(25,50]|(50,75]", "(50,75]|(75,100]")
        )
      )
      expect_equal(nrow(get_parameters(m1)), 7)
      expect_equal(get_parameters(m1)$parameter, c("avg.ed", "mobility", "stypeH", "stypeM", "(0,25]|(25,50]", "(25,50]|(50,75]", "(50,75]|(75,100]"))
    })
  }
}
