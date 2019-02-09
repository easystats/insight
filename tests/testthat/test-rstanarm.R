.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (suppressWarnings(
    require("testthat") &&
      require("insight") &&
      require("rstanarm")
  )) {
    context("insight, rstanarm")

    m1 <- stan_glmer(
      cbind(incidence, size - incidence) ~ size + period + (1|herd),
      data = lme4::cbpp, family = binomial, QR = TRUE,
      chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
    )

    test_that("clean_names", {
      expect_identical(clean_names(m1), c("incidence", "size", "period", "herd"))
    })


    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("size", "period")))
      expect_identical(find_predictors(m1, flatten = TRUE), c("size", "period"))
      expect_identical(find_predictors(m1, effects = "all", component = "all"), list(conditional = c("size", "period"), random = "herd"))
      expect_identical(find_predictors(m1, effects = "all", component = "all", flatten = TRUE), c("size", "period", "herd"))
    })

    test_that("find_response", {
      expect_equal(find_response(m1, combine = TRUE), "cbind(incidence, size - incidence)")
      expect_equal(find_response(m1, combine = FALSE), c("incidence", "size"))
    })

    test_that("get_response", {
      expect_equal(nrow(get_response(m1)), 56)
      expect_equal(colnames(get_response(m1)), c("incidence", "size"))
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period"),
          random = "herd"
      ))
      expect_identical(
        find_terms(m1, effects = "fixed"),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period")
        ))
      expect_null(find_terms(m1, component = "zi"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 56)
    })

    test_that("find_paramaters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = c("(Intercept)", "size", "period2", "period3", "period4"),
          random = sprintf("b[(Intercept) herd:%i]", 1:15)
        )
      )
    })

    test_that("find_paramaters", {
      expect_equal(
        colnames(get_parameters(m1)),
        c("(Intercept)", "size", "period2", "period3", "period4")
      )
      expect_equal(
        colnames(get_parameters(m1, effects = "all")),
        c("(Intercept)", "size", "period2", "period3", "period4", sprintf("b[(Intercept) herd:%i]", 1:15))
      )
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 56)
      expect_equal(
        colnames(get_data(m1)),
        c("cbind(incidence, size - incidence)", "size", "period", "herd", "incidence")
      )
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equal(
        find_formula(m1),
        list(
          conditional = as.formula("cbind(incidence, size - incidence) ~ size + period"),
          random = as.formula("~1 | herd")
        )
      )
    })

  }
}
