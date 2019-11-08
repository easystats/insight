.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest || Sys.getenv("USER") == "travis") {
  unloadNamespace("gam")
  if (require("testthat") && require("insight") && require("mgcv")) {
    context("insight, model_info")

    set.seed(0)
    dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
    m1 <-
      gamm(
        y ~ s(x0) + s(x1) + s(x2),
        family = poisson,
        data = dat,
        random = list(fac = ~1)
      )

    test_that("model_info", {
      expect_true(model_info(m1)$is_poisson)
    })

    test_that("clean_names", {
      expect_equal(clean_names(m1), c("y", "x0", "x1", "x2"))
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2")))
      expect_identical(find_predictors(m1, flatten = TRUE), c("x0", "x1", "x2"))
      expect_null(find_predictors(m1, effects = "random"))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "y")
    })

    test_that("get_response", {
      expect_equal(get_response(m1), dat$y)
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 200)
      expect_equal(colnames(get_data(m1)), c("y", "x0", "x1", "x2", "fac", "g", "g.0", "g.1", "y.0", "Xr.V1", "Xr.V2", "Xr.V3", "Xr.V4", "Xr.V5", "Xr.V6", "Xr.V7", "Xr.V8", "Xr.0.V1", "Xr.0.V2", "Xr.0.V3", "Xr.0.V4", "Xr.0.V5", "Xr.0.V6", "Xr.0.V7", "Xr.0.V8", "Xr.1.V1", "Xr.1.V2", "Xr.1.V3", "Xr.1.V4", "Xr.1.V5", "Xr.1.V6", "Xr.1.V7", "Xr.1.V8", "X.(Intercept)", "X.s(x0)Fx1", "X.s(x1)Fx1", "X.s(x2)Fx1"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 1)
      expect_equal(
        find_formula(m1),
        list(conditional = as.formula("y ~ s(x0) + s(x1) + s(x2)"))
      )
    })

    test_that("find_terms", {
      expect_equal(find_terms(m1), list(response = "y", conditional = c("s(x0)", "s(x1)", "s(x2)")))
      expect_equal(find_terms(m1, flatten = TRUE), c("y", "s(x0)", "s(x1)", "s(x2)"))
    })

    test_that("find_variables", {
      expect_equal(find_variables(m1), list(response = "y", conditional = c("x0", "x1", "x2")))
      expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 200)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("find_parameters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = "(Intercept)",
          smooth_terms = c("s(x0)", "s(x1)", "s(x2)")
        )
      )
      expect_equal(nrow(get_parameters(m1)), 4)
      expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)"))
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
    })
  }
}