.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"
# if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
      require("insight") &&
      require("rstanarm")
  )) {
    context("insight, rstanarm")

    m1 <- insight::download_model("stanreg_merMod_5")
    m2 <- insight::download_model("stanreg_glm_6")
    m3 <- insight::download_model("stanreg_glm_1")

    test_that("get_priors", {
      expect_equal(colnames(get_priors(m1)), c("parameter", "distribution", "location", "scale"))
      expect_equal(colnames(get_priors(m2)), c("parameter", "distribution", "location", "scale", "adjusted_scale"))
      expect_equal(get_priors(m1)$scale, c(10.0, 2.5, 2.5, 2.5, 2.5), tolerance = 1e-3)
      expect_equal(get_priors(m2)$adjusted_scale, c(4.3586628, 0.4119705, 0.5360283, 0.6172700, 1.0896657, 1.0896657), tolerance = 1e-3)
      expect_equal(get_priors(m3)$adjusted_scale, c(NA, 2.555042), tolerance = 1e-3)
    })


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

    test_that("find_random", {
      expect_equal(find_random(m1), list(random = "herd"))
    })

    test_that("get_random", {
      expect_equal(get_random(m1), lme4::cbpp[, "herd", drop = FALSE])
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period"),
          random = "herd"
        )
      )
      expect_identical(
        find_terms(m1, effects = "fixed"),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period")
        )
      )
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
      expect_equal(find_parameters(m1, flatten = TRUE), c("(Intercept)", "size", "period2", "period3", "period4", sprintf("b[(Intercept) herd:%i]", 1:15)))
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
      expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-4)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 56)
      expect_equal(
        colnames(get_data(m1)),
        c("incidence", "size", "period", "herd")
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

    test_that("get_variance", {
      expect_equal(get_variance(m1), list(
        var.fixed = 0.3710157,
        var.random = 0.6113405,
        var.residual = 3.289868,
        var.distribution = 3.289868,
        var.dispersion = 0,
        var.intercept = c(herd = 0.6113405)
      ),
      tolerance = 1e-4)

      expect_equal(get_variance_fixed(m1), c(var.fixed = 0.3710157), tolerance = 1e-4)
      expect_equal(get_variance_random(m1), c(var.random = 0.6113405), tolerance = 1e-4)
      expect_equal(get_variance_residual(m1), c(var.residual = 3.289868), tolerance = 1e-4)
      expect_equal(get_variance_distribution(m1), c(var.distribution = 3.289868), tolerance = 1e-4)
      expect_equal(get_variance_dispersion(m1), c(var.dispersion = 0), tolerance = 1e-4)
    })

    test_that("find_algorithm", {
      expect_equal(find_algorithm(m1), list(
        algorithm = "sampling",
        chains = 2,
        iterations = 500,
        warmup = 250
      ))
    })
  }

# }
