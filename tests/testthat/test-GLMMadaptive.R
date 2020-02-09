.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest || Sys.getenv("USER") == "travis") {
  if (require("testthat") && require("insight") && require("GLMMadaptive") && require("lme4")) {
    m <- download_model("GLMMadaptive_zi_2")
    m2 <- download_model("GLMMadaptive_zi_1")

    data(cbpp)
    m3 <- GLMMadaptive::mixed_model(
      cbind(incidence, size - incidence) ~ period,
      random = ~ 1 | herd,
      data = cbpp,
      family = binomial
    )

    test_that("model_info", {
      expect_true(model_info(m)$is_zero_inflated)
      expect_true(model_info(m)$is_count)
      expect_true(model_info(m)$is_pois)
      expect_false(model_info(m)$is_negbin)
    })

    test_that("find_predictors", {
      expect_identical(
        find_predictors(m, effects = "fixed")$conditional,
        c("child", "camper")
      )
      expect_identical(
        find_predictors(m, effects = "fixed")$zero_inflated,
        c("child", "livebait")
      )
      expect_identical(
        find_predictors(m, effects = "all", flatten = TRUE),
        c("child", "camper", "persons", "livebait")
      )
      expect_identical(
        find_predictors(m, effects = "all")$zero_inflated_random,
        c("persons")
      )
      expect_identical(find_predictors(m, effects = "random")$random, "persons")
      expect_identical(
        find_predictors(
          m,
          effects = "fixed",
          component = "cond",
          flatten = TRUE
        ),
        c("child", "camper")
      )
      expect_identical(
        find_predictors(
          m,
          effects = "all",
          component = "cond",
          flatten = TRUE
        ),
        c("child", "camper", "persons")
      )
      expect_identical(
        find_predictors(m, effects = "all", component = "cond")$conditional,
        c("child", "camper")
      )

      expect_identical(
        find_predictors(
          m,
          effects = "random",
          component = "cond",
          flatten = TRUE
        ),
        "persons"
      )
      expect_identical(
        find_predictors(
          m,
          effects = "fixed",
          component = "zi",
          flatten = TRUE
        ),
        c("child", "livebait")
      )
      expect_identical(
        find_predictors(
          m,
          effects = "all",
          component = "zi",
          flatten = TRUE
        ),
        c("child", "livebait", "persons")
      )
      expect_identical(
        find_predictors(
          m,
          effects = "random",
          component = "zi",
          flatten = TRUE
        ),
        "persons"
      )
      expect_null(find_predictors(
        m,
        effects = "fixed",
        component = "dispersion",
        flatten = TRUE
      ))
      expect_null(find_predictors(
        m,
        effects = "all",
        component = "dispersion",
        flatten = TRUE
      ))
      expect_null(find_predictors(
        m,
        effects = "random",
        component = "dispersion",
        flatten = TRUE
      ))
    })

    test_that("find_response", {
      expect_identical(find_response(m), "count")
    })

    test_that("link_inverse", {
      expect_identical(link_inverse(m)(.2), exp(.2))
    })

    test_that("clean_names", {
      expect_identical(
        clean_names(m),
        c("count", "child", "camper", "persons", "livebait")
      )
    })

    test_that("find_formula", {
      expect_length(find_formula(m), 4)
      expect_identical(
        names(find_formula(m)),
        c(
          "conditional",
          "random",
          "zero_inflated",
          "zero_inflated_random"
        )
      )
    })

    test_that("find_random", {
      expect_identical(
        find_random(m),
        list(random = "persons", zero_inflated_random = "persons")
      )
      expect_identical(find_random(m, flatten = TRUE), "persons")
    })

    test_that("find_respone", {
      expect_identical(find_response(m), "count")
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m),
        list(
          response = "count",
          conditional = c("child", "camper"),
          random = "persons",
          zero_inflated = c("child", "livebait"),
          zero_inflated_random = "persons"
        )
      )
      expect_identical(
        find_terms(m, flatten = TRUE),
        c("count", "child", "camper", "persons", "livebait")
      )
    })

    test_that("get_response", {
      expect_identical(get_response(m3), cbpp[, c("incidence", "size")])
    })

    test_that("get_predictors", {
      expect_identical(
        colnames(get_predictors(m)),
        c("child", "camper", "livebait")
      )
    })

    test_that("get_random", {
      expect_identical(colnames(get_random(m)), "persons")
    })

    test_that("get_data", {
      expect_identical(
        colnames(get_data(m)),
        c("count", "child", "camper", "livebait", "persons")
      )
      expect_identical(
        colnames(get_data(m, effects = "fixed")),
        c("count", "child", "camper", "livebait")
      )
      expect_identical(colnames(get_data(m, effects = "random")), "persons")
      expect_identical(
        colnames(get_data(m, component = "zi")),
        c("count", "child", "livebait", "persons")
      )
      expect_identical(colnames(get_data(
        m,
        component = "zi", effects = "fixed"
      )), c("count", "child", "livebait"))
      expect_identical(colnames(get_data(
        m,
        component = "zi", effects = "random"
      )), "persons")
      expect_identical(
        colnames(get_data(m, component = "cond")),
        c("count", "child", "camper", "persons")
      )
      expect_identical(colnames(get_data(
        m,
        component = "cond", effects = "fixed"
      )), c("count", "child", "camper"))
      expect_identical(colnames(get_data(
        m,
        component = "cond", effects = "random"
      )), "persons")
      expect_identical(colnames(get_data(m, component = "dispersion")), "count")
      expect_null(get_data(m, component = "dispersion", effects = "random"))
      expect_identical(
        colnames(get_data(m3)),
        c("incidence", "size", "period", "herd")
      )
    })

    test_that("find_parameter", {
      expect_equal(
        find_parameters(m),
        list(
          conditional = c("(Intercept)", "child", "camper1"),
          random = "(Intercept)",
          zero_inflated = c("(Intercept)", "child", "livebait1"),
          zero_inflated_random = "zi_(Intercept)"
        )
      )
      expect_equal(
        find_parameters(m2),
        list(
          conditional = c("(Intercept)", "child", "camper1"),
          random = "(Intercept)",
          zero_inflated = c("(Intercept)", "child", "livebait1")
        )
      )
      expect_equal(
        find_parameters(m3),
        list(
          conditional = c("(Intercept)", "period2", "period3", "period4"),
          random = "(Intercept)"
        )
      )

      expect_equal(nrow(get_parameters(m)), 6)
      expect_equal(
        get_parameters(m, effects = "random"),
        list(
          random = c(-1.0715496, 1.4083630, 1.9129880, 0.2007521),
          zero_inflated_random = c(-0.1676294, 0.5502481, 1.2592406, 0.9336591)
        ),
        tolerance = 1e-5
      )
      expect_equal(nrow(get_parameters(m2)), 6)
      expect_equal(get_parameters(m2, effects = "random"),
        list(random = c(
          -1.3262364, -0.2048055, 1.3852572, 0.5282277
        )),
        tolerance = 1e-5
      )
      expect_equal(
        get_parameters(m3)$Component,
        c(
          "conditional",
          "conditional",
          "conditional",
          "conditional"
        )
      )
      expect_error(get_parameters(m3, "zi"))
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m)))
      expect_false(is.null(link_function(m2)))
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m))
      expect_false(is_multivariate(m2))
    })

    test_that("find_algorithm", {
      expect_equal(
        find_algorithm(m),
        list(algorithm = "quasi-Newton", optimizer = "optim")
      )
    })
  }

  # these run successfully for devtools::test_file() locally but fail on Travis
  # not sure what's going on

  # test_that("find_statistic", {
  #   expect_identical(find_statistic(m1), "z-statistic")
  #   expect_identical(find_statistic(m2), "z-statistic")
  #   expect_identical(find_statistic(m3), "z-statistic")
  # })
}
