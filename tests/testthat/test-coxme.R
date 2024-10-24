skip_if_not_installed("survival")
skip_if_not_installed("lme4")
skip_if_not_installed("nlme")
skip_if_not_installed("bdsmatrix")
skip_if_not_installed("coxme")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  {
    lung <- survival::lung
    Surv <- survival::Surv

    set.seed(1234)
    lung$inst2 <- sample.int(10, size = nrow(lung), replace = TRUE)
    lung <- subset(lung, subset = ph.ecog %in% 0:2)
    lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

    d <<- lung

    m1 <- suppressWarnings(coxme::coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), d))
    m2 <- suppressWarnings(coxme::coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst) + (1 | inst2), d))

    expect_identical(clean_names(m1), c("time", "status", "ph.ecog", "age", "inst"))
    expect_identical(clean_names(m2), c("time", "status", "ph.ecog", "age", "inst", "inst2"))

    expect_identical(clean_names(find_terms(m1)$conditional), c("ph.ecog", "age"))
    expect_identical(clean_names(find_terms(m1)$random), "inst")

    expect_identical(clean_names(find_terms(m2)$conditional), c("ph.ecog", "age"))
    expect_identical(clean_names(find_terms(m2)$random), c("inst", "inst2"))

    test_that("model_info", {
      expect_true(model_info(m1)$is_logit)
      expect_false(model_info(m1)$is_linear)
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("ph.ecog", "age")))
      expect_identical(
        find_predictors(m1, effects = "random"),
        list(random = "inst")
      )
      expect_identical(find_predictors(m2), list(conditional = c("ph.ecog", "age")))
      expect_identical(find_predictors(m2, effects = "random"), list(random = c("inst", "inst2")))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "Surv(time, status)")
      expect_identical(find_response(m1, combine = FALSE), c("time", "status"))
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(0.2), plogis(0.2), tolerance = 1e-5)
      expect_equal(link_inverse(m2)(0.2), plogis(0.2), tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_identical(nrow(get_data(m1)), 225L)
      expect_named(get_data(m1), c("time", "status", "ph.ecog", "age", "inst"))
      expect_named(get_data(m2), c("time", "status", "ph.ecog", "age", "inst", "inst2"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equal(
        find_formula(m1),
        list(
          conditional = as.formula("Surv(time, status) ~ ph.ecog + age"),
          random = as.formula("~1 | inst")
        ),
        ignore_attr = TRUE
      )

      expect_length(find_formula(m2), 2)
      expect_equal(
        find_formula(m2),
        list(
          conditional = as.formula("Surv(time, status) ~ ph.ecog + age"),
          random = list(as.formula("~1 | inst"), as.formula("~1 | inst2"))
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = "Surv(time, status)",
          conditional = c("ph.ecog", "age"),
          random = "inst"
        )
      )
      expect_identical(
        find_terms(m1, flatten = TRUE),
        c("Surv(time, status)", "ph.ecog", "age", "inst")
      )
      expect_identical(
        find_terms(m2),
        list(
          response = "Surv(time, status)",
          conditional = c("ph.ecog", "age"),
          random = c("inst", "inst2")
        )
      )
      expect_identical(
        find_terms(m2, flatten = TRUE),
        c("Surv(time, status)", "ph.ecog", "age", "inst", "inst2")
      )
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = c("time", "status"),
          conditional = c("ph.ecog", "age"),
          random = "inst"
        )
      )
      expect_identical(
        find_variables(m1, flatten = TRUE),
        c("time", "status", "ph.ecog", "age", "inst")
      )
      expect_identical(
        find_variables(m2),
        list(
          response = c("time", "status"),
          conditional = c("ph.ecog", "age"),
          random = c("inst", "inst2")
        )
      )
      expect_identical(
        find_variables(m2, flatten = TRUE),
        c("time", "status", "ph.ecog", "age", "inst", "inst2")
      )
    })

    test_that("n_obs", {
      expect_identical(n_obs(m1), 225)
      expect_identical(n_obs(m2), 225)
    })

    test_that("get_response", {
      expect_named(get_response(m1), c("time", "status"))
      expect_identical(nrow(get_response(m1)), 225L)
      expect_named(get_response(m1), c("time", "status"))
      expect_identical(nrow(get_response(m2)), 225L)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
      expect_false(is.null(link_function(m2)))
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
    })

    test_that("find_parameters", {
      expect_identical(
        find_parameters(m1),
        list(
          conditional = c("ph.ecogok", "ph.ecoglimited", "age"),
          random = "inst"
        )
      )
      expect_identical(
        find_parameters(m2),
        list(
          conditional = c("ph.ecogok", "ph.ecoglimited", "age"),
          random = c("inst", "inst2")
        )
      )
      expect_identical(nrow(get_parameters(m1)), 3L)
      expect_identical(
        get_parameters(m1)$Parameter,
        c("ph.ecogok", "ph.ecoglimited", "age")
      )

      expect_identical(nrow(get_parameters(m2)), 3L)
      expect_identical(
        get_parameters(m2)$Parameter,
        c("ph.ecogok", "ph.ecoglimited", "age")
      )

      expect_length(get_parameters(m2, effects = "random"), 2)
    })

    test_that("find_statistic", {
      expect_identical(find_statistic(m1), "z-statistic")
      expect_identical(find_statistic(m2), "z-statistic")
    })
  }
)


# get_data() works with this example
test_that("get_data works with mf", {
  withr::with_environment(
    new.env(),
    {
      data(eortc, package = "coxme")
      Surv <- survival::Surv
      d2 <<- as.data.frame(eortc)
      mcoxme <- coxme::coxme(Surv(y, uncens) ~ trt + (1 | center), data = d2)

      # environment
      out <- get_data(mcoxme)
      expect_identical(nrow(out), 2323L)
      expect_named(out, c("y", "uncens", "trt", "center"))
      # modelframe
      out <- get_data(mcoxme, source = "mf")
      expect_identical(nrow(out), 2323L)
      expect_named(out, c("y", "uncens", "center", "trt"))

      d <- as.data.frame(eortc)
      d$surv <- survival::Surv(d$y, d$uncens)
      d3 <<- d
      mcoxme <- coxme::coxme(surv ~ trt + (1 | center), data = d3)

      # environment
      out <- get_data(mcoxme)
      expect_identical(nrow(out), 2323L)
      expect_named(out, c("surv", "trt", "center"))
      # modelframe
      out <- get_data(mcoxme, source = "mf")
      expect_identical(nrow(out), 2323L)
      expect_named(out, c("y", "uncens", "center", "trt", "surv"))
    }
  )
})
