.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {
  if (require("testthat") &&
    require("insight") && require("lme4") && require("robustlmm")) {
    data(sleepstudy)

    set.seed(123)
    sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
    sleepstudy$mysubgrp <- NA
    for (i in 1:5) {
      filter_group <- sleepstudy$mygrp == i
      sleepstudy$mysubgrp[filter_group] <-
        sample(1:30,
          size = sum(filter_group),
          replace = TRUE
        )
    }

    m1 <- rlmer(
      Reaction ~ Days + (Days | Subject),
      data = sleepstudy,
      rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
      rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10)
    )

    m2 <-
      rlmer(
        Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
        data = sleepstudy,
        rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
        rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10)
      )

    test_that("model_info", {
      expect_true(model_info(m1)$is_linear)
      expect_true(model_info(m2)$is_linear)
    })

    test_that("find_predictors", {
      expect_equal(
        find_predictors(m1, effects = "all"),
        list(conditional = "Days", random = "Subject")
      )
      expect_equal(
        find_predictors(m1, effects = "all", flatten = TRUE),
        c("Days", "Subject")
      )
      expect_equal(
        find_predictors(m1, effects = "fixed"),
        list(conditional = "Days")
      )
      expect_equal(
        find_predictors(m1, effects = "fixed", flatten = TRUE),
        "Days"
      )
      expect_equal(
        find_predictors(m1, effects = "random"),
        list(random = "Subject")
      )
      expect_equal(
        find_predictors(m1, effects = "random", flatten = TRUE),
        "Subject"
      )
      expect_equal(
        find_predictors(m2, effects = "all"),
        list(
          conditional = "Days",
          random = c("mysubgrp", "mygrp", "Subject")
        )
      )
      expect_equal(
        find_predictors(m2, effects = "all", flatten = TRUE),
        c("Days", "mysubgrp", "mygrp", "Subject")
      )
      expect_equal(
        find_predictors(m2, effects = "fixed"),
        list(conditional = "Days")
      )
      expect_equal(find_predictors(m2, effects = "random"), list(random = c("mysubgrp", "mygrp", "Subject")))
      expect_null(find_predictors(m2, effects = "all", component = "zi"))
      expect_null(find_predictors(m2, effects = "fixed", component = "zi"))
      expect_null(find_predictors(m2, effects = "random", component = "zi"))
    })

    test_that("find_random", {
      expect_equal(find_random(m1), list(random = "Subject"))
      expect_equal(find_random(m1, flatten = TRUE), "Subject")
      expect_equal(find_random(m2), list(random = c(
        "mysubgrp:mygrp", "mygrp", "Subject"
      )))
      expect_equal(find_random(m2, split_nested = TRUE), list(random = c("mysubgrp", "mygrp", "Subject")))
      expect_equal(
        find_random(m2, flatten = TRUE),
        c("mysubgrp:mygrp", "mygrp", "Subject")
      )
      expect_equal(
        find_random(m2, split_nested = TRUE, flatten = TRUE),
        c("mysubgrp", "mygrp", "Subject")
      )
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "Reaction")
      expect_identical(find_response(m2), "Reaction")
    })

    test_that("get_response", {
      expect_equal(get_response(m1), sleepstudy$Reaction)
    })

    test_that("link_inverse", {
      expect_identical(link_inverse(m1)(.2), .2)
      expect_identical(link_inverse(m2)(.2), .2)
    })

    test_that("get_data", {
      expect_equal(colnames(get_data(m1)), c("Reaction", "Days", "Subject"))
      expect_equal(colnames(get_data(m1, effects = "all")), c("Reaction", "Days", "Subject"))
      expect_equal(colnames(get_data(m1, effects = "random")), "Subject")
      expect_equal(
        colnames(get_data(m2)),
        c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
      )
      expect_equal(
        colnames(get_data(m2, effects = "all")),
        c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
      )
      expect_equal(colnames(get_data(m2, effects = "random")), c("mysubgrp", "mygrp", "Subject"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_length(find_formula(m2), 2)
      expect_equal(
        find_formula(m1, component = "conditional"),
        list(
          conditional = as.formula("Reaction ~ Days"),
          random = as.formula("~Days | Subject")
        )
      )
      expect_equal(
        find_formula(m2, component = "conditional"),
        list(
          conditional = as.formula("Reaction ~ Days"),
          random = list(
            as.formula("~1 | mysubgrp:mygrp"),
            as.formula("~1 | mygrp"),
            as.formula("~1 | Subject")
          )
        )
      )
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = "Reaction",
          conditional = "Days",
          random = c("Days", "Subject")
        )
      )
      expect_identical(
        find_terms(m1, flatten = TRUE),
        c("Reaction", "Days", "Subject")
      )
      expect_identical(
        find_terms(m2),
        list(
          response = "Reaction",
          conditional = "Days",
          random = c("mysubgrp", "mygrp", "Subject")
        )
      )
      expect_identical(
        find_terms(m2, flatten = TRUE),
        c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
      )
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = "Reaction",
          conditional = "Days",
          random = "Subject"
        )
      )
    })

    test_that("get_response", {
      expect_identical(get_response(m1), sleepstudy$Reaction)
    })

    test_that("get_predictors", {
      expect_identical(colnames(get_predictors(m1)), "Days")
      expect_identical(colnames(get_predictors(m2)), "Days")
    })

    test_that("get_random", {
      expect_identical(colnames(get_random(m1)), "Subject")
      expect_identical(
        colnames(get_random(m2)),
        c("mysubgrp", "mygrp", "Subject")
      )
    })

    test_that("clean_names", {
      expect_identical(clean_names(m1), c("Reaction", "Days", "Subject"))
      expect_identical(
        clean_names(m2),
        c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
      )
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
      expect_false(is.null(link_function(m2)))
    })

    test_that("find_parameters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = c("(Intercept)", "Days"),
          random = list(Subject = c("(Intercept)", "Days"))
        )
      )
      expect_equal(nrow(get_parameters(m1)), 2)
      expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "Days"))

      expect_equal(
        find_parameters(m2),
        list(
          conditional = c("(Intercept)", "Days"),
          random = list(
            `mysubgrp:mygrp` = "(Intercept)",
            Subject = "(Intercept)",
            mygrp = "(Intercept)"
          )
        )
      )

      expect_equal(nrow(get_parameters(m2)), 2)
      expect_equal(get_parameters(m2)$Parameter, c("(Intercept)", "Days"))
      expect_equal(
        names(get_parameters(m2, effects = "random")),
        c("mysubgrp:mygrp", "Subject", "mygrp")
      )
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
      expect_false(is_multivariate(m2))
    })

    test_that("get_variance", {
      skip_on_cran()
      skip_on_travis()

      expect_equal(
        get_variance(m1),
        list(
          var.fixed = 972.98333873885542288917,
          var.random = 1909.82627106414997797401,
          var.residual = 401.79840084390571064432,
          var.distribution = 401.79840084390571064432,
          var.dispersion = 0,
          var.intercept = c(Subject = 750.51639089692923789698),
          var.slope = c(Subject.Days = 41.06728604073937560770),
          cor.slope_intercept = c(Subject = -0.00703001666895963079)
        ),
        tolerance = 1e-4
      )

      expect_equal(
        get_variance(m2),
        list(
          var.fixed = 914.841369525452,
          var.random = 1406.78220090082,
          var.residual = 809.318117324236,
          var.distribution = 809.318117324236,
          var.dispersion = 0,
          var.intercept = c(
            `mysubgrp:mygrp` = 0,
            Subject = 1390.66848951126,
            mygrp = 16.113711389561
          )
        ),
        tolerance = 1e-4
      )
    })

    test_that("find_algorithm", {
      expect_equal(
        find_algorithm(m1),
        list(algorithm = "REML", optimizer = "rlmer.fit.DAS.nondiag")
      )
    })

    test_that("find_random_slopes", {
      expect_equal(find_random_slopes(m1), list(random = "Days"))
      expect_null(find_random_slopes(m2))
    })

    test_that("find_statistic", {
      expect_identical(find_statistic(m1), "t-statistic")
      expect_identical(find_statistic(m2), "t-statistic")
    })
  }
}
