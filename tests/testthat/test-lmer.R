if (require("testthat") &&
  require("insight") &&
  require("lme4")) {
  context("insight, find_predictors")

  data(sleepstudy)

  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m1 <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy
  )

  m2 <- lme4::lmer(Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
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
    expect_equal(find_random(m2), list(random = c("mysubgrp:mygrp", "mygrp", "Subject")))
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
      c("Reaction", "Days", "mygrp", "mysubgrp", "Subject")
    )
    expect_equal(
      colnames(get_data(m2, effects = "all")),
      c("Reaction", "Days", "mygrp", "mysubgrp", "Subject")
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
        random = as.formula("~1 + Days | Subject")
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
    expect_identical(
      find_variables(m1, flatten = TRUE),
      c("Reaction", "Days", "Subject")
    )
    expect_identical(
      find_variables(m2),
      list(
        response = "Reaction",
        conditional = "Days",
        random = c("mysubgrp", "mygrp", "Subject")
      )
    )
    expect_identical(
      find_variables(m2, flatten = TRUE),
      c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
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
    expect_identical(colnames(get_random(m2)), c("mysubgrp", "mygrp", "Subject"))
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
        var.fixed = 908.953362623165,
        var.random = 1698.23306388298,
        var.residual = 654.940795852432,
        var.distribution = 654.940795852432,
        var.dispersion = 0,
        var.intercept = c(Subject = 611.897607104638),
        var.slope = c(Subject.Days = 35.081069440305),
        cor.slope_intercept = c(Subject = 0.0656180314242511)
      ),
      tolerance = 1e-1
    )

    expect_equal(get_variance_fixed(m1),
      c(var.fixed = 908.95336262316459396970),
      tolerance = 1e-1
    )
    expect_equal(get_variance_random(m1),
      c(var.random = 1698.23306388298283309268),
      tolerance = 1e-1
    )
    expect_equal(
      get_variance_residual(m1),
      c(var.residual = 654.94079585243218843971),
      tolerance = 1e-1
    )
    expect_equal(
      get_variance_distribution(m1),
      c(var.distribution = 654.94079585243218843971),
      tolerance = 1e-1
    )
    expect_equal(get_variance_dispersion(m1),
      c(var.dispersion = 0),
      tolerance = 1e-1
    )

    expect_equal(
      get_variance_intercept(m1),
      c(var.intercept.Subject = 611.89760710463770010392),
      toleance = 1e-1
    )
    expect_equal(
      get_variance_slope(m1),
      c(var.slope.Subject.Days = 35.08106944030500073950),
      toleance = 1e-1
    )
    expect_equal(
      get_correlation_slope_intercept(m1),
      c(cor.slope_intercept.Subject = 0.06561803),
      toleance = 1e-1
    )

    expect_warning(expect_equal(
      get_variance(m2),
      list(
        var.fixed = 889.329700216337,
        var.residual = 941.817768377025,
        var.distribution = 941.817768377025,
        var.dispersion = 0,
        var.intercept = c(
          `mysubgrp:mygrp` = 0,
          Subject = 1357.35782386825,
          mygrp = 24.4073139080596
        )
      ),
      tolerance = 1e-1,
    ))
  })

  test_that("find_algorithm", {
    expect_equal(
      find_algorithm(m1),
      list(algorithm = "REML", optimizer = "nloptwrap")
    )
  })

  test_that("find_random_slopes", {
    expect_equal(find_random_slopes(m1), list(random = "Days"))
    expect_null(find_random_slopes(m2))
  })


  m3 <- lme4::lmer(Reaction ~ (1 + Days | Subject),
    data = sleepstudy
  )

  m4 <- lme4::lmer(Reaction ~ (1 |
    mygrp / mysubgrp) + (1 | Subject),
  data = sleepstudy
  )

  m5 <- lme4::lmer(Reaction ~ 1 + (1 + Days | Subject),
    data = sleepstudy
  )

  m6 <- lme4::lmer(Reaction ~ 1 + (1 |
    mygrp / mysubgrp) + (1 | Subject),
  data = sleepstudy
  )

  test_that("find_formula", {
    expect_equal(
      find_formula(m3),
      list(
        conditional = as.formula("Reaction ~ 1"),
        random = as.formula("~1 + Days | Subject")
      )
    )

    expect_equal(
      find_formula(m5),
      list(
        conditional = as.formula("Reaction ~ 1"),
        random = as.formula("~1 + Days | Subject")
      )
    )

    expect_equal(
      find_formula(m4),
      list(
        conditional = as.formula("Reaction ~ 1"),
        random = list(
          as.formula("~1 | mysubgrp:mygrp"),
          as.formula("~1 | mygrp"),
          as.formula("~1 | Subject")
        )
      )
    )

    expect_equal(
      find_formula(m6),
      list(
        conditional = as.formula("Reaction ~ 1"),
        random = list(
          as.formula("~1 | mysubgrp:mygrp"),
          as.formula("~1 | mygrp"),
          as.formula("~1 | Subject")
        )
      )
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
    expect_identical(find_statistic(m2), "t-statistic")
  })
}
