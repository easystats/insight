if (requiet("testthat") &&
  requiet("insight") &&
  requiet("nlme") &&
  requiet("lme4")) {
  data("sleepstudy")
  data(Orthodont)
  m1 <- lme(Reaction ~ Days,
    random = ~ 1 + Days | Subject,
    data = sleepstudy
  )

  m2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~1)

  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m3 <- lme(Reaction ~ Days,
    random = ~ 1 | mygrp / mysubgrp,
    data = sleepstudy
  )

  # from easystats/insight/482
  cr <- corAR1(form = ~ 1 | Mare)
  m4 <- lme(follicles ~ Time, Ovary, correlation = cr)

  test_that("nested_varCorr", {
    skip_on_cran()

    expect_equal(
      insight:::.get_nested_lme_varcorr(m3),
      list(
        mysubgrp = structure(
          7.508310765,
          .Dim = c(1L, 1L),
          .Dimnames = list("(Intercept)", "(Intercept)")
        ),
        mygrp = structure(
          0.004897827,
          .Dim = c(1L, 1L),
          .Dimnames = list("(Intercept)", "(Intercept)")
        )
      ),
      tolerance = 1e-3
    )
  })


  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "Days"))
    expect_identical(find_predictors(m2), list(conditional = c("age", "Sex")))
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(conditional = "Days", random = "Subject")
    )
    expect_identical(find_predictors(m2, effects = "all"), list(conditional = c("age", "Sex")))
    expect_identical(find_predictors(m1, flatten = TRUE), "Days")
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "Subject")
    )
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "Reaction")
    expect_identical(find_response(m2), "distance")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), sleepstudy$Reaction)
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "Subject"))
    expect_null(find_random(m2))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), data.frame(Subject = sleepstudy$Subject), ignore_attr = TRUE)
    expect_warning(get_random(m2))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 180)
    expect_equal(colnames(get_data(m1)), c("Reaction", "Days", "Subject"))
    expect_equal(colnames(get_data(m2)), c("distance", "age", "Sex"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("Reaction ~ Days"),
        random = as.formula("~1 + Days | Subject")
      ),
      ignore_attr = TRUE
    )
    expect_length(find_formula(m2), 2)
    expect_equal(
      find_formula(m2),
      list(
        conditional = as.formula("distance ~ age + Sex"),
        random = as.formula("~1")
      ),
      ignore_attr = TRUE
    )
    expect_length(find_formula(m4), 2)
    expect_equal(
      find_formula(m4),
      list(
        conditional = as.formula("follicles ~ Time"),
        correlation = as.formula("~1 | Mare")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(m1),
      list(
        response = "Reaction",
        conditional = "Days",
        random = "Subject"
      )
    )
    expect_equal(
      find_variables(m1, flatten = TRUE),
      c("Reaction", "Days", "Subject")
    )
    expect_equal(
      find_variables(m2),
      list(
        response = "distance",
        conditional = c("age", "Sex")
      )
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 180)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "Days"),
        random = c("(Intercept)", "Days")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 2)
    expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "Days"))
    expect_equal(
      find_parameters(m2),
      list(
        conditional = c("(Intercept)", "age", "SexFemale"),
        random = c("(Intercept)")
      )
    )
  })

  test_that("find_algorithm", {
    expect_equal(
      find_algorithm(m1),
      list(algorithm = "REML", optimizer = "nlminb")
    )
  })

  test_that("get_variance", {
    skip_on_cran()

    expect_equal(
      get_variance(m1),
      list(
        var.fixed = 908.95336262308865116211,
        var.random = 1698.06593646939654718153,
        var.residual = 654.94240352794997761521,
        var.distribution = 654.94240352794997761521,
        var.dispersion = 0,
        var.intercept = c(Subject = 612.07951112963326067984),
        var.slope = c(Subject.Days = 35.07130179308116169068),
        cor.slope_intercept = c(Subject = 0.06600000000000000311)
      ),
      tolerance = 1e-3
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
    expect_identical(find_statistic(m2), "t-statistic")
    expect_identical(find_statistic(m3), "t-statistic")
  })
}
