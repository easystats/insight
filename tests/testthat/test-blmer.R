if (require("testthat") && require("insight") && require("blme")) {
  context("insight, find_predictors")

  data(sleepstudy)

  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <- sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m1 <- blmer(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    cov.prior = NULL
  )

  m2 <- blmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy,
    cov.prior = wishart
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
    expect_true(model_info(m2)$is_linear)
  })

  test_that("find_predictors", {
    expect_equal(find_predictors(m1, effects = "all"), list(conditional = "Days", random = "Subject"))
    expect_equal(find_predictors(m1, effects = "all", flatten = TRUE), c("Days", "Subject"))
    expect_equal(find_predictors(m1, effects = "fixed"), list(conditional = "Days"))
    expect_equal(find_predictors(m1, effects = "fixed", flatten = TRUE), "Days")
    expect_equal(find_predictors(m1, effects = "random"), list(random = "Subject"))
    expect_equal(find_predictors(m1, effects = "random", flatten = TRUE), "Subject")
    expect_equal(find_predictors(m2, effects = "all"), list(conditional = "Days", random = c("mysubgrp", "mygrp", "Subject")))
    expect_equal(find_predictors(m2, effects = "all", flatten = TRUE), c("Days", "mysubgrp", "mygrp", "Subject"))
    expect_equal(find_predictors(m2, effects = "fixed"), list(conditional = "Days"))
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
    expect_equal(find_random(m2, flatten = TRUE), c("mysubgrp:mygrp", "mygrp", "Subject"))
    expect_equal(find_random(m2, split_nested = TRUE, flatten = TRUE), c("mysubgrp", "mygrp", "Subject"))
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
    expect_equal(colnames(get_data(m2)), c("Reaction", "Days", "mygrp", "mysubgrp", "Subject"))
    expect_equal(colnames(get_data(m2, effects = "all")), c("Reaction", "Days", "mygrp", "mysubgrp", "Subject"))
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
    expect_identical(find_terms(m1), list(
      response = "Reaction",
      conditional = "Days",
      random = "Subject"
    ))
    expect_identical(find_terms(m1, flatten = TRUE), c("Reaction", "Days", "Subject"))
    expect_identical(find_terms(m2), list(
      response = "Reaction",
      conditional = "Days",
      random = c("mysubgrp", "mygrp", "Subject")
    ))
    expect_identical(find_terms(m2, flatten = TRUE), c("Reaction", "Days", "mysubgrp", "mygrp", "Subject"))
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
    expect_identical(clean_names(m2), c("Reaction", "Days", "mysubgrp", "mygrp", "Subject"))
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
    expect_equal(get_parameters(m1)$parameter, c("(Intercept)", "Days"))

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
    expect_equal(get_parameters(m2)$parameter, c("(Intercept)", "Days"))
    expect_equal(names(get_parameters(m2, effects = "random")), c("mysubgrp:mygrp", "Subject", "mygrp"))
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
    expect_false(is_multivariate(m2))
  })

  test_that("get_variance", {

    skip_on_cran()

    expect_equal(get_variance(m1), list(
      var.fixed = 908.95336262316459396970,
      var.random = 1698.23306388298283309268,
      var.residual = 654.94079585243218843971,
      var.distribution = 654.94079585243218843971,
      var.dispersion = 0,
      var.intercept = c(Subject = 611.89760710463770010392),
      var.slope = c(Subject.Days = 35.08106944030500073950),
      cor.slope_intercept = c(Subject = 0.06561803142425107205)
    ),
    tolerance = 1e-4)

    expect_equal(get_variance_fixed(m1), c(var.fixed = 908.95336262316459396970), tolerance = 1e-4)
    expect_equal(get_variance_random(m1), c(var.random = 1698.23306388298283309268), tolerance = 1e-4)
    expect_equal(get_variance_residual(m1), c(var.residual = 654.94079585243218843971), tolerance = 1e-4)
    expect_equal(get_variance_distribution(m1), c(var.distribution = 654.94079585243218843971), tolerance = 1e-4)
    expect_equal(get_variance_dispersion(m1), c(var.dispersion = 0), tolerance = 1e-4)

    expect_equal(get_variance_intercept(m1), c(var.intercept.Subject = 611.89760710463770010392), toleance = 1e-4)
    expect_equal(get_variance_slope(m1), c(var.slope.Subject.Days = 35.08106944030500073950), toleance = 1e-4)
    expect_equal(get_correlation_slope_intercept(m1), c(cor.slope_intercept.Subject = 0.06561803), toleance = 1e-4)

    expect_equal(get_variance(m2), list(
      var.fixed = 899.71188516552831515583,
      var.random = 1412.32769480994716104760,
      var.residual = 928.81331970697453925823,
      var.distribution = 928.81331970697453925823,
      var.dispersion = 0,
      var.intercept = c(
        `mysubgrp:mygrp` = 12.61789198780912713005,
        Subject = 1376.88290392979729404033,
        mygrp = 22.82689889233859048545
      )
    ),
    tolerance = 1e-4)
  })

  test_that("find_algorithm", {
    expect_equal(find_algorithm(m1), list(
      algorithm = "REML", optimizer = "nloptwrap"
    ))
  })

  test_that("find_random_slopes", {
    expect_equal(find_random_slopes(m1), list(random = "Days"))
    expect_null(find_random_slopes(m2))
  })
}
