# TODO: why the model fails to run
# see https://github.com/easystats/insight/pull/735
skip_if_not_installed("lme4")
skip_if_not_installed("afex")

data(sleepstudy, package = "lme4")
df_sleepstudy <- sleepstudy

set.seed(123)
df_sleepstudy$mygrp <- sample(1:5, size = nrow(df_sleepstudy), replace = TRUE)
df_sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- df_sleepstudy$mygrp == i
  df_sleepstudy$mysubgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

m1_mixed <<- afex::mixed(Reaction ~ Days + (1 + Days | Subject), data = df_sleepstudy)
m2_mixed <<- afex::mixed(Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject), data = df_sleepstudy)

test_that("model_info", {
  expect_true(model_info(m1_mixed)$is_linear)
  expect_true(model_info(m2_mixed)$is_linear)
})

test_that("find_predictors", {
  expect_equal(
    find_predictors(m1_mixed, effects = "all"),
    list(conditional = "Days", random = "Subject")
  )
  expect_equal(
    find_predictors(m1_mixed, effects = "all", flatten = TRUE),
    c("Days", "Subject")
  )
  expect_equal(
    find_predictors(m1_mixed, effects = "fixed"),
    list(conditional = "Days")
  )
  expect_equal(
    find_predictors(m1_mixed, effects = "fixed", flatten = TRUE),
    "Days"
  )
  expect_equal(
    find_predictors(m1_mixed, effects = "random"),
    list(random = "Subject")
  )
  expect_equal(
    find_predictors(m1_mixed, effects = "random", flatten = TRUE),
    "Subject"
  )
  expect_equal(
    find_predictors(m2_mixed, effects = "all"),
    list(
      conditional = "Days",
      random = c("mysubgrp", "mygrp", "Subject")
    )
  )
  expect_equal(
    find_predictors(m2_mixed, effects = "all", flatten = TRUE),
    c("Days", "mysubgrp", "mygrp", "Subject")
  )
  expect_equal(
    find_predictors(m2_mixed, effects = "fixed"),
    list(conditional = "Days")
  )
  expect_equal(find_predictors(m2_mixed, effects = "random"), list(random = c("mysubgrp", "mygrp", "Subject")))
  expect_null(find_predictors(m2_mixed, effects = "all", component = "zi"))
  expect_null(find_predictors(m2_mixed, effects = "fixed", component = "zi"))
  expect_null(find_predictors(m2_mixed, effects = "random", component = "zi"))
})

test_that("find_random", {
  expect_equal(find_random(m1_mixed), list(random = "Subject"))
  expect_equal(find_random(m1_mixed, flatten = TRUE), "Subject")
  expect_equal(find_random(m2_mixed), list(random = c("mysubgrp:mygrp", "mygrp", "Subject")))
  expect_equal(find_random(m2_mixed, split_nested = TRUE), list(random = c("mysubgrp", "mygrp", "Subject")))
  expect_equal(
    find_random(m2_mixed, flatten = TRUE),
    c("mysubgrp:mygrp", "mygrp", "Subject")
  )
  expect_equal(
    find_random(m2_mixed, split_nested = TRUE, flatten = TRUE),
    c("mysubgrp", "mygrp", "Subject")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1_mixed), "Reaction")
  expect_identical(find_response(m2_mixed), "Reaction")
})

test_that("get_response", {
  expect_equal(get_response(m1_mixed), df_sleepstudy$Reaction)
})

test_that("link_inverse", {
  expect_identical(link_inverse(m1_mixed)(0.2), 0.2)
  expect_identical(link_inverse(m2_mixed)(0.2), 0.2)
})

test_that("get_data", {
  expect_equal(colnames(get_data(m1_mixed)), c("Reaction", "Days", "Subject"))
  expect_equal(colnames(get_data(m1_mixed, effects = "all")), c("Reaction", "Days", "Subject"))
  expect_equal(colnames(get_data(m1_mixed, effects = "random")), "Subject")
  expect_equal(
    colnames(get_data(m2_mixed)),
    c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
  )
  expect_equal(
    colnames(get_data(m2_mixed, effects = "all")),
    c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
  )
  expect_equal(colnames(get_data(m2_mixed, effects = "random")), c("mysubgrp", "mygrp", "Subject"))
})

test_that("get_df", {
  expect_equal(
    get_df(m1_mixed, type = "residual"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1_mixed, type = "normal"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m1_mixed, type = "wald"),
    Inf,
    ignore_attr = TRUE
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1_mixed), 2)
  expect_length(find_formula(m2_mixed), 2)
  expect_equal(
    find_formula(m1_mixed, component = "conditional"),
    list(
      conditional = as.formula("Reaction ~ Days"),
      random = as.formula("~1 + Days | Subject")
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2_mixed, component = "conditional"),
    list(
      conditional = as.formula("Reaction ~ Days"),
      random = list(
        as.formula("~1 | mysubgrp:mygrp"),
        as.formula("~1 | mygrp"),
        as.formula("~1 | Subject")
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1_mixed),
    list(
      response = "Reaction",
      conditional = "Days",
      random = "Subject"
    )
  )
  expect_identical(
    find_variables(m1_mixed, flatten = TRUE),
    c("Reaction", "Days", "Subject")
  )
  expect_identical(
    find_variables(m2_mixed),
    list(
      response = "Reaction",
      conditional = "Days",
      random = c("mysubgrp", "mygrp", "Subject")
    )
  )
  expect_identical(
    find_variables(m2_mixed, flatten = TRUE),
    c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
  )
})

test_that("get_response", {
  expect_identical(get_response(m1_mixed), df_sleepstudy$Reaction)
})

test_that("get_predictors", {
  expect_identical(colnames(get_predictors(m1_mixed)), "Days")
  expect_identical(colnames(get_predictors(m2_mixed)), "Days")
})

test_that("get_random", {
  expect_identical(colnames(get_random(m1_mixed)), "Subject")
  expect_identical(colnames(get_random(m2_mixed)), c("mysubgrp", "mygrp", "Subject"))
})

test_that("clean_names", {
  expect_identical(clean_names(m1_mixed), c("Reaction", "Days", "Subject"))
  expect_identical(
    clean_names(m2_mixed),
    c("Reaction", "Days", "mysubgrp", "mygrp", "Subject")
  )
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1_mixed)))
  expect_false(is.null(link_function(m2_mixed)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1_mixed),
    list(
      conditional = c("(Intercept)", "Days"),
      random = list(Subject = c("(Intercept)", "Days"))
    )
  )
  expect_equal(nrow(get_parameters(m1_mixed)), 2)
  expect_equal(get_parameters(m1_mixed)$Parameter, c("(Intercept)", "Days"))

  expect_equal(
    find_parameters(m2_mixed),
    list(
      conditional = c("(Intercept)", "Days"),
      random = list(
        `mysubgrp:mygrp` = "(Intercept)",
        Subject = "(Intercept)",
        mygrp = "(Intercept)"
      )
    )
  )

  expect_equal(nrow(get_parameters(m2_mixed)), 2)
  expect_equal(get_parameters(m2_mixed)$Parameter, c("(Intercept)", "Days"))
  expect_named(get_parameters(m2_mixed, effects = "random"), c("mysubgrp:mygrp", "Subject", "mygrp"))
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1_mixed))
  expect_false(is_multivariate(m2_mixed))
})

test_that("get_variance", {
  expect_equal(
    get_variance(m1_mixed),
    list(
      var.fixed = 908.9534,
      var.random = 1698.084,
      var.residual = 654.94,
      var.distribution = 654.94,
      var.dispersion = 0,
      var.intercept = c(Subject = 612.1002),
      var.slope = c(Subject.Days = 35.07171),
      cor.slope_intercept = c(Subject = 0.06555124)
    ),
    tolerance = 1e-1
  )

  expect_equal(get_variance_fixed(m1_mixed),
    c(var.fixed = 908.9534),
    tolerance = 1e-1
  )
  expect_equal(get_variance_random(m1_mixed),
    c(var.random = 1698.084),
    tolerance = 1e-1
  )
  expect_equal(
    get_variance_residual(m1_mixed),
    c(var.residual = 654.94),
    tolerance = 1e-1
  )
  expect_equal(
    get_variance_distribution(m1_mixed),
    c(var.distribution = 654.94),
    tolerance = 1e-1
  )
  expect_equal(get_variance_dispersion(m1_mixed),
    c(var.dispersion = 0),
    tolerance = 1e-1
  )

  expect_equal(
    get_variance_intercept(m1_mixed),
    c(var.intercept.Subject = 612.1002),
    tolerance = 1e-1
  )
  expect_equal(
    get_variance_slope(m1_mixed),
    c(var.slope.Subject.Days = 35.07171),
    tolerance = 1e-1
  )
  expect_equal(
    get_correlation_slope_intercept(m1_mixed),
    c(cor.slope_intercept.Subject = 0.06555124),
    tolerance = 1e-1
  )


  expect_warning(expect_equal(
    get_variance(m2_mixed),
    list(
      var.fixed = 889.3301,
      var.residual = 941.8135,
      var.distribution = 941.8135,
      var.dispersion = 0,
      var.intercept = c(
        `mysubgrp:mygrp` = 0,
        Subject = 1357.4257,
        mygrp = 24.4064
      )
    ),
    tolerance = 1e-1,
  ))
})

test_that("find_algorithm", {
  expect_equal(
    find_algorithm(m1_mixed),
    list(algorithm = "REML", optimizer = "nloptwrap")
  )
})

test_that("find_random_slopes", {
  expect_equal(find_random_slopes(m1_mixed), list(random = "Days"))
  expect_null(find_random_slopes(m2_mixed))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1_mixed), "t-statistic")
  expect_identical(find_statistic(m2_mixed), "t-statistic")
})
