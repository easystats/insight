skip_if_not_installed("nlme")

nlme_loblolly <- datasets::Loblolly
nlme_soybean <- nlme::Soybean
nlme_wafer <- nlme::Wafer

# one grouping level, taken from grouped data, no fixed-effect covariates
m_nlme1 <- nlme::nlme(
  height ~ SSasymp(age, Asym, R0, lrc),
  data = nlme_loblolly,
  fixed = Asym + R0 + lrc ~ 1,
  random = Asym ~ 1,
  start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
)

# a fixed-effect covariate, two random parameters, explicit grouping factor
m_nlme2 <- nlme::nlme(
  weight ~ SSlogis(Time, Asym, xmid, scal),
  data = nlme_soybean,
  fixed = list(Asym ~ Variety, xmid ~ 1, scal ~ 1),
  random = Asym + xmid ~ 1 | Plot,
  start = c(18, 0, 52, 7.5)
)

# two nested grouping levels, Site within Wafer
m_nlme3 <- nlme::nlme(
  current ~ A + B * cos(4.5679 * voltage) + C * sin(4.5679 * voltage),
  data = nlme_wafer,
  fixed = A + B + C ~ 1,
  random = list(Wafer = A ~ 1, Site = A ~ 1),
  start = c(-4.26, 5.62, 1.26)
)

nlme_models <- list(m_nlme1, m_nlme2, m_nlme3)


test_that("find_formula, nlme", {
  f <- find_formula(m_nlme1)
  expect_named(f, c("conditional", "nonlinear", "random"))
  expect_equal(f$conditional, height ~ 1, ignore_attr = TRUE)
  expect_equal(f$nonlinear, ~ SSasymp(age, Asym, R0, lrc), ignore_attr = TRUE)
  expect_equal(f$random, ~ Asym | Seed, ignore_attr = TRUE)

  f <- find_formula(m_nlme2)
  expect_named(f, c("conditional", "nonlinear", "random"))
  expect_equal(f$conditional, weight ~ Variety, ignore_attr = TRUE)
  expect_equal(f$nonlinear, ~ SSlogis(Time, Asym, xmid, scal), ignore_attr = TRUE)
  expect_equal(f$random, ~ Asym + xmid | Plot, ignore_attr = TRUE)

  f <- find_formula(m_nlme3)
  expect_named(f, c("conditional", "nonlinear", "random"))
  expect_equal(f$conditional, current ~ 1, ignore_attr = TRUE)
  expect_equal(
    f$nonlinear,
    ~ A + B * cos(4.5679 * voltage) + C * sin(4.5679 * voltage),
    ignore_attr = TRUE
  )
  expect_length(f$random, 2)
  expect_equal(f$random[[1]], ~ A | Wafer, ignore_attr = TRUE)
  expect_equal(f$random[[2]], ~ A | Site:Wafer, ignore_attr = TRUE)
})


test_that("find_response, get_response, nlme", {
  responses <- c("height", "weight", "current")
  datasets <- list(nlme_loblolly, nlme_soybean, nlme_wafer)
  for (i in seq_along(nlme_models)) {
    expect_identical(find_response(nlme_models[[i]]), responses[i])
    expect_equal(
      get_response(nlme_models[[i]]),
      datasets[[i]][[responses[i]]],
      ignore_attr = TRUE
    )
  }
})


test_that("find_random, get_random, nlme", {
  expect_identical(find_random(m_nlme1), list(random = "Seed"))
  expect_identical(find_random(m_nlme2), list(random = "Plot"))
  expect_identical(find_random(m_nlme3), list(random = c("Wafer", "Site:Wafer")))
  expect_identical(
    find_random(m_nlme3, split_nested = TRUE),
    list(random = c("Wafer", "Site"))
  )
  for (m in nlme_models) {
    expect_identical(
      colnames(get_random(m)),
      find_random(m, split_nested = TRUE)$random
    )
  }
})


test_that("find_variables, nlme", {
  expect_identical(
    find_variables(m_nlme2),
    list(
      response = "weight",
      conditional = "Variety",
      nonlinear = c("Time", "Asym", "xmid", "scal"),
      random = "Plot"
    )
  )
  expect_identical(
    find_variables(m_nlme1),
    list(
      response = "height",
      nonlinear = c("age", "Asym", "R0", "lrc"),
      random = "Seed"
    )
  )
})


test_that("model_info, find_terms, find_predictors, nlme", {
  for (m in nlme_models) {
    expect_no_warning(info <- model_info(m))
    expect_true(info$is_mixed)
    expect_no_warning(trms <- find_terms(m))
    expect_true("nonlinear" %in% names(trms))
    expect_no_warning(preds <- find_predictors(m))
    expect_true("nonlinear" %in% names(preds))
  }
})


test_that("get_predicted, nlme", {
  for (m in nlme_models) {
    expect_no_warning(p <- get_predicted(m))
    expect_equal(as.vector(p), as.vector(stats::predict(m)), tolerance = 1e-10)
    d <- get_data(m)
    expect_no_warning(p <- get_predicted(m, data = d))
    expect_equal(
      as.vector(p),
      as.vector(stats::predict(m, newdata = d)),
      tolerance = 1e-10
    )
  }
})


test_that("get_predicted, nlme, standard errors", {
  skip_if_not_installed("marginaleffects")
  # oracle: live, marginaleffects::predictions(), which computes its own
  # Jacobian of predict() with respect to the fixed effects
  soybean <- as.data.frame(nlme_soybean)
  # rows with different `Time` and both `Variety` levels, so that each fixed
  # effect enters the Jacobian
  d <- soybean[c(1, which(soybean$Variety == "P")[5], 300), ]
  expect_setequal(as.character(d$Variety), c("F", "P"))
  p <- as.data.frame(get_predicted(m_nlme2, data = d, ci = 0.95))
  expect_equal(
    p$SE,
    marginaleffects::predictions(m_nlme2, newdata = d)$std.error,
    tolerance = 1e-4
  )
  # rows of different wafers, sites and voltages
  d <- as.data.frame(nlme_wafer)[c(1, 8, 45), ]
  p <- as.data.frame(get_predicted(m_nlme3, data = d, ci = 0.95))
  expect_equal(
    p$SE,
    marginaleffects::predictions(m_nlme3, newdata = d)$std.error,
    tolerance = 1e-4
  )
})


test_that("get_predicted, nlme, standard errors of a linear model", {
  # oracle: closed-form. The model is linear in its parameters, so the
  # Jacobian is the model matrix X, and the standard errors are the square
  # roots of the diagonal of X V X'
  m <- nlme::nlme(
    height ~ a + b1 * age,
    data = nlme_loblolly,
    fixed = a + b1 ~ 1,
    random = a ~ 1 | Seed,
    start = c(a = 0, b1 = 2.5)
  )
  loblolly <- as.data.frame(nlme_loblolly)
  d <- loblolly[match(c(3, 10, 25), loblolly$age), ]
  X <- cbind(1, d$age)
  se <- sqrt(diag(X %*% stats::vcov(m) %*% t(X)))
  p <- as.data.frame(get_predicted(m, data = d, ci = 0.95))
  expect_equal(p$SE, se, tolerance = 1e-6, ignore_attr = TRUE)
  p <- as.data.frame(get_predicted(m, data = d, ci = 0.95, level = 0))
  expect_equal(p$SE, se, tolerance = 1e-6, ignore_attr = TRUE)
})


test_that("get_predicted, nlme, standard errors on the population level", {
  # oracle: invariant. A grid without grouping levels of the model data gives
  # population-level predictions, so its standard errors equal those for an
  # explicit `level = 0`
  grid <- get_datagrid(m_nlme2, by = "Variety")
  p <- as.data.frame(get_predicted(m_nlme2, data = grid, ci = 0.95))
  p0 <- as.data.frame(get_predicted(m_nlme2, data = grid, ci = 0.95, level = 0))
  expect_equal(p$SE, p0$SE, tolerance = 1e-10)
  expect_false(anyNA(p$SE))
  # control: on the group level, the standard errors differ, so the
  # comparison above depends on the level
  d <- get_data(m_nlme2)[1:3, ]
  p0 <- as.data.frame(get_predicted(m_nlme2, data = d, ci = 0.95, level = 0))
  p1 <- as.data.frame(get_predicted(m_nlme2, data = d, ci = 0.95, level = 1))
  expect_gt(max(abs(p0$SE - p1$SE)), 1e-3)
})


test_that("get_predicted, nlme, confidence intervals", {
  # oracle: invariant, the Wald interval with the t-distribution
  for (m in nlme_models) {
    expect_silent(p <- as.data.frame(get_predicted(m, ci = 0.95)))
    expect_named(p, c("Predicted", "SE", "CI_low", "CI_high"))
    expect_false(anyNA(p$SE))
    crit <- stats::qt(0.975, min(get_df(m, type = "wald")))
    expect_equal(p$CI_low, p$Predicted - crit * p$SE, tolerance = 1e-10)
    expect_equal(p$CI_high, p$Predicted + crit * p$SE, tolerance = 1e-10)
    p <- as.data.frame(get_predicted(m, ci = NULL))
    expect_named(p, "Predicted")
  }
  # called directly without `data`, get_predicted_ci() uses the model data
  p <- get_predicted(m_nlme1, ci = 0.95)
  out <- get_predicted_ci(m_nlme1, p)
  expect_equal(out, attributes(p)$ci_data, tolerance = 1e-10, ignore_attr = TRUE)
})


test_that("get_predicted, nlme, unsupported interval arguments", {
  # each call gives a message that names the argument, and no intervals
  unsupported <- list(
    list(predict = "prediction", regexp = "predict = \"prediction\""),
    list(vcov = "HC0", regexp = "`vcov`"),
    list(vcov = stats::vcov(m_nlme1), regexp = "`vcov`"),
    list(ci_method = "satterthwaite", regexp = "`ci_method`")
  )
  for (u in unsupported) {
    call_args <- c(list(m_nlme1, ci = 0.95), u[names(u) != "regexp"])
    expect_message(p <- do.call(get_predicted, call_args), regexp = u$regexp, fixed = TRUE)
    expect_named(as.data.frame(p), "Predicted")
    expect_silent(do.call(get_predicted, c(call_args, verbose = FALSE)))
  }
})


test_that("get_predicted, nlme, data grid without random effects", {
  # the grouping factor of the grid has no level of the model data, so the
  # predictions are on the population level
  grid <- get_datagrid(m_nlme2, by = "Variety")
  expect_true(all(grid$Plot == 0))
  expect_false(any(as.character(grid$Plot) %in% levels(nlme_soybean$Plot)))
  expect_no_warning(p <- get_predicted(m_nlme2, data = grid))
  grid$Plot <- NULL
  expect_equal(
    as.vector(p),
    as.vector(stats::predict(m_nlme2, newdata = grid, level = 0)),
    tolerance = 1e-10
  )
  # an explicit `level` is passed to predict()
  d <- get_data(m_nlme2)[1:3, ]
  expect_equal(
    as.vector(get_predicted(m_nlme2, data = d, level = 0)),
    as.vector(stats::predict(m_nlme2, newdata = d, level = 0)),
    tolerance = 1e-10
  )
  # a data column named like a parameter is not checked as a variable
  d_par <- d
  d_par$Asym <- NA
  expect_no_warning(p <- get_predicted(m_nlme2, data = d_par))
  expect_equal(
    as.vector(p),
    as.vector(stats::predict(m_nlme2, newdata = d)),
    tolerance = 1e-10
  )
  # a factor level that is not in the model data gives NA for that row
  d$Variety <- factor(c("F", "X", "P"))
  expect_warning(
    p <- get_predicted(m_nlme2, data = d),
    regexp = "factor levels that are not in the model data"
  )
  expect_identical(is.na(as.vector(p)), c(FALSE, TRUE, FALSE))
  # the standard errors and intervals are NA for that row, and the other rows
  # get the values they have alone
  expect_warning(
    p <- as.data.frame(get_predicted(m_nlme2, data = d, ci = 0.95)),
    regexp = "factor levels that are not in the model data"
  )
  p13 <- as.data.frame(get_predicted(m_nlme2, data = d[c(1, 3), ], ci = 0.95))
  for (i in c("SE", "CI_low", "CI_high")) {
    expect_identical(is.na(p[[i]]), c(FALSE, TRUE, FALSE))
    expect_equal(p[[i]][c(1, 3)], p13[[i]], tolerance = 1e-10)
  }
})


test_that("find_formula, nlme, parameter names with dots", {
  set.seed(123)
  nlme_dots <- data.frame(
    x = rep(1:10, 10),
    w = rep(c(0, 1), 50),
    g = factor(rep(1:10, each = 10))
  )
  b <- 5 + nlme_dots$w + stats::rnorm(10, 0, 0.5)[nlme_dots$g]
  nlme_dots$y <- b + 3 * exp(-0.3 * nlme_dots$x) + stats::rnorm(100, 0, 0.1)
  m <- nlme::nlme(
    y ~ b + b.1 * exp(-b.2 * x),
    data = nlme_dots,
    fixed = list(b ~ w, b.1 ~ 1, b.2 ~ 1),
    random = b ~ 1 | g,
    start = c(5, 1, 3, 0.3)
  )
  # "b.1" and "b.2" are parameters, not terms "1" and "2" of parameter "b"
  expect_equal(find_formula(m)$conditional, y ~ w, ignore_attr = TRUE)
  expect_identical(find_variables(m)$conditional, "w")
})


test_that("find_statistic, supported_models, nlme", {
  expect_identical(find_statistic(m_nlme1), "t-statistic")
  expect_true("nlme" %in% supported_models())
})


test_that("get_variance, nlme", {
  expect_message(
    out <- get_variance(m_nlme1),
    regexp = "nonlinear mixed models"
  )
  expect_null(out)
  expect_silent(out <- get_variance(m_nlme1, verbose = FALSE))
  expect_null(out)
  # `component` is the second argument, as for other models
  expect_message(
    out <- get_variance(m_nlme1, "random"),
    regexp = "nonlinear mixed models"
  )
  expect_null(out)
  expect_message(
    out <- get_variance_random(m_nlme1),
    regexp = "nonlinear mixed models"
  )
  expect_null(out)
})


test_that("parameters, varcov, statistic, n_obs, get_data, nlme", {
  data_columns <- list(
    c("height", "age", "Seed"),
    c("weight", "Time", "Variety", "Plot"),
    c("current", "voltage", "Wafer", "Site")
  )
  for (i in seq_along(nlme_models)) {
    m <- nlme_models[[i]]
    expect_equal(
      get_parameters(m)$Estimate,
      unname(nlme::fixef(m)),
      tolerance = 1e-10
    )
    expect_identical(find_parameters(m)$conditional, names(nlme::fixef(m)))
    expect_equal(get_varcov(m), stats::vcov(m), tolerance = 1e-10, ignore_attr = TRUE)
    expect_equal(
      get_statistic(m)$Statistic,
      unname(summary(m)$tTable[, "t-value"]),
      tolerance = 1e-10
    )
    expect_identical(n_obs(m), stats::nobs(m))
    expect_true(all(data_columns[[i]] %in% colnames(get_data(m))))
  }
})
