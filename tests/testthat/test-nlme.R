skip_if_not_installed("nlme")

data(Soybean, package = "nlme")
data(Wafer, package = "nlme")
nlme_loblolly <- datasets::Loblolly
nlme_soybean <- Soybean
nlme_wafer <- Wafer

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
