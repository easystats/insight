skip_on_os(os = "mac")
skip_on_cran()

# LM and GLM --------------------------------------------------------------
# =========================================================================

test_that("get_predicted - lm", {
  skip_on_cran()

  x <- lm(mpg ~ cyl + hp, data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)

  # Relation vs. Prediction
  rezpred <- get_predicted(x, predict = "prediction", ci = 0.95)
  expect_equal(mean(abs(rezlink - rezpred)), 0, tolerance = 1e-3)
  expect_true(all(mean(summary(rezlink)$CI_high - summary(rezpred)$CI_high) < 0))

  # Confidence
  ref <- predict(x, se.fit = TRUE, interval = "confidence")
  rez <- as.data.frame(get_predicted(x, predict = "expectation", ci = 0.95))
  expect_identical(nrow(rez), 32L)
  expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-10)
  expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

  # Prediction
  ref <- predict(x, newdata = get_data(x), se.fit = TRUE, interval = "prediction")
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = 0.95))
  expect_identical(nrow(rez), 32L)
  expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
  expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

  # Bootstrap
  set.seed(333)
  ref <- predict(x, newdata = get_data(x), se.fit = TRUE, interval = "confidence")
  rez <- get_predicted(x, iterations = 600, ci = 0.95)
  expect_length(rez, 32)
  expect_null(nrow(rez))
  expect_equal(mean(abs(as.data.frame(ref$fit)$fit - summary(rez)$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(as.data.frame(ref$fit)$lwr - summary(rez)$CI_low)), 0, tolerance = 0.5)
  # TODO: Is it possible to get "prediction" CIs via bootstrapping?

  skip_if_not_installed("rstanarm")

  # vs. Bayesian
  xbayes <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = 0.95))
  rezbayes <- summary(get_predicted(xbayes, predict = "link", ci = 0.95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.1)
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = 0.95))
  rezbayes <- summary(get_predicted(xbayes, predict = "prediction", ci = 0.95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.2)
})


test_that("get_predicted - glm", {
  skip_on_cran()

  x <- glm(vs ~ wt, data = mtcars, family = "binomial")

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  expect_lt(min(rezlink), 0)
  expect_gt(min(rezrela), 0)
  expect_lt(min(summary(rezlink)$CI_low), 0)
  expect_gt(min(summary(rezrela)$CI_low), 0)

  # Relation vs. Prediction
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  rezpred <- get_predicted(x, predict = "prediction", ci = 0.95)
  expect_equal(mean(abs(rezrela - rezpred)), 0, tolerance = 1e-3)
  expect_true(all(mean(summary(rezrela)$CI_high - summary(rezpred)$CI_high) < 0))

  # Against stats::predict
  ref <- predict(x, se.fit = TRUE, type = "response", ci = 0.95)
  rez <- as.data.frame(get_predicted(x, predict = "expectation", ci = 0.95))
  expect_identical(nrow(rez), 32L)
  expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-4)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-4)
  ref <- as.data.frame(suppressWarnings(link_inverse(x)(predict.lm(x, interval = "confidence"))))
  expect_equal(max(abs(ref$lwr - rez$CI_low)), 0, tolerance = 1e-2)

  ref <- predict(x, se.fit = TRUE, type = "link")
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = 0.95))
  expect_identical(nrow(rez), 32L)
  expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-4)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-4)

  # Bootstrap
  set.seed(333)
  ref <- suppressWarnings(predict(x, se.fit = TRUE, type = "response"))
  rez <- suppressWarnings(summary(get_predicted(x, iterations = 800, verbose = FALSE, ci = 0.95)))
  expect_equal(mean(abs(ref$fit - rez$Predicted)), 0, tolerance = 0.1)

  skip_if_not_installed("rstanarm")

  # vs. Bayesian
  xbayes <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0, seed = 333)
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = 0.95))
  rezbayes <- summary(get_predicted(xbayes, predict = "link", ci = 0.95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.1)
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = 0.95))
  rezbayes <- summary(get_predicted(xbayes, predict = "prediction", ci = 0.95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  # expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.3)
})

test_that("get_predicted - glm", {
  skip_on_cran()
  skip_if_not_installed("modelbased")
  # link works for gaussian with log-link
  set.seed(123)
  dat <- data.frame(Y = rlnorm(100), x = rnorm(100))
  ## fit glm
  dat_glm <- glm(Y ~ 1, data = dat, family = gaussian(link = "log"))
  ## predictions on the response scale - correct
  out <- modelbased::estimate_relation(dat_glm, length = 1)
  expect_equal(
    out$Predicted,
    predict(dat_glm, type = "response")[1],
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  ## predictions on the link scale - incorrect
  out <- modelbased::estimate_link(dat_glm, length = 1)
  expect_equal(
    out$Predicted,
    predict(dat_glm, type = "link")[1],
    tolerance = 0.01,
    ignore_attr = TRUE
  )
})

test_that("get_predicted - lm (log)", {
  x <- lm(mpg ~ log(hp), data = mtcars)
  rez <- get_predicted(x)
  expect_length(rez, 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0, tolerance = 1e-4)
  expect_equal(max(abs(rez - stats::predict(x))), 0, tolerance = 1e-4)

  data <- as.data.frame(rez)
  expect_equal(max(abs(rez - data$Predicted)), 0, tolerance = 1e-4)
  expect_identical(nrow(data), 32L)
})


test_that("robust vcov", {
  skip_if_not_installed("sandwich")
  mod <- lm(mpg ~ hp, data = mtcars)
  se0 <- get_predicted_se(mod)
  se1 <- suppressWarnings(get_predicted_se(mod, vcov = "HC"))
  se2 <- suppressWarnings(get_predicted_se(mod, vcov = "HC3"))
  se3 <- get_predicted_se(mod, vcov = "HC", vcov_args = list(type = "HC3"))
  expect_true(all(se0 != se1))
  expect_true(all(se1 == se2))
  expect_true(all(se1 == se3))
  # hardcoded values obtained before vcov_estimation was deprecated
  expect_equal(head(se1), c(
    0.862974605863594, 0.862974605863594,
    1.04476534302177, 0.862974605863594,
    0.942213270105983, 0.911147902473696
  ),
  ignore_attr = TRUE
  )
  # various user inputs
  se1 <- get_predicted_se(mod, vcov = "HC2")
  se2 <- get_predicted_se(mod, vcov = "vcovHC", vcov_args = list(type = "HC2"))
  se3 <- get_predicted_se(mod, vcov = sandwich::vcovHC, vcov_args = list(type = "HC2"))
  expect_true(all(se1 == se2))
  expect_true(all(se1 == se3))
  se1 <- get_predicted_se(mod, vcov = "HC1")
  se2 <- get_predicted_se(mod, vcov = sandwich::vcovHC, vcov_args = list(type = "HC1"))
  expect_true(all(se1 == se2))
})


test_that("MASS::rlm", {
  skip_if_not_installed("MASS")
  mod <- MASS::rlm(mpg ~ hp + am, data = mtcars)
  p <- get_predicted.default(mod)
  expect_s3_class(p, "get_predicted")
  p <- data.frame(p)
  expect_true("CI_low" %in% colnames(p))
})


# Mixed --------------------------------------------------------------
# =========================================================================

test_that("get_predicted - lmerMod", {
  suppressWarnings(skip_if_not_installed("glmmTMB"))
  skip_if_not_installed("lme4")
  skip_if_not_installed("merTools")
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  suppressPackageStartupMessages({
    suppressWarnings(suppressMessages(library(rstanarm, quietly = TRUE, warn.conflicts = FALSE))) # nolint
  })

  x <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  rezpred <- get_predicted(x, predict = "prediction", ci = 0.95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)
  expect_true(all(summary(rezlink)$CI_high - summary(rezpred)$CI_high < 0))

  # Bootstrap
  set.seed(333)
  rez <- as.data.frame(get_predicted(x, iterations = 5, ci = 0.95))
  expect_identical(c(nrow(rez), ncol(rez)), c(32L, 9L))


  # Compare to merTools
  rez_merTools <- merTools::predictInterval(x, level = 0.95, seed = 333, n.sims = 2000)
  expect_equal(mean(abs(as.data.frame(rezpred)$CI_low - rez_merTools$lwr)), 0, tolerance = 0.5)


  # Compare to emmeans (not sure what it does)
  # refgrid <- emmeans::ref_grid(x, at = as.list(get_data(x)), data = get_data(x))
  # rez_emmeans <- as.data.frame(predict(refgrid, level = 0.95, interval = "prediction"))
  # This is completely off
  # expect_equal(mean(as.data.frame(rez)$CI_low - rez_emmeans$lower.PL), 0, tolerance = 0.5)

  # Compare with glmmTMB
  ref <- get_predicted(glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars), predict = "expectation", ci = 0.95)
  expect_equal(mean(abs(rezrela - ref)), 0, tolerance = 0.1) # A bit high
  # expect_equal(mean(abs(as.data.frame(rezrela)$SE - as.data.frame(ref)$SE)), 0, tolerance = 1e-5)
  # expect_equal(mean(abs(as.data.frame(rezrela)$CI_low - as.data.frame(ref)$CI_low)), 0, tolerance = 1e-5)

  # Compare with rstanarm
  xref <- suppressWarnings(
    rstanarm::stan_lmer(mpg ~ am + (1 | cyl),
      data = mtcars,
      refresh = 0, iter = 1000, seed = 333
    )
  )
  rez_stan <- get_predicted(xref, predict = "expectation", ci = 0.95)
  expect_equal(mean(abs(rezrela - rez_stan)), 0, tolerance = 0.1)
  # Different indeed
  # expect_equal(mean(as.data.frame(rezrela)$CI_low - as.data.frame(rez_stan)$CI_low), 0, tolerance = 0.5)
})


test_that("get_predicted - glmer with matrix response", {
  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  model <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp, family = binomial
  )
  grid <- get_datagrid(model, by = "period", range = "grid", preserve_range = FALSE)
  out <- as.data.frame(get_predicted(model, data = grid, ci = 0.95))
  expect_equal(out$Predicted, c(0.19808, 0.08392, 0.07402, 0.04843), tolerance = 1e-3)
  expect_equal(out$CI_low, c(0.1357, 0.04775, 0.0404, 0.02164), tolerance = 1e-3)
})


test_that("get_predicted - lmerMod (log)", {
  skip_if_not_installed("lme4")
  x <- lme4::lmer(mpg ~ am + log(hp) + (1 | cyl), data = mtcars)
  rez <- get_predicted(x)
  expect_length(rez, 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0, tolerance = 1e-4)
  expect_equal(max(abs(rez - stats::predict(x))), 0, tolerance = 1e-4)
  expect_equal(nrow(as.data.frame(rez)), 32, tolerance = 1e-4)

  # No random
  rez2 <- get_predicted(x, newdata = mtcars[c("am", "hp")], verbose = FALSE)
  expect_false(all(is.na(as.data.frame(rez2))))
})


test_that("get_predicted - merMod", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  x <- lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  expect_lt(min(rezlink), 0)
  expect_gt(min(rezrela), 0)
  expect_lt(min(summary(rezlink)$CI_low), 0)
  expect_gt(min(summary(rezrela)$CI_low), 0)
  expect_equal(max(abs(rezrela - stats::fitted(x))), 0, tolerance = 1e-4)
  expect_equal(max(abs(rezrela - stats::predict(x, type = "response"))), 0, tolerance = 1e-4)
  expect_identical(nrow(as.data.frame(rezlink)), 32L)

  # Compare with glmmTMB
  xref <- glmmTMB::glmmTMB(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rez_ref <- get_predicted(xref, predict = "expectation", ci = 0.95)
  expect_equal(max(abs(rezrela - rez_ref)), 0, tolerance = 1e-5)
  expect_equal(mean(abs(as.data.frame(rezrela)$SE - as.data.frame(rez_ref)$SE)), 0, tolerance = 0.2)
})


test_that("get_predicted - glmmTMB", {
  skip_if_not_installed("glmmTMB")
  x <- glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)

  # Bootstrap
  set.seed(333)
  rez <- as.data.frame(get_predicted(x, iterations = 5, predict = "link", ci = 0.95))
  expect_identical(c(nrow(rez), ncol(rez)), c(32L, 9L))

  # Binomial
  x <- glmmTMB::glmmTMB(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rezlink <- get_predicted(x, predict = "link", ci = 0.95)
  rezrela <- get_predicted(x, predict = "expectation", ci = 0.95)
  expect_lt(min(rezlink), 0)
  expect_gt(min(rezrela), 0)
  expect_lt(min(summary(rezlink)$CI_low), 0)
  expect_gt(min(summary(rezrela)$CI_low), 0)
  expect_equal(max(abs(rezrela - stats::fitted(x))), 0, tolerance = 1e-4)
  expect_equal(max(abs(rezrela - stats::predict(x, type = "response"))), 0, tolerance = 1e-4)
  expect_identical(nrow(as.data.frame(rez)), 32L)

  # No random
  rez <- get_predicted(x, newdata = mtcars["am"], verbose = FALSE, ci = 0.95)
  expect_false(all(is.na(as.data.frame(rez))))
  x <- glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
  rez <- get_predicted(x, data = data.frame(Petal.Width = c(0, 1, 2)), verbose = FALSE)
  expect_length(rez, 3)

  # vs. Bayesian
  # x <- glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars)
  # rez <- summary(get_predicted(x))
  # xref <- rstanarm::stan_lmer(mpg ~ am + (1 | cyl), data = mtcars, refresh = 0, iter = 1000, seed = 333)
  # rezbayes <- summary(get_predicted(xref))
  # expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  # expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.2)
})


# GAM --------------------------------------------------------------
# =========================================================================
test_that("get_predicted - mgcv::gam and gamm", {
  skip_if_not_installed("mgcv")
  x <- mgcv::gam(mpg ~ am + s(wt), data = mtcars)
  expect_length(get_predicted(x, ci = 0.95), 32)
  rez <- get_predicted(x, data = data.frame(am = c(0, 0, 1), wt = c(2, 3, 4)), ci = 0.95)
  expect_length(rez, 3)

  # No smooth
  rez <- get_predicted(x, data = data.frame(am = c(0, 0, 1)), ci = 0.95)
  expect_length(rez, 3)
  rez2 <- get_predicted(x, data = data.frame(am = c(0, 0, 1), wt = c(2, 3, 4)), ci = 0.95, include_smooth = FALSE)
  expect_equal(max(abs(as.numeric(rez - rez2))), 0, tolerance = 1e-4)
  expect_length(unique(attributes(rez)$data$wt), 1)

  # Bootstrap
  set.seed(333)
  rez <- summary(get_predicted(x, iterations = 50, ci = 0.95))
  expect_identical(nrow(rez), 32L)

  # Binomial
  x <- mgcv::gam(vs ~ am + s(wt), data = mtcars, family = "binomial")
  rez <- get_predicted(x, ci = 0.95)
  expect_length(rez, 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0, tolerance = 1e-4)
  expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0, tolerance = 1e-4)
  expect_identical(nrow(as.data.frame(rez)), 32L)

  # GAMM
  x <- mgcv::gamm(vs ~ am + s(wt), random = list(cyl = ~1), data = mtcars, family = "binomial", verbosePQL = FALSE)
  rez <- get_predicted(x, ci = 0.95)
  expect_length(rez, 32)
  expect_equal(max(abs(rez - x$gam$fitted.values)), 0, tolerance = 1e-4)
  expect_equal(max(abs(rez - stats::predict(x$gam, type = "response"))), 0, tolerance = 1e-4)
  expect_identical(nrow(as.data.frame(rez)), 32L)
})


# Bayesian --------------------------------------------------------------
# =========================================================================


test_that("get_predicted - rstanarm", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")

  suppressPackageStartupMessages({
    suppressWarnings(suppressMessages(library(rstanarm, quietly = TRUE, warn.conflicts = FALSE))) # nolint
  })

  # LM
  x <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
  rezlink <- summary(get_predicted(x, predict = "link", ci = 0.95))
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = 0.95))
  expect_equal(mean(abs(rezlink$Predicted - rezrela$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rezlink$CI_high - rezrela$CI_high)), 0, tolerance = 0.1)
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = 0.95))
  expect_equal(mean(abs(rezlink$Predicted - rezpred$Predicted)), 0, tolerance = 0.1)
  expect_true(all(mean(rezlink$CI_high - rezpred$CI_high) < 0))

  # GLM
  x <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0, seed = 333)
  rezlink <- summary(get_predicted(x, predict = "link", ci = 0.95))
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = 0.95))
  expect_lt(min(rezlink$Predicted), 0)
  expect_gt(min(rezrela$Predicted), 0)
  expect_lt(min(rezlink$CI_high), 0)
  expect_gt(min(rezrela$CI_high), 0)
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = 0.95))
  expect_equal(mean(abs(rezrela$Predicted - rezpred$Predicted)), 0, tolerance = 0.1)
  expect_true(all(mean(rezrela$CI_high - rezpred$CI_high) < 0))

  # Mixed
  x <- suppressWarnings(
    rstanarm::stan_lmer(mpg ~ am + (1 | cyl),
      data = mtcars, refresh = 0,
      seed = 333, iter = 500
    )
  )
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = 0.95))
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = 0.95))
  rezrela2 <- summary(get_predicted(x, predict = "expectation", ci = 0.95, include_random = FALSE))
  rezpred2 <- summary(get_predicted(x, predict = "prediction", ci = 0.95, include_random = FALSE))
  expect_gt(mean(abs(rezrela$Predicted - rezrela2$Predicted)), 0)
  expect_gt(mean(abs(rezpred$Predicted - rezpred2$Predicted)), 0)
  rezrela3 <- summary(get_predicted(x, predict = "expectation", ci = 0.95, data = mtcars["am"]), verbose = FALSE)
  expect_equal(mean(abs(rezrela2$Predicted - rezrela3$Predicted)), 0, tolerance = 0.001)
})


test_that("get_predicted - brms, auxiliary", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_sigma_2")
  dg <- get_datagrid(m, reference = "grid", include_random = TRUE)
  out <- get_predicted(m, data = dg, predict = "sigma")
  expect_equal(
    as.numeric(out),
    c(
      1.02337, 0.82524, 0.58538, 0.74573, 0.66292, 1.0336, 0.94714,
      0.74541, 0.71533, 0.7032, 0.63151, 0.65244, 0.58731, 0.45177,
      0.75789
    ),
    tolerance = 1e-4
  )
})


test_that("get_predicted - brms, categorical family", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_categorical_1_fct")
  out <- get_predicted(m, data = get_datagrid(m))
  expect_identical(ncol(out), 4L)
  expect_identical(nrow(out), 30L)
  expect_named(out, c("Row", "Response", "mpg", "Predicted"))
})


# FA / PCA ----------------------------------------------------------------
# =========================================================================

test_that("get_predicted - FA / PCA", {
  skip_if_not_installed("fungible")
  skip_if_not_installed("psych")

  # PCA
  x <- get_predicted(psych::principal(mtcars, 3))
  expect_identical(dim(x), c(32L, 3L))
  x <- get_predicted(psych::principal(mtcars, 3), data = mtcars[1:5, ])
  expect_identical(dim(x), c(5L, 3L))

  x <- get_predicted(prcomp(mtcars))
  expect_identical(dim(x), as.integer(c(32, ncol(mtcars))))
  x <- get_predicted(prcomp(mtcars), data = mtcars[1:5, ])
  expect_identical(dim(x), as.integer(c(5, ncol(mtcars))))

  # FA
  x <- get_predicted(psych::fa(mtcars, 3))
  expect_identical(dim(x), c(32L, 3L))
  x <- get_predicted(psych::fa(mtcars, 3), data = mtcars[1:5, ])
  expect_identical(dim(x), c(5L, 3L))

  expect_error(get_predicted(fungible::faMain(mtcars, numFactors = 3)))
  x <- get_predicted(fungible::faMain(mtcars, numFactors = 3), data = mtcars[1:5, ])
  expect_identical(dim(x), c(5L, 3L))
})


# arguments: `predict` vs. `type` -----------------------------------------
# =========================================================================

test_that("lm: get_predicted vs barebones `predict()`", {
  mod <- lm(mpg ~ hp + factor(cyl), mtcars)
  known <- predict(mod, se.fit = TRUE, interval = "confidence")
  unknown1 <- as.data.frame(get_predicted(mod, ci = 0.95))
  unknown2 <- as.data.frame(get_predicted(mod, ci = 0.95, predict = "expectation"))
  unknown3 <- suppressWarnings(as.data.frame(get_predicted(mod, ci = 0.95, predict = "response")))
  expect_equal(unknown1$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown1$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown1$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown1$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
  expect_equal(unknown2$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown2$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown2$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown2$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
  expect_equal(unknown3$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown3$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown3$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown3$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
})


test_that("using both `predict` and `type` raises an informative warning", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  expect_message(get_predicted(mod, predict = "response", type = "response"))
})


test_that("`predict` and `type` are both `NULL`", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  expect_error(get_predicted(mod, predict = NULL), regexp = "supply")
})


test_that("`predict()` vs. `get_predicted` link equivalence", {
  # link
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  known <- predict(mod, type = "link", interval = "confidence", se.fit = TRUE)
  unknown <- as.data.frame(get_predicted(mod, predict = NULL, type = "link", ci = 0.95))
  expect_equal(unname(known$fit), unknown$Predicted, ignore_attr = TRUE)
  expect_equal(unname(known$se.fit), unknown$SE, ignore_attr = TRUE)

  # response
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  known <- predict(mod, type = "response", se.fit = TRUE)
  unknown1 <- as.data.frame(get_predicted(mod, predict = "expectation", ci = 0.95))
  unknown2 <- as.data.frame(get_predicted(mod, predict = NULL, type = "response", ci = 0.95))
  unknown3 <- as.data.frame(get_predicted(mod, predict = "response", ci = 0.95))
  expect_equal(unname(known$fit), unknown1$Predicted, ignore_attr = TRUE)
  expect_equal(unname(known$se.fit), unknown1$SE, ignore_attr = TRUE)
  expect_equal(unname(known$fit), unknown2$Predicted, ignore_attr = TRUE)
  expect_equal(unname(known$se.fit), unknown2$SE, ignore_attr = TRUE)
  expect_equal(unname(known$fit), unknown3$Predicted, ignore_attr = TRUE)
  expect_equal(unname(known$se.fit), unknown3$SE, ignore_attr = TRUE)
})


test_that("hurdle: get_predicted matches `predict()`", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  mod <- pscl::hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
  known <- predict(mod, type = "response")
  unknown <- get_predicted(mod, predict = "response", ci = 0.95, verbose = FALSE)
  expect_equal(known, unknown, ignore_attr = TRUE)
  known <- predict(mod, type = "zero")
  unknown <- suppressWarnings(get_predicted(mod, predict = "zero", ci = 0.95, verbose = FALSE))
  expect_equal(known, unknown, ignore_attr = TRUE)
})


test_that("bugfix: used to return all zeros", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  pred <- get_predicted(mod, predict = "response", ci = 0.95, verbose = FALSE)
  expect_false(any(pred == 0))
  expect_error(expect_warning(get_predicted(mod, predict = "original", ci = 0.95, verbose = FALSE)))
})

# Original Error: "variables were specified with different types from the fit"
# Originates from using base R scale on dataframe (easystats/performance#432)
test_that("bugfix: used to fail with matrix variables", {
  # put model data in a separate environment, to ensure we retrieve the correct
  # data to fix its classes
  foo <- function() {
    mtcars2 <- mtcars
    mtcars2$wt <- scale(mtcars2$wt)
    lm(mpg ~ wt + cyl + gear + disp, data = mtcars2)
  }
  pred <- get_predicted(foo())
  expect_s3_class(pred, c("get_predicted", "numeric"))
  expect_true(all(attributes(attributes(attributes(
    pred
  )$data)$terms)$dataClasses == "numeric"))

  # Now verify with the data in the same environment
  mtcars2 <- mtcars
  mtcars2$wt <- scale(mtcars2$wt)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars2)
  expect_no_error({
    pred <- get_predicted(m)
  })

  mtcars2$wt <- as.numeric(mtcars2$wt)
  m2 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars2)
  pred2 <- get_predicted(m2)
  expect_equal(pred, pred2, ignore_attr = TRUE)
})


test_that("brms: `type` in ellipsis used to produce the wrong intervals", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_on_os(os = "windows")
  void <- capture.output(
    suppressMessages({
      mod <- brms::brm(am ~ hp + mpg,
        family = brms::bernoulli, data = mtcars,
        chains = 2, iter = 1000, seed = 1024, silent = 2
      )
    })
  )
  x <- get_predicted(mod, predict = "link", ci = 0.95)
  y <- get_predicted(mod, predict = "expectation", ci = 0.95)
  z <- get_predicted(mod, predict = NULL, type = "response", ci = 0.95, verbose = FALSE)
  expect_equal(round(x[1], 1), -1.5, tolerance = 1e-1)
  expect_equal(round(y[1], 1), 0.2, tolerance = 1e-1)
  expect_equal(y, z, ignore_attr = TRUE)

  data <- mtcars
  data$cyl <- as.character(data$cyl)
  void <- capture.output(
    suppressMessages(suppressWarnings({
      model <- brms::brm(cyl ~ mpg * vs + (1 | carb),
        data = data,
        iter = 1000,
        seed = 1024,
        algorithm = "meanfield",
        refresh = 0,
        family = brms::categorical(link = "logit", refcat = "4")
      )
    }))
  )
  x <- as.data.frame(get_predicted(model, ci = 0.95))
  # Test shape
  expect_identical(c(nrow(x), ncol(x)), c(96L, 1010L))
  # Test whether median point-estimate indeed different from default (mean)
  expect_gt(max(x$Predicted - get_predicted(model, centrality_function = stats::median)$Predicted), 0)
  # predictions include variables from data grid
  x <- get_predicted(model, ci = 0.95)
  expect_named(x, c("Row", "Response", "cyl", "mpg", "vs", "carb", "Predicted"))
})


test_that("zero-inflation stuff works", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("pscl")

  data(fish)
  m1 <- glmmTMB::glmmTMB(
    count ~ child + camper,
    ziformula = ~ child + camper,
    data = fish,
    family = poisson()
  )

  m2 <- pscl::zeroinfl(
    count ~ child + camper | child + camper,
    data = fish,
    dist = "poisson"
  )

  p1 <- head(predict(m1, type = "response"))
  p2 <- head(predict(m2, type = "response"))
  p3 <- head(get_predicted(m1))
  p4 <- head(get_predicted(m2))

  expect_equal(p1, p2, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p3, p4, tolerance = 1e-1, ignore_attr = TRUE)

  m1 <- glmmTMB::glmmTMB(
    count ~ child + camper,
    ziformula = ~ child + camper,
    data = fish,
    family = glmmTMB::truncated_poisson()
  )

  m2 <- pscl::hurdle(
    count ~ child + camper | child + camper,
    data = fish,
    dist = "poisson"
  )

  p1 <- head(predict(m1, type = "response"))
  p2 <- head(predict(m2, type = "response"))
  p3 <- head(get_predicted(m1))
  p4 <- head(get_predicted(m2))

  expect_equal(p1, p2, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p3, p4, tolerance = 1e-1, ignore_attr = TRUE)
})
