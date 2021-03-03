osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && !osx && require("testthat") && require("insight") && require("lme4") && require("glmmTMB") && require("mgcv") && require("gamm4") && require("rstanarm") && require("merTools") && require("emmeans") && require("bayestestR") && require("mclust") && require("rstantools")) {
  data(mtcars)



  # LM and GLM --------------------------------------------------------------
  # =========================================================================

  test_that("get_predicted - lm", {
    x <- lm(mpg ~ cyl + hp, data = mtcars)

    # General
    rezlink <- get_predicted_new(x, type = "link")
    rezpred <- get_predicted_new(x, type = "prediction")
    rezresp <- get_predicted_new(x, type = "response")
    expect_equal(mean(abs(rezlink - rezpred)), 0, tolerance = 1e-3)
    expect_equal(mean(abs(rezresp - rezpred)), 0, tolerance = 1e-3)
    expect_equal(mean(abs(attributes(rezpred)$CI_high - attributes(rezresp)$CI_high)), 0, tolerance = 1e-3)
    expect_true(all(mean(attributes(rezlink)$CI_high - attributes(rezresp)$CI_high) < 0))

    # Confidence
    ref <- predict(x, se.fit = TRUE, interval = "confidence")
    rez <- as.data.frame(get_predicted_new(x, type = "link"))
    expect_equal(nrow(rez), 32)
    expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
    expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-10)
    expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

    # Prediction
    ref <- predict(x, newdata = insight::get_data(x), se.fit = TRUE, interval = "prediction")
    rez <- as.data.frame(get_predicted_new(x, type = "response"))
    expect_equal(nrow(rez), 32)
    expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
    expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

    # Bootstrap
    set.seed(333)
    ref <- predict(x, newdata = insight::get_data(x), se.fit = TRUE, interval = "confidence")
    rez <- get_predicted_new(x, iterations = 600)
    expect_equal(nrow(rez), 32)
    expect_equal(mean(abs(as.data.frame(ref$fit)$fit - attributes(rez)$Predicted)), 0, tolerance = 0.1)
    expect_equal(mean(abs(as.data.frame(ref$fit)$lwr - attributes(rez)$CI_low)), 0, tolerance = 0.5)
    # Is it possible to get "prediction" CIs via bootstrapping?

    # vs. Bayesian
    xbayes <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
    rez <- as.data.frame(get_predicted_new(x, type = "link"))
    rezbayes <- summary(get_predicted_new(xbayes, type = "link"))
    expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
    expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.1)
    rez <- as.data.frame(get_predicted_new(x, type = "response"))
    rezbayes <- summary(get_predicted_new(xbayes, type = "response"))
    expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.2)
    expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.2)
  })

  test_that("get_predicted - glm", {
    x <- glm(vs ~ wt, data = mtcars, family = "binomial")
    # ciTools::add_ci(mtcars, x)

    # General
    rezlink <- get_predicted_new(x, type = "link")
    rezlink2 <- get_predicted_new(x, type = "link", scale = "link")
    rezpred <- get_predicted_new(x, type = "prediction")
    rezresp <- get_predicted_new(x, type = "response")
    expect_equal(mean(abs(rezlink - rezpred)), 0, tolerance = 1e-3)
    expect_true(all(unique(rezresp) %in% c(0, 1)))
    expect_true(all(mean(attributes(rezlink)$CI_high - attributes(rezresp)$CI_high) < 0))
    expect_true(min(rezlink2) < 0)

    # Confidence
    ref <- predict(x, se.fit = TRUE, type = "response")
    rez <- as.data.frame(get_predicted_new(x, type = "prediction"))
    expect_equal(nrow(rez), 32)
    expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-10)
    # TODO: this should work
    # expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-10)

    # Prediction
    ref <- predict(x, se.fit = TRUE, type = "response")
    rez <- as.data.frame(get_predicted_new(x, type = "link"))
    expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-10)

    # Comparison with results from predict.lm (Mattan's method for CI)
    ref <- as.data.frame(suppressWarnings(insight::link_inverse(x)(predict.lm(x, interval = "prediction"))))
    expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-10)
    # TODO: I think our current CIs for logistic models are wrong
    # expect_equal(max(abs(ref$lwr - rez$CI_low)), 0, tolerance = 1e-10)
    ref <- as.data.frame(insight::link_inverse(x)(predict.lm(x, interval = "confidence")))
    expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-10)
    # TODO: The CI also doesn't match
    # expect_equal(max(abs(ref$lwr - rez$CI_low)), 0, tolerance = 1e-10)

    # Bootstrap
    ref <- predict(x, se.fit = TRUE, predict = "response")
    rez <- summary(get_predicted_new(x, iterations = 800))
    # expect_equal(mean(abs(ref$fit - rez$Predicted)), 0, tolerance = 0.5)

    # vs. Bayesian
    xbayes <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0, seed = 333)
    rez <- as.data.frame(get_predicted_new(x, type = "link"))
    rezbayes <- summary(get_predicted_new(xbayes, type = "link"))
    expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.3)
    expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.3)
    rez <- as.data.frame(get_predicted_new(x, type = "response"))
    rezbayes <- summary(get_predicted_new(xbayes, type = "link"))
    expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.3)
    # expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.4)
  })


  # Mixed --------------------------------------------------------------
  # =========================================================================
  # test_that("get_predicted - merMod", {
  #   x <- lme4::glmer(vs ~ wt + (1|am), data = mtcars, family = "binomial")
  #   x <- glmmTMB::glmmTMB(vs ~ wt + (1|am), data = mtcars, family = "binomial")
  #   predict(x, type = "link")
  # })


  # Bayesian --------------------------------------------------------------
  # =========================================================================

  test_that("get_predicted - rstanarm", {
    set.seed(333)
    # LM
    x <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
    rezlink <- summary(get_predicted_new(x, type = "link"))
    rezpred <- summary(get_predicted_new(x, type = "prediction"))
    rezresp <- summary(get_predicted_new(x, type = "response"))
    expect_equal(mean(abs(rezlink$Predicted - rezpred$Predicted)), 0, tolerance = 0.2)
    expect_equal(mean(abs(rezresp$Predicted - rezpred$Predicted)), 0, tolerance = 0.2)
    expect_true(all(mean(rezlink$CI_high - rezpred$CI_high) < 0))
    expect_equal(mean(abs(rezpred$CI_high - rezresp$CI_high)), 0, tolerance = 0.5)



    # GLM
    x <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0)
    rezlink <- summary(get_predicted_new(x, type = "link"))
    rezlink2 <- summary(get_predicted_new(x, type = "link", scale = "link"))
    rezpred <- summary(get_predicted_new(x, type = "prediction"))
    rezresp <- summary(get_predicted_new(x, type = "response"))
    expect_equal(mean(abs(rezlink$Predicted - rezpred$Predicted)), 0, tolerance = 0.2)
    expect_equal(mean(abs(rezresp$Predicted - rezpred$Predicted)), 0, tolerance = 0.2)
    expect_true(all(mean(rezlink$CI_high - rezpred$CI_high) < 0))
    expect_equal(mean(abs(rezpred$CI_high - rezresp$CI_high)), 0, tolerance = 0.1)
  })
}
