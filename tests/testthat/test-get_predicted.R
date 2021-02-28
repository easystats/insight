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
}
