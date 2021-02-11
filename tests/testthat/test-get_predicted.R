osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})


if (!osx && require("testthat") && require("insight") && require("lme4") && require("glmmTMB") && require("mgcv") && require("gamm4") && require("rstanarm") && require("merTools") && require("emmeans") && require("bayestestR")) {
  data(mtcars)



# Basic -------------------------------------------------------------------


  test_that("get_predicted - lm", {
    x <- lm(mpg ~ am, data = mtcars)
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x))), 0)

    data <- as.data.frame(rez)
    expect_equal(max(abs(rez - data$Predicted)), 0)
    expect_equal(nrow(data), 32)
  })


  test_that("get_predicted - data first", {
    set.seed(333)
    m <- lm(mpg ~ am, data = mtcars)
    newdata <- data.frame(am = sample(c(0, 1), replace = TRUE, size = 20))

    expect_equal(length(get_predicted(m, newdata)), 20)
    expect_equal(length(get_predicted(newdata, m)), 20)
  })

  test_that("get_predicted - glm", {
    x <- glm(vs ~ am, data = mtcars, family = "binomial")
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)
  })


# Mixed Models ------------------------------------------------------------


  test_that("get_predicted - lmerMod", {
    x <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)

    # No random
    rez <- insight::get_predicted(x, newdata = mtcars[c("am")])
    expect_true(!all(is.na(as.data.frame(rez))))

    # Bootstrapped
    set.seed(333)
    rez_pred <- insight::get_predicted(x, ci_type = "prediction", iter = 500)
    rez_boot <- insight::get_predicted(x, bootstrap = TRUE, iter = 500)
    # Such a big discrepancy!
    # expect_equal(mean(as.data.frame(rez_boot)$CI_low - as.data.frame(rez_pred)$CI_low), 0, tol = 0.001)

    # Compare to merTools
    rez_merTools <- merTools::predictInterval(x, level = 0.95, seed = 333, n.sims = 2000)
    # expect_equal(mean(as.data.frame(rez)$CI_low - rez_merTools$lwr), 0, tolerance = 0.5)
    # expect_equal(mean(as.data.frame(rez_boot)$CI_low - rez_merTools$lwr), 0, tolerance = 0.001)


    # Compare to emmeans (not sure what it does)
    refgrid <- emmeans::ref_grid(x, at = as.list(get_data(x)), data = get_data(x))
    rez_emmeans <- as.data.frame(predict(refgrid, level = 0.95, interval = "prediction"))
    # This is completely off
    # expect_equal(mean(as.data.frame(rez)$CI_low - rez_emmeans$lower.PL), 0, tolerance = 0.5)

    # Compare with rstanarm
    x2 <- rstanarm::stan_lmer(mpg ~ am + (1 | cyl), data = mtcars, refresh=0, iter=1000, seed=333)
    rez_stan <- as.data.frame(t(insight::get_predicted(x2)))
    # expect_equal(max(abs(rez - sapply(rez_stan, median))), 0, tolerance = 0.5)
    # Different indeed
    # expect_equal(mean(as.data.frame(rez)$CI_low - bayestestR::hdi(rez_stan)$CI_low), 0, tolerance = 0.5)
  })

  test_that("get_predicted - merMod", {
    x <- lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)

  })

  test_that("get_predicted - glmmTMB", {
    x <- glmmTMB::glmmTMB(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)

    # Compare to lme4
    x2 <- lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
    rez2 <- insight::get_predicted(x2)
    expect_equal(max(abs(rez - rez2)), 0, tolerance = 1e-5)
    # expect_equal(max(abs(as.data.frame(rez)$SE - as.data.frame(rez2)$SE)), 0, tolerance = 1e-5)
    # expect_equal(max(abs(as.data.frame(rez)$CI_low - as.data.frame(rez2)$CI_low)), 0, tolerance = 1e-5)
    # rez_boot <- insight::get_predicted(x2, bootstrap = TRUE, iter = 500)
    # expect_equal(max(abs(as.data.frame(rez)$CI_low - as.data.frame(rez_boot)$CI_low)), 0, tolerance = 1e-5)

    # No random
    rez <- insight::get_predicted(x, newdata = mtcars[c("am")])
    expect_true(!all(is.na(as.data.frame(rez))))
  })


# GAMs --------------------------------------------------------------------


  test_that("get_predicted - mgcv::gam and gamm", {
    x <- mgcv::gam(vs ~ am + s(wt), data = mtcars, family = "binomial")
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)

    x <- mgcv::gamm(vs ~ am + s(wt), random = list(cyl = ~1), data = mtcars, family = "binomial", verbosePQL = FALSE)
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - x$gam$fitted.values)), 0)
    expect_equal(max(abs(rez - stats::predict(x$gam, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)
  })


  test_that("get_predicted - gamm4::gamm4", {
    x <- gamm4::gamm4(vs ~ am + s(wt), random = ~ (1 | cyl), data = mtcars, family = "binomial")
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x$gam))), 0)
    expect_equal(max(abs(rez - stats::predict(x$gam, type = "response"))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)
  })


# Bayesian ----------------------------------------------------------------

  test_that("get_predicted - rstanarm (lm)", {
    x <- suppressWarnings(rstanarm::stan_glm(mpg ~ am, data = mtcars, iter = 500, refresh = 0, seed = 333))
    rez <- insight::get_predicted(x)
    expect_equal(nrow(rez), 32)

    df <- as.data.frame(rez)
    expect_equal(nrow(df), 32)

    df <- as.matrix(rez)
    expect_equal(nrow(df), 32)

    # Convert to dataframe
    rez <- as.data.frame(t(rez))

    # What does fitted() return????
    # expect_equal(max(abs(sapply(rez_stan, median) - stats::fitted(x))), 0, tolerance = 0.5)

    # Compare to lm
    xref <- as.data.frame(insight::get_predicted(lm(mpg ~ am, data = mtcars)))
    expect_equal(max(abs(sapply(rez, median) - xref$Predicted)), 0, tolerance = 0.1)

    # expect_equal(max(abs(bayestestR::hdi(rez)$CI_low - xref$CI_low)), 0, tolerance = 0.1)
  })
}
