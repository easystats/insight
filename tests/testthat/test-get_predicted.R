if (require("testthat") && require("insight") && require("lme4") && require("glmmTMB") && require("mgcv") && require("gamm4") && require("rstanarm")) {
  data(mtcars)


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


  test_that("get_predicted - data", {
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

  test_that("get_predicted - lmerMod", {
    x <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)
    rez <- insight::get_predicted(x)
    expect_equal(length(rez), 32)

    expect_equal(max(abs(rez - stats::fitted(x))), 0)
    expect_equal(max(abs(rez - stats::predict(x))), 0)
    expect_equal(nrow(as.data.frame(rez)), 32)
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
  })

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

  test_that("get_predicted - rstanarm (lm)", {
    x <- rstanarm::stan_glm(mpg ~ am, data = mtcars, iter = 200, refresh = 0, seed = 333)
    rez <- insight::get_predicted(x)
    expect_equal(nrow(rez), 32)

    df <- as.data.frame(rez)
    expect_equal(nrow(df), 32)

    # TODO: Not sure what fitted.stanreg and predict.stanreg do as they return different point-estimates

    # sapply(as.data.frame(t(df)), median)
    # expect_equal(max(abs(rowMeans(rez) - stats::fitted(x))), 0)
    # expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
  })
}
