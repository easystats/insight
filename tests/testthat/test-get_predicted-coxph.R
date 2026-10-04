skip_if_not_installed("survival")

lung <- subset(survival::lung, subset = ph.ecog %in% 0:2)
lung$sex <- factor(lung$sex)
Surv <- survival::Surv
m <- survival::coxph(Surv(time, status) ~ sex + age, data = lung)
nd <- data.frame(
  time = c(200, 400, 600),
  status = 1,
  sex = factor(c(1, 2, 1), levels = 1:2),
  age = c(60, 60, 70)
)


test_that("get_predicted, coxph, survival CIs match survfit()", {
  out <- as.data.frame(get_predicted(m, data = nd, predict = "survival", ci = 0.95))
  for (i in seq_len(nrow(nd))) {
    sf <- summary(survival::survfit(m, newdata = nd[i, ]), times = nd$time[i])
    expect_equal(out$Predicted[i], sf$surv, tolerance = 1e-6)
    expect_equal(out$SE[i], sf$std.err, tolerance = 1e-6)
    expect_equal(out$CI_low[i], sf$lower, tolerance = 1e-4)
    expect_equal(out$CI_high[i], sf$upper, tolerance = 1e-4)
  }
  expect_true(all(out$CI_low > 0 & out$CI_high < 1))
})


test_that("get_predicted, coxph, expectation returns SE without warning", {
  expect_silent({
    out <- as.data.frame(get_predicted(m, data = nd, predict = "expectation", ci = 0.95))
  })
  lp <- predict(m, newdata = nd, type = "lp", se.fit = TRUE)
  z <- stats::qnorm(0.975)
  expect_equal(out$Predicted, as.vector(exp(lp$fit)), tolerance = 1e-6)
  expect_equal(out$SE, as.vector(exp(lp$fit) * lp$se.fit), tolerance = 1e-6)
  expect_equal(out$CI_low, as.vector(exp(lp$fit - z * lp$se.fit)), tolerance = 1e-4)
  expect_equal(out$CI_high, as.vector(exp(lp$fit + z * lp$se.fit)), tolerance = 1e-4)

  expect_silent(get_predicted(m, data = nd, predict = "risk", ci = 0.95))
})
