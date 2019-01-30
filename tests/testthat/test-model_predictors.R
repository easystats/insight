if (require("testthat") && require("insight") && require("lme4") && require("glmmTMB")) {
  context("insight, model_predictors")

  data(sleepstudy)

  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <- sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m1 <- lme4::lmer(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy
  )

  m2 <- lme4::lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  )

  test_that("model_predictors", {
    expect_equal(model_predictors(m1, effects = "all"), c("Days", "Subject"))
    expect_equal(model_predictors(m1, effects = "fixed"), "Days")
    expect_equal(model_predictors(m1, effects = "random"), "Subject")
    expect_equal(model_predictors(m2, effects = "all"), c("Days", "mygrp", "mysubgrp", "Subject"))
    expect_equal(model_predictors(m2, effects = "fixed"), "Days")
    expect_equal(model_predictors(m2, effects = "random"), c("mysubgrp:mygrp", "mygrp", "Subject"))
  })


  data(fish)

  m3 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + livebait + (1 | persons),
    data = fish,
    family = truncated_poisson()
  )

  test_that("model_predictors", {
    expect_identical(model_predictors(m3, effects = "fixed", zi = FALSE), c("child", "camper"))
    expect_identical(model_predictors(m3, effects = "fixed", zi = TRUE), c("child", "camper", "livebait"))
    expect_identical(model_predictors(m3, effects = "all", zi = FALSE), c("child", "camper", "persons"))
    expect_identical(model_predictors(m3, effects = "all", zi = TRUE), c("child", "camper", "persons", "livebait"))
    expect_identical(model_predictors(m3, effects = "random", zi = FALSE), "persons")
    expect_identical(model_predictors(m3, effects = "random", zi = TRUE), "persons")
  })

}

