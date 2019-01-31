if (require("testthat") && require("insight") && require("lme4")) {
  context("insight, find_predictors")

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

  test_that("find_predictors", {
    expect_equal(find_predictors(m1, effects = "all"), c("Days", "Subject"))
    expect_equal(find_predictors(m1, effects = "fixed"), "Days")
    expect_equal(find_predictors(m1, effects = "random"), "Subject")
    expect_equal(find_predictors(m2, effects = "all"), c("Days", "mygrp", "mysubgrp", "Subject"))
    expect_equal(find_predictors(m2, effects = "fixed"), "Days")
    expect_equal(find_predictors(m2, effects = "random"), c("mysubgrp:mygrp", "mygrp", "Subject"))
  })

  test_that("find_random", {
    expect_equal(find_random(m1), "Subject")
    expect_equal(find_random(m2), c("mysubgrp:mygrp", "mygrp", "Subject"))
    expect_equal(find_random(m2, split_nested = TRUE), c("mysubgrp", "mygrp", "Subject"))
    expect_equal(find_random(m1, component = "cond"), "Subject")
    expect_equal(find_random(m1, component = "all"), "Subject")
    expect_null(find_random(m1, component = "zi"))
    expect_null(find_random(m1, component = "disp"))
    expect_equal(find_random(m2, component = "cond"), c("mysubgrp:mygrp", "mygrp", "Subject"))
    expect_equal(find_random(m2, component = "cond", split_nested = TRUE), c("mysubgrp", "mygrp", "Subject"))
    expect_equal(find_random(m2, component = "all"), c("mysubgrp:mygrp", "mygrp", "Subject"))
    expect_equal(find_random(m2, component = "all", split_nested = TRUE), c("mysubgrp", "mygrp", "Subject"))
    expect_null(find_random(m2, component = "zi"))
    expect_null(find_random(m2, component = "zi", split_nested = TRUE))
    expect_null(find_random(m2, component = "disp"))
    expect_null(find_random(m2, component = "disp", split_nested = TRUE))
  })
}

