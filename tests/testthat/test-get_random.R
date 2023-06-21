skip_on_os("mac")
skip_if_not_installed("lme4")
test_that("get_random works with missings", {
  data("sleepstudy", package = "lme4")
  sleepstudy$Days[1] <- NA
  fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  expect_identical(nrow(get_data(fm1)), length(get_random(fm1)[[1]]))

  set.seed(123)
  # prepare some data...
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }
  sleepstudy$Reaction[5] <- NA
  mmiss2 <- lme4::lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  )
  expect_identical(nrow(get_random(mmiss2)), 178L)
})
