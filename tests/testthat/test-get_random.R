skip_on_os("mac")
skip_if_not_installed("lme4")
test_that("get_random works with missings", {
  data("sleepstudy", package = "lme4")
  sleepstudy$Days[1] <- NA
  fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  expect_identical(nrow(get_data(fm1)), length(get_random(fm1)[[1]]))
})
