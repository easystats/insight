if (requiet("lme4")) {
  data(sleepstudy)
  set.seed(12345)
  sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$subgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$grp == i
    sleepstudy$subgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  d <<- sleepstudy
  model <- lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = d
  )

  test_that("n_grouplevels", {
    out <- n_grouplevels(model)
    expect_identical(out$Group, c("subgrp", "grp", "Subject", "subgrp:grp"))
    expect_identical(out$N_levels, c(30L, 5L, 18L, 108L))
  })
}
