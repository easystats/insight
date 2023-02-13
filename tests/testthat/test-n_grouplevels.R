if (skip_if_not_or_load_if_installed("lme4")) {
  data(sleepstudy)
  d <- sleepstudy
  set.seed(12345)
  d$grp <- sample(1:5, size = 180, replace = TRUE)
  d$subgrp <- NA
  for (i in 1:5) {
    filter_group <- d$grp == i
    d$subgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }
  dd <<- d
  model <- lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = dd
  )

  test_that("n_grouplevels", {
    out <- n_grouplevels(model)
    expect_identical(out$Group, c("subgrp", "grp", "Subject", "subgrp:grp"))
    expect_identical(out$N_levels, c(30L, 5L, 18L, 108L))
  })
}
