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

if (!osx && .runThisTest && requiet("testthat") && requiet("insight") && requiet("lme4")) {
  data("sleepstudy")
  data("Penicillin")
  set.seed(12345)
  sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$subgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$grp == i
    sleepstudy$subgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
  fm3 <- lmer(
    Reaction ~ Days + (1 + Days || grp / subgrp) + (1 + Days | Subject),
    data = sleepstudy
  )
  fm4 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  fm5 <- lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = sleepstudy
  )
  fm6 <- lmer(diameter ~ 0 + sample + (1 | plate), data = Penicillin)

  v1 <- suppressWarnings(get_variance(fm1))
  v2 <- suppressWarnings(get_variance(fm2))
  v3 <- suppressWarnings(get_variance(fm3))
  v4 <- suppressWarnings(get_variance(fm4))
  v5 <- suppressWarnings(get_variance(fm5))
  v6 <- suppressWarnings(get_variance(fm6))

  test_that("get_variance-1", {
    expect_equal(v1$var.intercept,
      c(Subject = 612.10016),
      tolerance = 1e-2
    )
    expect_equal(v1$var.slope,
      c(Subject.Days = 35.07171),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-2", {
    expect_equal(v2$var.intercept,
      c(Subject = 627.56905),
      tolerance = 1e-2
    )
    expect_equal(v2$var.slope,
      c(Subject.Days = 35.85838),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-3", {
    expect_equal(v3$var.intercept,
      c(subgrp.grp.1 = 0, Subject = 662.52047, grp.1 = 0),
      tolerance = 1e-2
    )
    expect_equal(v3$var.slope,
      c(Subject.Days = 34.25771, subgrp.grp.Days = 7.88485, grp.Days = 0),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-4", {
    expect_equal(v4$var.intercept,
      c(Subject = 1378.17851),
      tolerance = 1e-2
    )
    expect_null(v4$var.slope)
  })

  test_that("get_variance-5", {
    expect_equal(v5$var.intercept,
      c(`subgrp:grp` = 38.76069, Subject = 1377.50569, grp = 3.32031),
      tolerance = 1e-2
    )
    expect_null(v5$var.slope)
  })

  test_that("get_variance-6", {
    expect_equal(v6$var.intercept, c(plate = 0.71691), tolerance = 1e-2)
    expect_equal(v6$var.random, 0.71691, tolerance = 1e-2)
    expect_null(v6$var.slope)
  })
}
