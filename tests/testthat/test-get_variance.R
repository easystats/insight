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


  # further examples

  model <- lmer(Reaction ~ Days + (1 + Days || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-7", {
    expect_equal(
      vmodel,
      list(var.fixed = 908.95336, var.random = 627.56905, var.residual = 653.5835,
           var.distribution = 653.5835, var.dispersion = 0, var.intercept = c(Subject = 627.56905),
           var.slope = c(Subject.Days = 35.85838)),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days + (0 + Days || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-8", {
    expect_equal(
      vmodel,
      list(var.fixed = 908.95336, var.random = 1502.179, var.residual = 842.02962,
           var.distribution = 842.02962, var.dispersion = 0, var.slope = c(Subject.Days = 52.70804)),
      tolerance = 1e-2
    )
  })


  # categorical rnd slope

  data("sleepstudy")
  sleepstudy$Days2 <- cut(sleepstudy$Days, breaks = c(-1, 3 ,6, 10))

  model <- lmer(Reaction ~ Days2 + (1 + Days2 | Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-9", {
    expect_equal(
      vmodel,
      list(var.fixed = 807.08545, var.random = 1711.44396, var.residual = 748.81107,
           var.distribution = 748.81107, var.dispersion = 0, var.intercept = c(Subject = 663.28042),
           var.slope = c(`Subject.Days2(3,6]` = 882.36419, `Subject.Days2(6,10]` = 1415.70768),
           cor.slope_intercept = structure(c(0.36117, 0.33188), .Dim = 2:1, .Dimnames = list(
             c("Days2(3,6]", "Days2(6,10]"), "Subject"))),
      tolerance = 1e-2
    )
  })

  model <- suppressWarnings(lmer(Reaction ~ Days2 + (1 + Days2 || Subject), data = sleepstudy))
  vmodel <- suppressWarnings(get_variance(model))

  test_that("get_variance-10", {
    expect_equal(
      vmodel,
      list(var.fixed = 807.08545355676, var.residual = 740.875581179784,
           var.distribution = 740.875581179784, var.dispersion = 0,
           var.intercept = c(Subject = 738.635155172211),
           var.slope = c(`Subject.Days2(-1,3]` = 0,
                         `Subject.Days2(3,6]` = 994.015865559888,
                         `Subject.Days2(6,10]` = 1545.72576115283),
           cor.slope_intercept = c(`Subject.1.Days2(3,6]` = NaN, `Subject.1.Days2(6,10]` = NaN)),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days2 + (0 + Days2 | Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-11", {
    expect_equal(
      vmodel,
      list(var.fixed = 807.08545, var.random = 1446.13555, var.residual = 748.81386,
           var.distribution = 748.81386, var.dispersion = 0, var.slope = c(`Subject.Days2(-1,3]` = 663.27446,
                                                                           `Subject.Days2(3,6]` = 2098.24692, `Subject.Days2(6,10]` = 2722.20492
           ), cor.slope_intercept = structure(c(0.79645, 0.73296), .Dim = 2:1, .Dimnames = list(
             c("Days2(3,6]", "Days2(6,10]"), "Subject"))),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days2 + (0 + Days2 || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-12", {
    expect_equal(
      vmodel,
      list(var.fixed = 807.08545, var.random = 1446.13555, var.residual = 748.81386,
           var.distribution = 748.81386, var.dispersion = 0, var.slope = c(`Subject.Days2(-1,3]` = 663.27446,
                                                                           `Subject.Days2(3,6]` = 2098.24692, `Subject.Days2(6,10]` = 2722.20492
           ), cor.slope_intercept = structure(c(0.79645, 0.73296), .Dim = 2:1, .Dimnames = list(
             c("Days2(3,6]", "Days2(6,10]"), "Subject"))),
      tolerance = 1e-2
    )
  })
}
