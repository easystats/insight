skip_if_not_installed("nlme")
skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data(Orthodont, package = "nlme")
data(Ovary, package = "nlme")

m1 <- nlme::lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)

m2 <- nlme::lme(distance ~ age + Sex, data = Orthodont, random = ~1)

set.seed(123)
sleepstudy$mygrp <- sample.int(5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <-
    sample.int(30, size = sum(filter_group), replace = TRUE)
}

m3 <- nlme::lme(Reaction ~ Days, random = ~ 1 | mygrp / mysubgrp, data = sleepstudy)

# from easystats/insight/482
cr <<- nlme::corAR1(form = ~ 1 | Mare)
m4 <- nlme::lme(follicles ~ Time, Ovary, correlation = cr)

test_that("nested_varCorr", {
  skip_on_cran()

  # variances, not standard deviations, and the outer group first
  expect_equal(
    insight:::.get_nested_lme_varcorr(m3)$mygrp[1, 1],
    56.37473,
    tolerance = 1e-3
  )
  expect_equal(
    insight:::.get_nested_lme_varcorr(m3)$mysubgrp[1, 1],
    2.400317e-05,
    tolerance = 1e-2
  )
})


test_that("get_variance, nested lme, easystats/insight#1232", {
  skip_on_cran()
  data(Pixel, package = "nlme")

  m <- nlme::lme(pixel ~ day, random = ~ 1 | Dog / Side, data = Pixel)
  vc <- insight:::.get_nested_lme_varcorr(m)
  expect_named(vc, c("Dog", "Side"))
  expect_equal(vc$Dog[1, 1], 647.3259, tolerance = 1e-4)
  expect_equal(vc$Side[1, 1], 218.3414, tolerance = 1e-4)

  # same model fitted with lme4
  m_lme4 <- lme4::lmer(pixel ~ day + (1 | Dog / Side), data = Pixel)
  v <- get_variance(m)
  v_lme4 <- get_variance(m_lme4)
  expect_equal(v$var.random, v_lme4$var.random, tolerance = 1e-3)
  expect_equal(
    v$var.intercept,
    c(Dog = 647.3259, Side = 218.3414),
    tolerance = 1e-4
  )

  # random slopes: variance on the diagonal, covariance off the diagonal
  m_slope <- nlme::lme(pixel ~ day, random = ~ day | Dog / Side, data = Pixel)
  vc_nlme <- nlme::VarCorr(m_slope)
  vc <- insight:::.get_nested_lme_varcorr(m_slope)
  expect_named(vc, c("Dog", "Side"))
  expect_equal(
    diag(vc$Dog),
    as.numeric(vc_nlme[2:3, "Variance"]),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    vc$Dog[1, 2],
    prod(as.numeric(vc_nlme[2:3, "StdDev"])) * as.numeric(vc_nlme[3, "Corr"]),
    tolerance = 1e-4
  )

  # uncorrelated random slopes: zero covariance
  m_diag <- nlme::lme(
    pixel ~ day,
    random = list(Dog = nlme::pdDiag(~day), Side = ~1),
    data = Pixel
  )
  vc_nlme <- nlme::VarCorr(m_diag)
  vc <- insight:::.get_nested_lme_varcorr(m_diag)
  expect_equal(
    vc$Dog,
    diag(as.numeric(vc_nlme[2:3, "Variance"])),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(vc$Side[1, 1], as.numeric(vc_nlme[5, "Variance"]), tolerance = 1e-4)
})


test_that("nested lme, three correlated random terms", {
  skip_on_cran()
  data(Pixel, package = "nlme")

  m <- nlme::lme(
    pixel ~ day,
    random = ~ day + I(day^2) | Dog / Side,
    data = Pixel,
    control = nlme::lmeControl(opt = "optim")
  )
  vc <- insight:::.get_nested_lme_varcorr(m)
  # covariance matrices from the fitted model, scaled by the residual variance
  vc_nlme <- lapply(
    nlme::pdMatrix(m$modelStruct$reStruct),
    function(i) i * m$sigma^2
  )
  # VarCorr() rounds the correlations to three decimals
  expect_equal(vc$Dog, vc_nlme$Dog, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(vc$Side, vc_nlme$Side, ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("get_variance, nested lme, block without an intercept", {
  skip_on_cran()
  data(Pixel, package = "nlme")

  # random slope without intercept on both levels
  m <- nlme::lme(pixel ~ day, random = ~ 0 + day | Dog / Side, data = Pixel)
  m_lme4 <- lme4::lmer(pixel ~ day + (0 + day | Dog / Side), data = Pixel)
  v <- get_variance(m)
  v_lme4 <- get_variance(m_lme4)
  expect_type(v, "list")
  expect_equal(v$var.fixed, v_lme4$var.fixed, tolerance = 1e-3)
  expect_equal(v$var.random, v_lme4$var.random, tolerance = 1e-3)
  expect_equal(v$var.residual, v_lme4$var.residual, tolerance = 1e-3)

  # random intercept on the outer level, random slope without intercept on
  # the inner level
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = ~1, Side = ~ 0 + day),
    data = Pixel
  )
  m_lme4 <- lme4::lmer(
    pixel ~ day + (1 | Dog) + (0 + day | Dog:Side),
    data = Pixel
  )
  v <- get_variance(m)
  v_lme4 <- get_variance(m_lme4)
  expect_type(v, "list")
  expect_equal(v$var.fixed, v_lme4$var.fixed, tolerance = 1e-3)
  expect_equal(v$var.random, v_lme4$var.random, tolerance = 1e-3)
  expect_equal(v$var.residual, v_lme4$var.residual, tolerance = 1e-3)
})


test_that("get_variance, lme, random slopes of blocks without an intercept", {
  skip_on_cran()
  data(Pixel, package = "nlme")
  # variances from the "Variance" column of VarCorr(), by row number
  vc_variance <- function(model, rows) {
    as.numeric(nlme::VarCorr(model)[rows, "Variance"])
  }

  # nested, random slope without intercept on both levels
  # VarCorr() rows: Dog =, day, Side =, day, Residual
  m <- nlme::lme(pixel ~ day, random = ~ 0 + day | Dog / Side, data = Pixel)
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, c(2, 4)), c("Dog.day", "Side.day")),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # nested, random intercept on the outer level, random slope without
  # intercept on the inner level
  # VarCorr() rows: Dog =, (Intercept), Side =, day, Residual
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = ~1, Side = ~ 0 + day),
    data = Pixel
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 4), "Side.day"),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # nested, two uncorrelated random slopes without intercept on the outer
  # level, random intercept on the inner level
  # VarCorr() rows: Dog =, day, I(day^2), Side =, (Intercept), Residual
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = nlme::pdDiag(~ 0 + day + I(day^2)), Side = ~1),
    data = Pixel
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 2:3), c("Dog.day", "Dog.I(day^2)")),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # not nested, random slope without intercept
  # VarCorr() rows: day, Residual
  m <- nlme::lme(pixel ~ day, random = ~ 0 + day | Dog, data = Pixel)
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 1), "Dog.day"),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # nested, two correlated random slopes without intercept on both levels
  # VarCorr() rows: Dog =, day, I(day^2), Side =, day, I(day^2), Residual
  m <- nlme::lme(
    pixel ~ day,
    random = ~ 0 + day + I(day^2) | Dog / Side,
    data = Pixel,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(
      vc_variance(m, c(2, 3, 5, 6)),
      c("Dog.day", "Dog.I(day^2)", "Side.day", "Side.I(day^2)")
    ),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # not nested, two correlated random slopes without intercept
  # VarCorr() rows: day, I(day^2), Residual
  m <- nlme::lme(
    pixel ~ day,
    random = ~ 0 + day + I(day^2) | Dog,
    data = Pixel,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 1:2), c("Dog.day", "Dog.I(day^2)")),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # nested, two correlated random slopes without intercept on the outer
  # level, random intercept on the inner level
  # VarCorr() rows: Dog =, day, I(day^2), Side =, (Intercept), Residual
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = ~ 0 + day + I(day^2), Side = ~1),
    data = Pixel,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 2:3), c("Dog.day", "Dog.I(day^2)")),
    tolerance = 1e-4
  )
  expect_null(v$cor.slope_intercept)

  # nested, correlated random intercept and slope on the outer level, random
  # slope without intercept on the inner level
  # VarCorr() rows: Dog =, (Intercept), day, Side =, day, Residual
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = ~day, Side = ~ 0 + day),
    data = Pixel,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, c(3, 5)), c("Dog.day", "Side.day")),
    tolerance = 1e-4
  )
  expect_equal(
    v$cor.slope_intercept,
    c(Dog = as.numeric(nlme::VarCorr(m)[3, "Corr"])),
    tolerance = 1e-4
  )

  # nested, the intercept is the second term of the outer block, so the
  # random slope is the first term
  # VarCorr() rows: Dog =, day, (Intercept), Side =, (Intercept), Residual
  m <- nlme::lme(
    pixel ~ day,
    random = list(
      Dog = nlme::pdBlocked(list(nlme::pdIdent(~ day - 1), ~1)),
      Side = ~1
    ),
    data = Pixel
  )
  v <- get_variance(m)
  expect_equal(
    v$var.slope,
    stats::setNames(vc_variance(m, 2), "Dog.day"),
    tolerance = 1e-4
  )
  expect_equal(
    v$var.intercept,
    stats::setNames(vc_variance(m, c(3, 5)), c("Dog", "Side")),
    tolerance = 1e-4
  )
})


test_that("get_variance, lme with an intercept in every block, unchanged", {
  skip_on_cran()
  data(Pixel, package = "nlme")

  # expected values are the results of insight 1.5.4.19 (commit 434d09c08)

  # one grouping factor, correlated intercept and slope
  m <- nlme::lme(pixel ~ day, random = ~ day | Dog, data = Pixel)
  expect_equal(
    get_variance(m),
    list(
      var.fixed = 0.02115043,
      var.random = 773.6784,
      var.residual = 307.939,
      var.distribution = 307.939,
      var.dispersion = 0,
      var.intercept = c(Dog = 1032.062),
      var.slope = c(Dog.day = 0.7841284),
      cor.slope_intercept = c(Dog = -0.755)
    ),
    tolerance = 1e-4
  )

  # nested grouping factors, intercepts only
  m <- nlme::lme(pixel ~ day, random = ~ 1 | Dog / Side, data = Pixel)
  expect_equal(
    get_variance(m),
    list(
      var.fixed = 2.352231,
      var.random = 865.6673,
      var.residual = 233.3453,
      var.distribution = 233.3453,
      var.dispersion = 0,
      var.intercept = c(Dog = 647.3259, Side = 218.3414)
    ),
    tolerance = 1e-4
  )

  # nested grouping factors, correlated intercept and slope on both levels
  m <- nlme::lme(pixel ~ day, random = ~ day | Dog / Side, data = Pixel)
  expect_equal(
    get_variance(m),
    list(
      var.fixed = 0.5543973,
      var.random = 915.051,
      var.residual = 211.2343,
      var.distribution = 211.2343,
      var.dispersion = 0,
      var.intercept = c(Dog = 991.8458, Side = 227.7047),
      var.slope = c(Dog.day = 1.149103, Side.day = 6.278051e-10),
      cor.slope_intercept = c(Dog = -0.786, Side = 0)
    ),
    tolerance = 1e-4
  )

  # nested grouping factors, uncorrelated intercept and slope on the outer level
  m <- nlme::lme(
    pixel ~ day,
    random = list(Dog = nlme::pdDiag(~day), Side = ~1),
    data = Pixel
  )
  expect_equal(
    get_variance(m),
    list(
      var.fixed = 0.5967164,
      var.random = 957.5163,
      var.residual = 221.8694,
      var.distribution = 221.8694,
      var.dispersion = 0,
      var.intercept = c(Dog = 703.421, Side = 223.2052),
      var.slope = c(Dog.day = 0.3816367)
    ),
    tolerance = 1e-4
  )

  # nested grouping factors, block-diagonal matrix on the outer level
  m <- nlme::lme(
    pixel ~ day,
    random = list(
      Dog = nlme::pdBlocked(list(~1, nlme::pdIdent(~ day - 1))),
      Side = ~1
    ),
    data = Pixel
  )
  expect_equal(
    get_variance(m),
    list(
      var.fixed = 0.5967557,
      var.random = 957.4883,
      var.residual = 221.8708,
      var.distribution = 221.8708,
      var.dispersion = 0,
      var.intercept = c(Dog = 703.3994, Side = 223.1996),
      var.slope = c(Dog.day = 0.381627)
    ),
    tolerance = 1e-4
  )
})


# correlations between random slopes (cor.slopes) ----------------------------

# correlation of two terms in a block, from the fitted model
pd_cor <- function(model, block, term1, term2) {
  stats::cov2cor(nlme::pdMatrix(model$modelStruct$reStruct)[[block]])[term1, term2]
}

# nested data with random intercepts and two correlated random slopes on both
# levels, 15 groups with 6 subgroups each
sim_slopecor_data <- function() {
  set.seed(2)
  d <- expand.grid(obs = 1:10, sub = 1:6, grp = 1:15)
  d$sub <- factor(paste(d$grp, d$sub, sep = "_"))
  d$grp <- factor(d$grp)
  d$x1 <- stats::rnorm(nrow(d))
  d$x2 <- stats::rnorm(nrow(d))
  L <- chol(matrix(c(1, 0.3, -0.2, 0.3, 1, 0.5, -0.2, 0.5, 1), 3))
  b_grp <- 1.5 * matrix(stats::rnorm(15 * 3), ncol = 3) %*% L
  b_sub <- matrix(stats::rnorm(nlevels(d$sub) * 3), ncol = 3) %*% L
  g <- as.integer(d$grp)
  s <- as.integer(d$sub)
  d$y <- 1 +
    d$x1 +
    d$x2 +
    b_grp[g, 1] +
    b_grp[g, 2] * d$x1 +
    b_grp[g, 3] * d$x2 +
    b_sub[s, 1] +
    b_sub[s, 2] * d$x1 +
    b_sub[s, 3] * d$x2 +
    stats::rnorm(nrow(d))
  d
}


test_that("get_variance, lme, cor.slopes", {
  skip_on_cran()
  sleep_slopecor <- sleepstudy
  sleep_slopecor$D2 <- sleep_slopecor$Days^2 / 10

  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = ~ Days + D2 | Subject,
    data = sleep_slopecor,
    control = nlme::lmeControl(opt = "optim")
  )
  m_lme4 <- lme4::lmer(Reaction ~ Days + D2 + (Days + D2 | Subject), data = sleep_slopecor)
  v <- get_variance(m)
  v_lme4 <- get_variance(m_lme4)

  expect_named(v$cor.slopes, "Subject.Days-D2")
  expect_named(v_lme4$cor.slopes, "Subject.Days-D2")
  expect_lte(abs(v$cor.slopes - v_lme4$cor.slopes), 0.01)
  expect_lte(abs(v$cor.slopes - pd_cor(m, "Subject", "Days", "D2")), 1e-6)

  # pdSymm, another general covariance block
  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = list(Subject = nlme::pdSymm(~ Days + D2)),
    data = sleep_slopecor,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_named(v$cor.slopes, "Subject.Days-D2")
  expect_lte(abs(v$cor.slopes - pd_cor(m, "Subject", "Days", "D2")), 1e-6)
})


test_that("get_variance, nested lme, cor.slopes", {
  skip_on_cran()
  d_slopecor <- sim_slopecor_data()

  expect_no_warning({
    m <- nlme::lme(y ~ x1 + x2, random = ~ x1 + x2 | grp / sub, data = d_slopecor)
  })
  m_lme4 <- lme4::lmer(y ~ x1 + x2 + (x1 + x2 | grp / sub), data = d_slopecor)
  expect_false(lme4::isSingular(m_lme4))
  v <- get_variance(m)
  v_lme4 <- get_variance(m_lme4)

  expect_named(v$cor.slopes, c("grp.x1-x2", "sub.x1-x2"))
  expect_lte(abs(v$cor.slopes[["grp.x1-x2"]] - v_lme4$cor.slopes[["grp.x1-x2"]]), 0.01)
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - v_lme4$cor.slopes[["sub:grp.x1-x2"]]), 0.01)
  # the nested path reads VarCorr(), which rounds correlations to 3 decimals
  expect_lte(abs(v$cor.slopes[["grp.x1-x2"]] - pd_cor(m, "grp", "x1", "x2")), 0.001)
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - pd_cor(m, "sub", "x1", "x2")), 0.001)

  # pdSymm blocks on both levels
  m <- nlme::lme(
    y ~ x1 + x2,
    random = list(grp = nlme::pdSymm(~ x1 + x2), sub = nlme::pdSymm(~ x1 + x2)),
    data = d_slopecor
  )
  v <- get_variance(m)
  expect_named(v$cor.slopes, c("grp.x1-x2", "sub.x1-x2"))
  expect_lte(abs(v$cor.slopes[["grp.x1-x2"]] - pd_cor(m, "grp", "x1", "x2")), 0.001)
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - pd_cor(m, "sub", "x1", "x2")), 0.001)

  # an intercept-only block or a pdDiag block does not stop the other block
  # from reporting
  expect_no_warning({
    m <- nlme::lme(
      y ~ x1 + x2,
      random = list(grp = ~1, sub = ~ x1 + x2),
      data = d_slopecor
    )
  })
  v <- get_variance(m)
  expect_named(v$cor.slopes, "sub.x1-x2")
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - pd_cor(m, "sub", "x1", "x2")), 0.001)

  expect_no_warning({
    m <- nlme::lme(
      y ~ x1 + x2,
      random = list(grp = nlme::pdDiag(~ x1 + x2), sub = ~ x1 + x2),
      data = d_slopecor
    )
  })
  v <- get_variance(m)
  expect_named(v$cor.slopes, "sub.x1-x2")
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - pd_cor(m, "sub", "x1", "x2")), 0.001)
})


test_that("get_variance, lme, no cor.slopes for pdDiag and pdIdent blocks", {
  skip_on_cran()
  data(Pixel, package = "nlme")
  sleep_slopecor <- sleepstudy
  sleep_slopecor$D2 <- sleep_slopecor$Days^2 / 10

  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = list(Subject = nlme::pdDiag(~ Days + D2)),
    data = sleep_slopecor
  )
  v <- get_variance(m)
  expect_false("cor.slopes" %in% names(v))
  expect_true("var.slope" %in% names(v))

  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = list(Subject = nlme::pdIdent(~ 0 + Days + D2)),
    data = sleep_slopecor
  )
  v <- get_variance(m)
  expect_false("cor.slopes" %in% names(v))
  expect_true("var.slope" %in% names(v))

  m <- nlme::lme(
    pixel ~ day + I(day^2),
    random = list(Dog = nlme::pdDiag(~ day + I(day^2)), Side = ~1),
    data = Pixel
  )
  v <- get_variance(m)
  expect_false("cor.slopes" %in% names(v))
  expect_true("var.slope" %in% names(v))
})


test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = "Days"))
  expect_identical(find_predictors(m2), list(conditional = c("age", "Sex")))
  expect_identical(
    find_predictors(m1, effects = "all"),
    list(conditional = "Days", random = "Subject")
  )
  expect_identical(
    find_predictors(m2, effects = "all"),
    list(conditional = c("age", "Sex"), random = "Subject")
  )
  expect_identical(find_predictors(m1, flatten = TRUE), "Days")
  expect_identical(
    find_predictors(m1, effects = "random"),
    list(random = "Subject")
  )
  expect_identical(
    find_predictors(m2, effects = "random"),
    list(random = "Subject")
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), "Reaction")
  expect_identical(find_response(m2), "distance")
})

test_that("get_response", {
  expect_equal(get_response(m1), sleepstudy$Reaction, ignore_attr = TRUE)
})

test_that("find_random", {
  expect_identical(find_random(m1), list(random = "Subject"))
  expect_identical(find_random(m2), list(random = "Subject"))
})

test_that("get_random", {
  expect_equal(
    get_random(m1),
    data.frame(Subject = sleepstudy$Subject),
    ignore_attr = TRUE
  )
  expect_equal(
    get_random(m2),
    data.frame(Subject = Orthodont$Subject),
    ignore_attr = TRUE
  )
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), 0.2, tolerance = 1e-5)
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1)), 180, ignore_attr = TRUE)
  expect_identical(colnames(get_data(m1)), c("Reaction", "Days", "Subject"))
  expect_identical(colnames(get_data(m2)), c("distance", "age", "Sex", "Subject"))
})

test_that("get_df", {
  expect_equal(get_df(m1, type = "residual"), c(161, 161), ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "normal"), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "wald"), c(161, 161), ignore_attr = TRUE)
  expect_equal(get_df(m2, type = "residual"), c(80, 80, 25), ignore_attr = TRUE)
  expect_equal(get_df(m2, type = "normal"), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "residual"), c(98, 76), ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "normal"), Inf, ignore_attr = TRUE)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("Reaction ~ Days"),
      random = as.formula("~1 + Days | Subject")
    ),
    ignore_attr = TRUE
  )
  expect_length(find_formula(m2), 2)
  expect_equal(
    find_formula(m2),
    list(
      conditional = as.formula("distance ~ age + Sex"),
      random = as.formula("~1 | Subject")
    ),
    ignore_attr = TRUE
  )
  expect_length(find_formula(m4), 2)
  expect_equal(
    find_formula(m4),
    list(
      conditional = as.formula("follicles ~ Time"),
      correlation = as.formula("~1 | Mare")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "Reaction",
      conditional = "Days",
      random = "Subject"
    )
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("Reaction", "Days", "Subject")
  )
  expect_identical(
    find_variables(m2),
    list(
      response = "distance",
      conditional = c("age", "Sex"),
      random = "Subject"
    )
  )
  expect_identical(
    find_variables(m4),
    list(
      response = "follicles",
      conditional = "Time",
      correlation = "Mare"
    )
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 180, ignore_attr = TRUE)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c("(Intercept)", "Days"),
      random = c("(Intercept)", "Days")
    )
  )
  expect_equal(nrow(get_parameters(m1)), 2) # nolint
  expect_identical(get_parameters(m1)$Parameter, c("(Intercept)", "Days"))
  expect_identical(
    find_parameters(m2),
    list(
      conditional = c("(Intercept)", "age", "SexFemale"),
      random = "(Intercept)"
    )
  )
})

test_that("find_algorithm", {
  expect_identical(
    find_algorithm(m1),
    list(algorithm = "REML", optimizer = "nlminb")
  )
})

test_that("get_variance", {
  skip_on_cran()

  expect_equal(
    get_variance(m1),
    list(
      var.fixed = 908.95336262308865116211,
      var.random = 1698.06593646939654718153,
      var.residual = 654.94240352794997761521,
      var.distribution = 654.94240352794997761521,
      var.dispersion = 0,
      var.intercept = c(Subject = 612.07951112963326067984),
      var.slope = c(Subject.Days = 35.07130179308116169068),
      cor.slope_intercept = c(Subject = 0.06600000000000000311)
    ),
    tolerance = 1e-3
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
  expect_identical(find_statistic(m3), "t-statistic")
})


test_that("Issue #658", {
  skip_if_not_installed("nlme")
  models <- lapply(
    c("", " + Sex"),
    function(x) {
      nlme::lme(as.formula(paste0("distance  ~ age", x)), random = ~1, data = Orthodont)
    }
  )
  dat <- lapply(models, get_data)
  form <- lapply(models, find_formula)
  expect_s3_class(form[[1]], "insight_formula")
  expect_s3_class(form[[2]], "insight_formula")
  expect_s3_class(dat[[1]], "data.frame")
  expect_s3_class(dat[[2]], "data.frame")
})

test_that("find_formula, random effects given as list or pdMat, #965", {
  data(RatPupWeight, package = "nlme")
  data(Pixel, package = "nlme")

  # one grouping factor, named list of formulas
  m_frm <- nlme::lme(weight ~ Treatment, random = ~ 1 | Litter, data = RatPupWeight)
  m_lst <- nlme::lme(weight ~ Treatment, random = list(Litter = ~1), data = RatPupWeight)
  expect_equal(find_formula(m_lst), find_formula(m_frm), ignore_attr = TRUE)
  expect_identical(find_random(m_lst), list(random = "Litter"))
  expect_identical(find_variables(m_lst), find_variables(m_frm))

  # random slope, named list of formulas and pdMat objects
  m_frm <- nlme::lme(distance ~ age, random = ~ age | Subject, data = Orthodont)
  m_lst <- nlme::lme(distance ~ age, random = list(Subject = ~age), data = Orthodont)
  m_pd <- nlme::lme(
    distance ~ age,
    random = list(Subject = nlme::pdDiag(~age)),
    data = Orthodont
  )
  m_pd2 <- nlme::lme(distance ~ age, random = nlme::pdDiag(~age), data = Orthodont)
  for (m in list(m_lst, m_pd, m_pd2)) {
    expect_equal(find_formula(m), find_formula(m_frm), ignore_attr = TRUE)
    expect_identical(find_random(m), list(random = "Subject"))
    expect_identical(find_random_slopes(m), list(random = "age"))
    expect_identical(find_variables(m), find_variables(m_frm))
  }

  # nested grouping factors, same random terms on each level
  m_frm <- nlme::lme(pixel ~ day, random = ~ 1 | Dog / Side, data = Pixel)
  m_lst <- nlme::lme(pixel ~ day, random = list(Dog = ~1, Side = ~1), data = Pixel)
  expect_equal(find_formula(m_lst), find_formula(m_frm), ignore_attr = TRUE)
  expect_identical(find_random(m_lst), find_random(m_frm))

  # nested grouping factors, different random terms on each level
  m_lst <- nlme::lme(pixel ~ day, random = list(Dog = ~day, Side = ~1), data = Pixel)
  expect_equal(
    find_formula(m_lst)$random,
    list(as.formula("~day | Dog"), as.formula("~1 | Dog:Side")),
    ignore_attr = TRUE
  )
  expect_identical(find_random(m_lst), list(random = c("Dog", "Dog:Side")))
  expect_identical(
    find_random(m_lst, split_nested = TRUE),
    list(random = c("Dog", "Side"))
  )
  expect_identical(find_random_slopes(m_lst), list(random = "day"))
})

test_that("find_formula, random effects given as object in another environment, #965", {
  fit <- function() {
    re <- list(Subject = nlme::pdDiag(~age))
    nlme::lme(distance ~ age, random = re, data = Orthodont)
  }
  m <- fit()
  m_frm <- nlme::lme(distance ~ age, random = ~ age | Subject, data = Orthodont)
  expect_equal(find_formula(m), find_formula(m_frm), ignore_attr = TRUE)
  expect_identical(find_random(m), list(random = "Subject"))
})

test_that("find_formula, glmmPQL with random effects given as list, #965", {
  skip_if_not_installed("MASS")
  data(bacteria, package = "MASS")

  m_frm <- MASS::glmmPQL(
    y ~ trt,
    random = ~ 1 | ID,
    family = binomial,
    data = bacteria,
    verbose = FALSE
  )
  m_lst <- MASS::glmmPQL(
    y ~ trt,
    random = list(ID = ~1),
    family = binomial,
    data = bacteria,
    verbose = FALSE
  )
  expect_equal(find_formula(m_lst), find_formula(m_frm), ignore_attr = TRUE)
  expect_identical(find_random(m_lst), list(random = "ID"))
  expect_identical(find_random(m_lst), find_random(m_frm))
})
