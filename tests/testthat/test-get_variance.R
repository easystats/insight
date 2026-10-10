skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data("Penicillin", package = "lme4")
set.seed(12345)
sleepstudy$grp <- sample.int(5, size = 180, replace = TRUE)
sleepstudy$subgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$grp == i
  sleepstudy$subgrp[filter_group] <-
    sample.int(30, size = sum(filter_group), replace = TRUE)
}

study_data <<- sleepstudy

suppressMessages({
  fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), study_data)
  fm2 <- lme4::lmer(Reaction ~ Days + (Days || Subject), study_data)
  fm3 <- lme4::lmer(
    Reaction ~ Days + (1 + Days || grp / subgrp) + (1 + Days | Subject),
    data = study_data
  )
  fm4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), study_data)
  fm5 <- lme4::lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = study_data
  )
  fm6 <- lme4::lmer(diameter ~ 0 + sample + (1 | plate), data = Penicillin)
})

v1 <- suppressWarnings(get_variance(fm1))
v2 <- suppressWarnings(get_variance(fm2))
v3 <- suppressWarnings(get_variance(fm3))
v4 <- suppressWarnings(get_variance(fm4))
v5 <- suppressWarnings(get_variance(fm5))
v6 <- suppressWarnings(get_variance(fm6))

test_that("error for non-mixed", {
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  expect_warning(
    get_variance(glmmTMB::glmmTMB(mpg ~ gear, data = mtcars)),
    regex = "This function only works for mixed models"
  )
  expect_silent(get_variance(
    glmmTMB::glmmTMB(mpg ~ gear, data = mtcars),
    verbose = FALSE
  ))
})

test_that("get_variance-1", {
  expect_equal(v1$var.intercept, c(Subject = 612.10016), tolerance = 1e-2)
  expect_equal(v1$var.slope, c(Subject.Days = 35.07171), tolerance = 1e-2)
})

test_that("get_variance-2", {
  expect_equal(v2$var.intercept, c(Subject = 627.56905), tolerance = 1e-2)
  expect_equal(v2$var.slope, c(Subject.Days = 35.85838), tolerance = 1e-2)
})

test_that("get_variance-3", {
  expect_equal(
    v3$var.intercept,
    c(
      subgrp.grp.1 = 0,
      Subject = 662.52047,
      grp.1 = 0
    ),
    tolerance = 1e-2
  )
  expect_equal(
    v3$var.slope,
    c(
      Subject.Days = 34.25771,
      subgrp.grp.Days = 7.88485,
      grp.Days = 0
    ),
    tolerance = 1e-2
  )
})

test_that("get_variance-4", {
  expect_equal(v4$var.intercept, c(Subject = 1378.17851), tolerance = 1e-2)
  expect_null(v4$var.slope)
})

test_that("get_variance-5", {
  expect_equal(
    v5$var.intercept,
    c(
      `subgrp:grp` = 38.76069,
      Subject = 1377.50569,
      grp = 3.32031
    ),
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

test_that("get_variance-7", {
  model <- lme4::lmer(Reaction ~ Days + (1 + Days || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  expect_equal(
    vmodel,
    list(
      var.fixed = 908.95336,
      var.random = 1649.53296,
      var.residual = 653.5835,
      var.distribution = 653.5835,
      var.dispersion = 0,
      var.intercept = c(Subject = 627.56905),
      var.slope = c(Subject.Days = 35.85838)
    ),
    tolerance = 1e-2
  )
})


test_that("get_variance, var.random includes uncorrelated random slopes", {
  # lme4 splits (x || g) into several VarCorr() elements ("g", "g.1"), but
  # ranef() returns one element "g"
  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), data = study_data)
  vc <- as.data.frame(lme4::VarCorr(model))
  x <- study_data$Days
  # mean random-effect variance (Johnson 2014)
  expected <- vc$vcov[1] + vc$vcov[2] * mean(x^2)
  expect_equal(get_variance_random(model), c(var.random = expected), tolerance = 1e-4)
  expect_equal(expected, 1649.53296, tolerance = 1e-4)

  # same for a centered predictor
  study_data$Days0 <- study_data$Days - 10
  model <- lme4::lmer(Reaction ~ Days0 + (Days0 || Subject), data = study_data)
  vc <- as.data.frame(lme4::VarCorr(model))
  expected <- vc$vcov[1] + vc$vcov[2] * mean(study_data$Days0^2)
  expect_equal(get_variance_random(model), c(var.random = expected), tolerance = 1e-4)

  # nested uncorrelated terms: VarCorr() names "subgrp.grp", ranef() "subgrp:grp"
  vc <- lme4::VarCorr(fm3)
  X <- lme4::getME(fm3, "X")
  expected <- sum(vapply(
    vc,
    function(Sigma) {
      Z <- X[, rownames(Sigma), drop = FALSE]
      mean(rowSums((Z %*% Sigma) * Z))
    },
    numeric(1)
  ))
  # fm3 is singular, so tolerance = 0 forces the computation
  expect_equal(
    get_variance_random(fm3, tolerance = 0, verbose = FALSE),
    c(var.random = expected),
    tolerance = 1e-4
  )
})


test_that("get_variance-8", {
  model <- lme4::lmer(Reaction ~ Days + (0 + Days || Subject), data = study_data)
  vmodel <- get_variance(model)

  expect_equal(
    vmodel,
    list(
      var.fixed = 908.95336,
      var.random = 1502.179,
      var.residual = 842.02962,
      var.distribution = 842.02962,
      var.dispersion = 0,
      var.slope = c(Subject.Days = 52.70804)
    ),
    tolerance = 1e-2
  )
})


# categorical rnd slope

test_that("get_variance-9", {
  data(sleepstudy, package = "lme4")
  sleepstudy$Days2 <- cut(sleepstudy$Days, breaks = c(-1, 3, 6, 10))
  study_data2 <<- sleepstudy

  model <- lme4::lmer(Reaction ~ Days2 + (1 + Days2 | Subject), data = study_data2)
  vmodel <- get_variance(model)

  expect_equal(
    vmodel,
    list(
      var.fixed = 807.085453556748,
      var.random = 1711.44396436951,
      var.residual = 748.811071562908,
      var.distribution = 748.811071562908,
      var.dispersion = 0,
      var.intercept = c(Subject = 663.280418978822),
      var.slope = c(
        `Subject.Days2(3,6]` = 882.364188919403,
        `Subject.Days2(6,10]` = 1415.70768194576
      ),
      cor.slope_intercept = structure(
        c(0.361173061386374, 0.331878499015884),
        dim = 2:1,
        dimnames = list(c("Days2(3,6]", "Days2(6,10]"), "Subject")
      ),
      cor.slopes = c(`Subject.Days2(3,6]-Days2(6,10]` = 0.847444720096841)
    ),
    tolerance = 1e-2
  )
})


test_that("get_variance-10", {
  model <- suppressMessages(lme4::lmer(
    Reaction ~ Days2 + (1 + Days2 || Subject),
    data = study_data2
  ))
  vmodel <- suppressWarnings(get_variance(model))

  expect_equal(
    vmodel,
    list(
      var.fixed = 807.08545355676,
      var.residual = 740.875581179784,
      var.distribution = 740.875581179784,
      var.dispersion = 0,
      var.intercept = c(Subject = 738.635155172211),
      var.slope = c(
        `Subject.Days2(-1,3]` = 0,
        `Subject.Days2(3,6]` = 994.015865559888,
        `Subject.Days2(6,10]` = 1545.72576115283
      ),
      cor.slopes = c(`Subject.1.Days2(3,6]-Days2(6,10]` = 0.859480774219098)
    ),
    tolerance = 1e-2
  )
})


test_that("get_variance-11", {
  model <- lme4::lmer(Reaction ~ Days2 + (0 + Days2 | Subject), data = study_data2)
  vmodel <- get_variance(model)

  expect_equal(
    vmodel,
    list(
      var.fixed = 807.085453556794,
      var.random = 1446.13555108848,
      var.residual = 748.813858500395,
      var.distribution = 748.813858500395,
      var.dispersion = 0,
      var.slope = c(
        `Subject.Days2(-1,3]` = 663.27445659023,
        `Subject.Days2(3,6]` = 2098.24691538121,
        `Subject.Days2(6,10]` = 2722.20492158038
      ),
      cor.slopes = c(
        `Subject.Days2(-1,3]-Days2(3,6]` = 0.796453122321232,
        `Subject.Days2(-1,3]-Days2(6,10]` = 0.732956077304911,
        `Subject.Days2(3,6]-Days2(6,10]` = 0.924018087860575
      )
    ),
    tolerance = 1e-2
  )
})


test_that("get_variance-12", {
  model <- lme4::lmer(Reaction ~ Days2 + (0 + Days2 || Subject), data = study_data2)
  vmodel <- get_variance(model)

  expect_equal(
    vmodel,
    list(
      var.fixed = 807.085453556794,
      var.random = 1446.13555108848,
      var.residual = 748.813858500395,
      var.distribution = 748.813858500395,
      var.dispersion = 0,
      var.slope = c(
        `Subject.Days2(-1,3]` = 663.27445659023,
        `Subject.Days2(3,6]` = 2098.24691538121,
        `Subject.Days2(6,10]` = 2722.20492158038
      ),
      cor.slopes = c(
        `Subject.Days2(-1,3]-Days2(3,6]` = 0.796453122321232,
        `Subject.Days2(-1,3]-Days2(6,10]` = 0.732956077304911,
        `Subject.Days2(3,6]-Days2(6,10]` = 0.924018087860575
      )
    ),
    tolerance = 1e-2
  )
})


# test random slope correlation for categorical random slope

test_that("get_variance-cat_random_slope", {
  data(cake, package = "lme4")
  suppressMessages({
    m <- lme4::lmer(angle ~ temperature + (temperature | recipe), data = cake)
  })
  vc <- suppressWarnings(get_variance(m))

  expect_equal(
    vc$cor.slopes,
    c(
      `recipe.temperature.L-temperature.C` = 0.99999964,
      `recipe.temperature.Q-temperature.C` = 0.99999931,
      `recipe.temperature.L-temperature.Q` = 0.99999941,
      `recipe.temperature.L-temperature^4` = 0.99999961,
      `recipe.temperature.Q-temperature^4` = 0.99999912,
      `recipe.temperature.C-temperature^4` = 0.99999996,
      `recipe.temperature.L-temperature^5` = -0.99999977,
      `recipe.temperature.Q-temperature^5` = -0.99999849,
      `recipe.temperature.C-temperature^5` = -0.99999936,
      `recipe.temperature^4-temperature^5` = -0.99999941
    ),
    tolerance = 1e-3
  )
})

data(sleepstudy, package = "lme4")
set.seed(123)
sleepstudy$Months <- sample.int(4, nrow(sleepstudy), TRUE)
study_data3 <<- sleepstudy

m2 <- lme4::lmer(Reaction ~ Days + (0 + Days | Subject), data = study_data3)
m5 <- lme4::lmer(Reaction ~ Days + (0 + Days + Months | Subject), data = study_data3)

test_that("random effects CIs, simple slope", {
  vc <- suppressWarnings(get_variance(m2))
  expect_named(
    vc,
    c(
      "var.fixed",
      "var.random",
      "var.residual",
      "var.distribution",
      "var.dispersion",
      "var.slope"
    )
  )
})

test_that("random effects CIs, simple slope", {
  vc <- suppressWarnings(get_variance(m5))
  expect_equal(
    vc,
    list(
      var.fixed = 921.929610133035,
      var.random = 1068.04697608476,
      var.residual = 764.479364064599,
      var.distribution = 764.479364064599,
      var.dispersion = 0,
      var.slope = c(
        Subject.Days = 37.4753324942022,
        Subject.Months = 27.6430649522841
      ),
      cor.slopes = c(`Subject.Days-Months` = 0.455625778436967)
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})


test_that("random effects CIs, poly slope", {
  data(cake, package = "lme4")
  suppressMessages({
    m <- lme4::lmer(
      angle ~ poly(temp, 2) + (poly(temp, 2) | replicate) + (1 | recipe),
      data = cake
    )
  })
  vc <- suppressWarnings(get_variance(m))

  expect_equal(
    vc$cor.slopes,
    c(`replicate.poly(temp, 2)1-poly(temp, 2)2` = 0.940016422944175),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("fixed effects variance for rank-deficient models, #765", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.8")
  set.seed(101)
  dd <- data.frame(
    z = rnorm(1000),
    x1 = 1:1000,
    x2 = runif(1000, 0, 10),
    re = rep(1:20, each = 50)
  )
  dd <- transform(
    dd,
    x3 = as.factor(ifelse(
      x1 <= 500,
      "Low",
      sample(c("Middle", "High"), 1000, replace = TRUE)
    ))
  )
  dd <- transform(
    dd,
    x4 = as.factor(ifelse(
      x1 > 500,
      "High",
      sample(c("Absent", "Low"), 1000, replace = TRUE)
    ))
  )
  dd <- transform(dd, z = z + re * 5)

  expect_message({
    mod_TMB <- glmmTMB::glmmTMB(
      z ~ x1 + x2 + x3 + x4 + (1 | re),
      data = dd,
      start = list(theta = 3),
      control = glmmTMB::glmmTMBControl(rank_check = "adjust")
    )
  })
  out <- get_variance_fixed(mod_TMB)
  expect_equal(out, 627.04511567, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("distribution variance for betabinomial uses the model's link", {
  skip_if_not_installed("glmmTMB")
  set.seed(123)
  d <- data.frame(
    g = factor(rep(1:15, each = 20)),
    x = rnorm(300),
    n = 20
  )
  eta <- -1 + 0.4 * d$x + rnorm(15, 0, 0.6)[d$g]
  p <- 1 - exp(-exp(eta))
  d$y <- rbinom(300, d$n, rbeta(300, p * 10, (1 - p) * 10))

  # expected value, with the inverse link stated independently of the model
  expected_variance <- function(m, inverse_link) {
    b0 <- unname(glmmTMB::fixef(null_model(m))$cond)
    mu <- inverse_link(b0)
    phi <- stats::sigma(m)
    log1p(mu * (1 - mu) / (1 + phi) / mu^2)
  }

  m_logit <- glmmTMB::glmmTMB(
    cbind(y, n - y) ~ x + (1 | g),
    data = d,
    family = glmmTMB::betabinomial(link = "logit")
  )
  expect_equal(
    get_variance_distribution(m_logit, verbose = FALSE),
    expected_variance(m_logit, function(eta) 1 / (1 + exp(-eta))),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  m_cloglog <- glmmTMB::glmmTMB(
    cbind(y, n - y) ~ x + (1 | g),
    data = d,
    family = glmmTMB::betabinomial(link = "cloglog")
  )
  expect_equal(
    get_variance_distribution(m_cloglog, verbose = FALSE),
    expected_variance(m_cloglog, function(eta) 1 - exp(-exp(eta))),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  m_probit <- glmmTMB::glmmTMB(
    cbind(y, n - y) ~ x + (1 | g),
    data = d,
    family = glmmTMB::betabinomial(link = "probit")
  )
  expect_equal(
    get_variance_distribution(m_probit, verbose = FALSE),
    expected_variance(m_probit, stats::pnorm),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("get_variance errors when performance is not installed, #928", {
  real_check <- check_if_installed
  # pretend that "performance" is not installed
  local_mocked_bindings(
    check_if_installed = function(package, ...) {
      package[package == "performance"] <- "notAnInstalledPackage928"
      real_check(package, ...)
    }
  )
  expect_error(get_variance(fm4), "to check for singularity", fixed = TRUE)
  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = study_data)
  expect_error(get_variance(m), "to check for singularity", fixed = TRUE)
  # non-mixed models keep their warning and do not need performance
  m <- glmmTMB::glmmTMB(Reaction ~ Days, data = study_data)
  expect_warning(
    expect_null(get_variance(m)),
    "This function only works for mixed models",
    fixed = TRUE
  )
})
