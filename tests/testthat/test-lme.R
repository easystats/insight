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
  v <- get_variance(m)
  expect_equal(v$var.random, 865.6673, tolerance = 1e-4)
  expect_equal(
    v$var.intercept,
    c(Dog = 647.3259, Side = 218.3414),
    tolerance = 1e-4
  )

  # random slopes: variance on the diagonal, covariance off the diagonal
  m_slope <- nlme::lme(pixel ~ day, random = ~ day | Dog / Side, data = Pixel)
  vc_nlme <- nlme::VarCorr(m_slope)
  vc <- insight:::.get_nested_lme_varcorr(m_slope)
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

  # nested, the intercept is the second term of the outer block
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
  sleep_slopecor$D3 <- cos(sleep_slopecor$Days)

  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = ~ Days + D2 | Subject,
    data = sleep_slopecor,
    control = nlme::lmeControl(opt = "optim")
  )
  v <- get_variance(m)
  expect_named(v$cor.slopes, "Subject.Days-D2")
  expect_lte(abs(v$cor.slopes - pd_cor(m, "Subject", "Days", "D2")), 1e-6)

  # pdDiag blocks estimate no correlations
  m <- nlme::lme(
    Reaction ~ Days + D2,
    random = list(Subject = nlme::pdDiag(~ Days + D2)),
    data = sleep_slopecor
  )
  expect_false("cor.slopes" %in% names(get_variance(m)))

  # Days and D2 share a pdSymm sub-block, D3 is in a pdIdent sub-block
  m <- nlme::lme(
    Reaction ~ Days + D2 + D3,
    random = list(
      Subject = nlme::pdBlocked(list(nlme::pdSymm(~ Days + D2), nlme::pdIdent(~ 0 + D3)))
    ),
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
  m <- nlme::lme(y ~ x1 + x2, random = ~ x1 + x2 | grp / sub, data = d_slopecor)
  v <- get_variance(m)
  expect_named(v$cor.slopes, c("grp.x1-x2", "sub.x1-x2"))
  # the nested path reads VarCorr(), which rounds correlations to 3 decimals
  expect_lte(abs(v$cor.slopes[["grp.x1-x2"]] - pd_cor(m, "grp", "x1", "x2")), 0.001)
  expect_lte(abs(v$cor.slopes[["sub.x1-x2"]] - pd_cor(m, "sub", "x1", "x2")), 0.001)
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
  data(Pixel, package = "nlme")

  m_frm <- nlme::lme(distance ~ age, random = ~ age | Subject, data = Orthodont)
  m_pd <- nlme::lme(
    distance ~ age,
    random = list(Subject = nlme::pdDiag(~age)),
    data = Orthodont
  )
  expect_equal(find_formula(m_pd), find_formula(m_frm), ignore_attr = TRUE)
  expect_identical(find_random(m_pd), list(random = "Subject"))
  expect_identical(find_random_slopes(m_pd), list(random = "age"))

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
})

test_that("find_formula, random effects given as object in another environment, #965", {
  fit <- function() {
    re <- list(Subject = nlme::pdDiag(~age))
    nlme::lme(distance ~ age, random = re, data = Orthodont)
  }
  m <- fit()
  expect_equal(find_formula(m)$random, ~ age | Subject, ignore_attr = TRUE)
})

test_that("find_formula, glmmPQL with random effects given as list, #965", {
  skip_if_not_installed("MASS")
  data(bacteria, package = "MASS")

  m_lst <- MASS::glmmPQL(
    y ~ trt,
    random = list(ID = ~1),
    family = binomial,
    data = bacteria,
    verbose = FALSE
  )
  expect_equal(find_formula(m_lst)$random, ~ 1 | ID, ignore_attr = TRUE)
  expect_identical(find_random(m_lst), list(random = "ID"))
})


test_that("get_modelmatrix and get_variance, glmmPQL", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("performance")
  data(bacteria, package = "MASS")

  # the terms of a glmmPQL model have the working response "zz", which is not
  # in the data
  m <- MASS::glmmPQL(
    y ~ trt + I(week > 2),
    random = ~ 1 | ID,
    family = binomial,
    data = bacteria,
    verbose = FALSE
  )
  mm <- model.matrix(~ trt + I(week > 2), data = bacteria)
  expect_equal(get_modelmatrix(m), mm, ignore_attr = TRUE)
  expect_equal(
    get_variance(m)$var.fixed,
    var(as.vector(mm %*% nlme::fixef(m))),
    tolerance = 1e-4
  )
})


test_that("null_model, lme and glmmPQL", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("performance")

  m <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont)
  m_null <- nlme::lme(distance ~ 1, random = ~ 1 | Subject, data = Orthodont)
  expect_equal(nlme::fixef(null_model(m)), nlme::fixef(m_null), tolerance = 1e-4)

  # a Poisson model needs the null model for the distribution variance
  set.seed(123)
  d_pql_pois <- data.frame(g1 = rep(1:20, each = 10), x = rnorm(200))
  d_pql_pois$cnt <- rpois(
    200,
    exp(0.5 + 0.3 * d_pql_pois$x + rnorm(20, sd = 0.4)[d_pql_pois$g1])
  )
  m <- MASS::glmmPQL(
    cnt ~ x,
    random = ~ 1 | g1,
    family = poisson,
    data = d_pql_pois,
    verbose = FALSE
  )
  m_null <- MASS::glmmPQL(
    cnt ~ 1,
    random = ~ 1 | g1,
    family = poisson,
    data = d_pql_pois,
    verbose = FALSE
  )
  v <- get_variance(m)
  v_null <- get_variance(m, null_model = m_null)
  expect_gt(v$var.distribution, 0)
  expect_equal(v$var.distribution, v_null$var.distribution, tolerance = 1e-4)
})


# correlation part, read from the fitted correlation structure ----------------

# `t` is an integer position for corAR1()
lme_orthodont_t <- Orthodont
lme_orthodont_t$t <- as.integer(lme_orthodont_t$age)

test_that("find_formula, lme, correlation object reassigned after the fit", {
  lme_corr_reassign <<- nlme::corAR1(form = ~ t | Subject)
  withr::defer(rm(lme_corr_reassign, envir = globalenv()))
  m <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    data = lme_orthodont_t,
    correlation = lme_corr_reassign
  )
  lme_corr_reassign <<- nlme::corAR1(form = ~ 1 | Sex)
  expect_equal(find_formula(m)$correlation, ~ t | Subject, ignore_attr = TRUE)
})

test_that("find_formula, lme, correlation object local to a function", {
  fit_local <- function() {
    lme_corr_local <- nlme::corAR1(form = ~ t | Subject)
    nlme::lme(
      distance ~ age,
      random = ~ 1 | Subject,
      data = lme_orthodont_t,
      correlation = lme_corr_local
    )
  }
  expect_equal(find_formula(fit_local())$correlation, ~ t | Subject, ignore_attr = TRUE)
})

test_that("find_formula, lme, correlation `form` given by position", {
  m <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    data = lme_orthodont_t,
    correlation = nlme::corAR1(0.2, ~ t | Subject)
  )
  expect_equal(find_formula(m)$correlation, ~ t | Subject, ignore_attr = TRUE)
})

test_that("find_formula, lme, correlation structure without `form`", {
  # nlme uses the grouping of the random effects
  m <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    data = lme_orthodont_t,
    correlation = nlme::corAR1()
  )
  expect_equal(find_formula(m)$correlation, ~ 1 | Subject, ignore_attr = TRUE)
  expect_identical(find_variables(m)$correlation, "Subject")
  # the grouping is also a predictor, as for an explicit `form`
  expect_identical(
    find_predictors(m),
    list(conditional = "age", correlation = "Subject")
  )

  m_nested <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Sex / Subject,
    data = lme_orthodont_t,
    correlation = nlme::corAR1()
  )
  expect_equal(
    find_formula(m_nested)$correlation,
    ~ 1 | Sex / Subject,
    ignore_attr = TRUE
  )
  expect_identical(find_variables(m_nested)$correlation, c("Sex", "Subject"))

  # no correlation structure, no correlation part
  m_none <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    data = lme_orthodont_t
  )
  expect_named(find_formula(m_none), c("conditional", "random"))
})
