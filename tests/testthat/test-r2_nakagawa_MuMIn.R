skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance")
skip_if_not_installed("datawizard")


# ==============================================================================
# linear mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, linear", {
  data(sleepstudy, package = "lme4")

  # no random effects
  m1 <- glmmTMB::glmmTMB(Reaction ~ Days, data = sleepstudy)
  m2 <- lm(Reaction ~ Days, data = sleepstudy)
  out1 <- performance::r2(m1)
  out2 <- performance::r2(m2)
  expect_equal(out1$R2, out2$R2, tolerance = 1e-4)
  expect_equal(out1$R2_adjusted, out2$R2_adjusted, tolerance = 1e-1)

  # linear, no random slope
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, no random slope, inverse
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = sleepstudy,
    family = gaussian("inverse")
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m))
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, with random slope
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, random slope, inverse
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    family = gaussian("inverse")
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m))
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, random slope, log
  m <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    family = gaussian("log")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# linear mixed models, lme4 ----
# ==============================================================================

test_that("lme4, linear", {
  data(sleepstudy, package = "lme4")

  # linear, no random slope
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # linear, with random slope
  m <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Gamma mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, Gamma", {
  ## FIXME: works. but is very slow. try to find better example
  skip_if(TRUE)
  data(sleepstudy, package = "lme4")

  # linear, no random slope
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy, family = Gamma())
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  expect_equal(out1[, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Bernoulli mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, bernoulli", {
  # dataset ---------------------------------
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 500, size = 1, prob = 0.3),
    var_binom = as.factor(rbinom(n = 500, size = 1, prob = 0.3)),
    var_cont = rnorm(n = 500, mean = 10, sd = 7)
  )
  dat$var_cont <- datawizard::standardize(dat$var_cont)
  dat$group <- NA
  dat$group[dat$outcome == 1] <- sample(
    letters[1:5],
    size = sum(dat$outcome == 1),
    replace = TRUE,
    prob = c(0.1, 0.2, 0.3, 0.1, 0.3)
  )
  dat$group[dat$outcome == 0] <- sample(
    letters[1:5],
    size = sum(dat$outcome == 0),
    replace = TRUE,
    prob = c(0.3, 0.1, 0.1, 0.4, 0.1)
  )

  # glmmTMB, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "logit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, probit, no random slope -----------------------------------------
  m <- glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "probit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, cloglog, no random slope -----------------------------------------
  m <- glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "cloglog")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, probit, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 + var_cont | group),
    data = dat,
    family = binomial(link = "probit")
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 + var_cont | group),
    data = dat,
    family = binomial(link = "logit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, cloglog, random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(
    outcome ~ var_binom + var_cont + (1 + var_cont | group),
    data = dat,
    family = binomial(link = "cloglog")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Bernoulli mixed models, lme4 ----
# ==============================================================================

test_that("lme4, bernoulli", {
  # dataset ---------------------------------
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 500, size = 1, prob = 0.3),
    var_binom = as.factor(rbinom(n = 500, size = 1, prob = 0.3)),
    var_cont = rnorm(n = 500, mean = 10, sd = 7)
  )
  dat$var_cont <- datawizard::standardize(dat$var_cont)
  dat$group <- NA
  dat$group[dat$outcome == 1] <- sample(
    letters[1:5],
    size = sum(dat$outcome == 1),
    replace = TRUE,
    prob = c(0.1, 0.2, 0.3, 0.1, 0.3)
  )
  dat$group[dat$outcome == 0] <- sample(
    letters[1:5],
    size = sum(dat$outcome == 0),
    replace = TRUE,
    prob = c(0.3, 0.1, 0.1, 0.4, 0.1)
  )

  # lme4, no random slope ----------------------------------------------------
  m <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "logit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, probit, no random slope ---------------------------------------------
  m <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "probit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, cloglog, no random slope ---------------------------------------------
  m <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "cloglog")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, random slope -------------------------------------------------
  m <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 + var_cont | group),
    data = dat,
    family = binomial(link = "logit")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, cloglog, random slope -------------------------------------------------
  m <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 + var_cont | group),
    data = dat,
    family = binomial(link = "cloglog")
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Binomial mixed models, lme4 ----
# ==============================================================================

test_that("lme4, binomial", {
  # dataset
  data(cbpp, package = "lme4")

  # lme4, no random slope ----------------------------------------------------
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)
})


# ==============================================================================
# Binomial mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, binomial", {
  # dataset
  data(cbpp, package = "lme4")

  # lme4, no random slope ----------------------------------------------------
  m <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)
})


# ==============================================================================
# Poisson mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, Poisson", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # glmmTMB, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, sqrt, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    family = poisson(), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # we have slight differences here: MuMIn uses "var(fitted())" to exctract fixed
  # effects variances, while insight uses "var(beta %*% t(mm))". The latter gives
  # different values when random slopes are involved
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # glmmTMB, sqrt, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    family = poisson("sqrt"), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# Poisson mixed models, lme4 ----
# ==============================================================================

test_that("lme4, Poisson", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # lme4, no random slope -------------------------------------------------
  m <- lme4::glmer(count ~ mined + (1 | site),
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # lme4, sqrt, no random slope -------------------------------------------------
  m <- lme4::glmer(count ~ mined + (1 | site),
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# neg-binomial mixed models, lme4 ----
# ==============================================================================

test_that("glmer, negbin", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- lme4::glmer.nb(
    count ~ mined + spp + (1 | site),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# neg-binomial1 mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, Nbinom1", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # with random slopes
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + cover + (1 + cover | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # no random slopes, sqrt
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1("sqrt"),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# neg-binomial2 mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, Nbinom2", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # with random slopes
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + cover + (1 + cover | site),
    family = glmmTMB::nbinom2(),
    data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)
})


# ==============================================================================
# Poisson zero-inflated mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, Poisson zero-inflated", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # glmmTMB, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson(), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, sqrt, no random slope -------------------------------------------------
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson("sqrt"), data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # glmmTMB, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    ziformula = ~mined,
    family = poisson(), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # we have slight differences here: MuMIn uses "var(fitted())" to exctract fixed
  # effects variances, while insight uses "var(beta %*% t(mm))". The latter gives
  # different values when random slopes are involved
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # glmmTMB, sqrt, random slope -------------------------------------------------
  m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 + cover | site),
    ziformula = ~mined,
    family = poisson("sqrt"), data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# neg-binomial1 zero-inflated mixed models, glmmTMB
# ==============================================================================

test_that("glmmTMB, Nbinom1 zero-inflated", {
  # dataset ---------------------------------
  data(Salamanders, package = "glmmTMB")

  # no random slopes
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    ziformula = ~mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)

  # with random slopes
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + cover + (1 + cover | site),
    ziformula = ~mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders
  ))
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m, tolerance = 1e-8)
  # matches theoretical values
  expect_equal(out1[2, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-1)
  expect_equal(out1[2, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-1)

  # no random slopes, sqrt
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    ziformula = ~mined,
    family = glmmTMB::nbinom1("sqrt"),
    data = Salamanders
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- performance::r2_nakagawa(m)
  # matches delta values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# beta mixed models, glmmTMB
# ==============================================================================

skip_if_not_installed("betareg")

test_that("glmmTMB, beta_family", {
  # dataset ---------------------------------
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    data = FoodExpenditure,
    family = glmmTMB::beta_family()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m, verbose = FALSE))
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})
