skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance")
skip_if_not_installed("datawizard")


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
