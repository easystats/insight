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
  # example data from Nakagawa et al. 2017
  Population <- gl(12, 80, 960)
  Container <- gl(120, 8, 960)
  Sex <- factor(rep(rep(c("Female", "Male"), each = 8), 60))
  Habitat <- factor(rep(rep(c("Dry", "Wet"), each = 4), 120))
  Treatment <- factor(rep(c("Cont", "Exp"), 480))
  Data <- data.frame(
    Population = Population, Container = Container, Sex = Sex,
    Habitat = Habitat, Treatment = Treatment
  )
  DataFemale <- Data[Data$Sex == "Female", ]
  set.seed(777)
  PopulationE <- rnorm(12, 0, sqrt(0.4))
  ContainerE <- rnorm(120, 0, sqrt(0.05))
  EggL <- with(DataFemale, 1.1 + 0.5 * (as.numeric(Treatment) - 1) + 0.1 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(480, 0, sqrt(0.1)))
  DataFemale$Egg <- rpois(length(EggL), exp(EggL))
  DataAll <- Data
  PopulationE <- rnorm(12, 0, sqrt(0.5))
  ContainerE <- rnorm(120, 0, sqrt(0.8))
  ParasiteL <- with(DataAll, 1.8 + 2 * (-1) * (as.numeric(Sex) - 1) + 0.8 * (-1) * (as.numeric(Treatment) - 1) + 0.7 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
  DataAll$Parasite <- rnbinom(length(ParasiteL), size = 5, mu = exp(ParasiteL))
  PopulationE <- rnorm(12, 0, sqrt(1.3))
  ContainerE <- rnorm(120, 0, sqrt(0.3))
  DataAll$BodyL <- 15 + 3 * (-1) * (as.numeric(Sex) - 1) + 0.4 * (as.numeric(Treatment) - 1) + 0.15 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(960, 0, sqrt(1.2))
  PopulationE <- rnorm(12, 0, sqrt(0.2))
  ContainerE <- rnorm(120, 0, sqrt(0.2))
  ExplorationL <- with(DataAll, 4 + 1 * (-1) * (as.numeric(Sex) - 1) + 2 * (as.numeric(Treatment) - 1) + 0.5 * (-1) * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
  DataAll$Exploration <- rgamma(length(ExplorationL), shape = exp(ExplorationL) * 0.3, rate = 0.3)

  sizemodeGLMERr <- lme4::glmer(
    BodyL ~ 1 + (1 | Population) + (1 | Container),
    family = Gamma(link = log),
    data = DataAll
  )
  # Fit alternative model including fixed and all random effects
  sizemodeGLMERf <- lme4::glmer(
    BodyL ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
    family = Gamma(link = log), data = DataAll
  )

  VarF <- var(as.vector(model.matrix(sizemodeGLMERf) %*% lme4::fixef(sizemodeGLMERf)))
  nuF <- 1 / attr(lme4::VarCorr(sizemodeGLMERf), "sc")^2
  VarOdF <- 1 / nuF # the delta method
  VarOlF <- log(1 + 1 / nuF) # log-normal approximation
  VarOtF <- trigamma(nuF) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf)))) / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOlF)
  out <- performance::r2_nakagawa(sizemodeGLMERf, null_model = sizemodeGLMERr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf)))) / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOdF)
  out <- performance::r2_nakagawa(sizemodeGLMERf, null_model = sizemodeGLMERr, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf)))) / (VarF + sum(as.numeric(lme4::VarCorr(sizemodeGLMERf))) + VarOtF)
  out <- performance::r2_nakagawa(sizemodeGLMERf, null_model = sizemodeGLMERr, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
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
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-3)
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
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-3)
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

  # we skip this test for now, because MuMIn might use a wrong computation
  # of the approximation here. See discussion in #877 for details
  skip_if(TRUE)

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
# Validate against Nakagawa et al. 2017 paper!
test_that("glmmTMB, Nbinom1", {
  data(Salamanders, package = "glmmTMB")
  glmmTMBr <- glmmTMB::glmmTMB(
    count ~ (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  glmmTMBf <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  # Calculation based on Supplement 2 of Nakagawa et al. 2017
  VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% glmmTMB::fixef(glmmTMBf)$cond))
  # this is "mu" in insight
  lambda <- as.numeric(exp(glmmTMB::fixef(glmmTMBr)$cond + 0.5 * (as.numeric(glmmTMB::VarCorr(glmmTMBr)$cond[1]))))
  # this is "sig" in insight
  thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
  # this is what ".variance_distributional()" returns
  VarOdF <- 1 / lambda + 1 / thetaF # the delta method
  VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
  VarOtF <- trigamma((1 / lambda + 1 / thetaF)^-1) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
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

  # we skip this test for now, because MuMIn might use a wrong computation
  # of the approximation here. See discussion in #877 for details
  skip_if(TRUE)

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
# Validate against Nakagawa et al. 2017 paper!
test_that("glmmTMB, Nbinom1 zero-inflated", {
  data(Salamanders, package = "glmmTMB")
  glmmTMBr <- glmmTMB::glmmTMB(
    count ~ (1 | site),
    ziformula = ~ 1,
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  glmmTMBf <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    ziformula = ~ mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders, REML = TRUE
  )
  # Calculation based on Supplement 2 of Nakagawa et al. 2017
  VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% glmmTMB::fixef(glmmTMBf)$cond))
  # this is "mu" in insight
  lambda <- as.numeric(exp(glmmTMB::fixef(glmmTMBr)$cond + 0.5 * (as.numeric(glmmTMB::VarCorr(glmmTMBr)$cond[1]))))
  # this is "sig" in insight
  thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
  # this is what ".variance_distributional()" returns
  VarOdF <- 1 / lambda + 1 / thetaF # the delta method
  VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
  VarOtF <- trigamma((1 / lambda + 1 / thetaF)^-1) # trigamma function

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOlF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOdF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "delta")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # trigamma
  R2glmmM <- VarF / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  R2glmmC <- (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(glmmTMB::VarCorr(glmmTMBf)$cond)) + VarOtF)
  out <- performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr, approximation = "trigamma")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
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
