skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("lme4")
skip_if_not_installed("performance", minimum_version = "0.12.1")
skip_if_not_installed("datawizard")


# ==============================================================================
# Bernoulli mixed models, glmmTMB ----
# ==============================================================================

test_that("glmmTMB, bernoulli", {
  skip_if(packageVersion("MuMIn") == "1.48.4")
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
# Validate against Nakagawa et al. 2017 paper!
test_that("glmer, Bernoulli", {
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
  DataMale <- subset(Data, Sex == "Male")
  PopulationE <- rnorm(12, 0, sqrt(1.2))
  ContainerE <- rnorm(120, 0, sqrt(0.2))
  ColourL <- with(DataMale, 0.8 * (-1) + 0.8 * (as.numeric(Treatment) - 1) + 0.5 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
  DataMale$Colour <- rbinom(length(ColourL), 1, plogis(ColourL))

  morphmodGLMERr <- lme4::glmer(
    Colour ~ 1 + (1 | Population) + (1 | Container),
    family = binomial(),
    data = DataMale
  )
  # Fit alternative model including fixed and all random effects
  morphmodGLMERf <- lme4::glmer(
    Colour ~ Treatment + Habitat + (1 | Population) + (1 | Container),
    family = binomial(),
    data = DataMale
  )

  VarF <- var(as.vector(model.matrix(morphmodGLMERf) %*% lme4::fixef(morphmodGLMERf)))
  VarDS <- pi^2 / 3
  Vt <- lme4::VarCorr(morphmodGLMERr)$Population + lme4::VarCorr(morphmodGLMERr)$Container
  pmean <- as.numeric(plogis(as.vector(lme4::fixef(morphmodGLMERr)) - 0.5 * Vt * tanh(as.vector(lme4::fixef(morphmodGLMERr)) * (1 + 2 * exp(-0.5 * Vt)) / 6)))
  VarOL <- 1 / (pmean * (1 - pmean))

  R2glmmM <- VarF / (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf))) + VarDS)
  R2glmmC <- (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf)))) / (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf))) + VarDS)
  out <- performance::r2_nakagawa(morphmodGLMERf)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  R2glmmM <- VarF / (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf))) + VarOL)
  R2glmmC <- (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf)))) / (VarF + sum(as.numeric(lme4::VarCorr(morphmodGLMERf))) + VarOL)
  out <- performance::r2_nakagawa(morphmodGLMERf, approximation = "observation_level")
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)
})
