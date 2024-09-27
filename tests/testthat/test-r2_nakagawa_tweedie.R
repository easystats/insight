skip_on_cran()

skip_if_not_installed("cplm")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("performance", minimum_version = "0.12.1")

# cplm::cpglmm doesn't work
suppressPackageStartupMessages({
  suppressWarnings(suppressMessages(library(cplm, quietly = TRUE, warn.conflicts = FALSE)))
})

# ==============================================================================
# Tweedie mixed models, cplm
# ==============================================================================

test_that("cplm, tweedie", {
  set.seed(1234)
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

  parmodCPr <- cpglmm(
    Parasite ~ 1 + (1 | Population) + (1 | Container),
    link = 0,
    data = DataAll
  )
  parmodCPf <- cpglmm(
    Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
    link = 0,
    data = DataAll
  )

  VarF <- var(as.vector(model.matrix(parmodCPf) %*% cplm::fixef(parmodCPf)))
  phiF <- parmodCPf@phi # the dispersion parameter
  pF <- parmodCPf@p # the index parameter
  mu <- exp(cplm::fixef(parmodCPr) + 0.5 * (cplm::VarCorr(parmodCPr)$Population[1] + cplm::VarCorr(parmodCPr)$Container[1]))
  VarOdF <- phiF * mu^(pF - 2) # the delta method
  VarOlF <- log(1 + (phiF * mu^(pF - 2)))

  # lognormal
  R2glmmM <- VarF / (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf))) + VarOlF)
  R2glmmC <- (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf)))) / (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf))) + VarOlF)
  out <- performance::r2_nakagawa(parmodCPf, null_model = parmodCPr)
  expect_equal(out$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # delta
  R2glmmM <- VarF / (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf))) + VarOdF)
  R2glmmC <- (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf)))) / (VarF + sum(as.numeric(cplm::VarCorr(parmodCPf))) + VarOdF)
  out2 <- performance::r2_nakagawa(parmodCPf, null_model = parmodCPr, approximation = "delta")
  expect_equal(out2$R2_conditional, R2glmmC, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out2$R2_marginal, R2glmmM, tolerance = 1e-4, ignore_attr = TRUE)

  # results for glmmTMB are very close to cplm
  m0 <- glmmTMB::glmmTMB(
    Parasite ~ (1 | Population) + (1 | Container),
    family = glmmTMB::tweedie(),
    start = list(psi = -12),
    map = list(psi = factor(NA)),
    data = DataAll
  )
  m <- glmmTMB::glmmTMB(
    Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
    family = glmmTMB::tweedie(),
    start = list(psi = -12),
    map = list(psi = factor(NA)),
    data = DataAll
  )
  out3 <- performance::r2_nakagawa(m, null_model = m0)
  expect_equal(out$R2_conditional, out3$R2_conditional, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, out3$R2_marginal, tolerance = 1e-1, ignore_attr = TRUE)
})
