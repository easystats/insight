skip_on_cran()

skip_if_not_installed("MuMIn")
skip_if_not_installed("performance", minimum_version = "0.12.1")

# ==============================================================================
# Gamma mixed models, glmmTMB ----
# ==============================================================================

# ==============================================================================
# Validate against Nakagawa et al. 2017 paper!
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
