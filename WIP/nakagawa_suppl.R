vsCodeSnippets::load_debug_pkg()

# installing glmmADMB
library(R2admb)
library(glmmADMB)
library(lme4)
library(cplm)
library(performance)
library(insight)
library(glmmTMB)


set.seed(1234)
# 12 different populations n = 960
Population <- gl(12, 80, 960)
# 120 containers (8 individuals in each container)
Container <- gl(120, 8, 960)
# Sex of the individuals. Uni-sex within each container (individuals are
# sorted at the pupa stage)
Sex <- factor(rep(rep(c("Female", "Male"), each = 8), 60))
# Habitat at the collection site: dry or wet soil (four indiviudal from each
# Habitat in each container)
Habitat <- factor(rep(rep(c("Dry", "Wet"), each = 4), 120))
# Food treatment at the larval stage: special food ('Exp') or standard food
# ('Cont')
Treatment <- factor(rep(c("Cont", "Exp"), 480))
# Data combined in a data frame
Data <- data.frame(
  Population = Population, Container = Container, Sex = Sex,
  Habitat = Habitat, Treatment = Treatment
)

# Subset the design matrix (only females lay eggs)
DataFemale <- Data[Data$Sex == "Female", ]
# set seed for reproduciblity (this will enable one to get the same data
# every time)
set.seed(777)
# simulation of the underlying random effects (Population and Container with
# variance of 0.4 and 0.05, respectively)
PopulationE <- rnorm(12, 0, sqrt(0.4))
ContainerE <- rnorm(120, 0, sqrt(0.05))
# generation of response values on latent scale (!) based on fixed effects,
# random effects and residual errors
EggL <- with(DataFemale, 1.1 + 0.5 * (as.numeric(Treatment) - 1) + 0.1 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(480, 0, sqrt(0.1)))
# data generation (on data scale!) based on Poisson distribution
DataFemale$Egg <- rpois(length(EggL), exp(EggL))

# Data frame for both sex
DataAll <- Data
# simulation of the underlying random effects (Population and Container with
# variance of 0.5 and 0.8, respectively)
PopulationE <- rnorm(12, 0, sqrt(0.5))
ContainerE <- rnorm(120, 0, sqrt(0.8))
# generation of response values on latent scale (!) based on fixed effects
# and random effects
ParasiteL <- with(DataAll, 1.8 + 2 * (-1) * (as.numeric(Sex) - 1) + 0.8 * (-1) * (as.numeric(Treatment) - 1) + 0.7 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
# data generation (on data scale!) based on negative binomial distributions;
# size = theta
DataAll$Parasite <- rnbinom(length(ParasiteL), size = 5, mu = exp(ParasiteL))

# simulation of the underlying random effects (Population and Container with
# variance of 1.3 and 0.3, respectively)
PopulationE <- rnorm(12, 0, sqrt(1.3))
ContainerE <- rnorm(120, 0, sqrt(0.3))
# data generation based on fixed effects, random effects and random
# residuals errors
DataAll$BodyL <- 15 + 3 * (-1) * (as.numeric(Sex) - 1) + 0.4 * (as.numeric(Treatment) - 1) + 0.15 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(960, 0, sqrt(1.2))
# simulation of the underlying random effects (Population and Container with
# variance of 0.2 and 0.2, respectively)
PopulationE <- rnorm(12, 0, sqrt(0.2))
ContainerE <- rnorm(120, 0, sqrt(0.2))
# generation of response values on latent scale (!) based on fixed effects
# and random effects
ExplorationL <- with(DataAll, 4 + 1 * (-1) * (as.numeric(Sex) - 1) + 2 * (as.numeric(Treatment) - 1) + 0.5 * (-1) * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
# data generation (on data scale!) based on gamma distribution; size = theta
DataAll$Exploration <- rgamma(length(ExplorationL), shape = exp(ExplorationL) * 0.3, rate = 0.3)

# Subset the design matrix (only males express colour morphs)
DataMale <- subset(Data, Sex == "Male")
# simulation of the underlying random effects (Population and Container with
# variance of 1.2 and 0.2, respectively)
PopulationE <- rnorm(12, 0, sqrt(1.2))
ContainerE <- rnorm(120, 0, sqrt(0.2))
# generation of response values on latent scale (!) based on fixed effects
# and random effects
ColourL <- with(DataMale, 0.8 * (-1) + 0.8 * (as.numeric(Treatment) - 1) + 0.5 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
# data generation (on data scale!) based on binomial distribution
DataMale$Colour <- rbinom(length(ColourL), 1, plogis(ColourL))



# ==============================================================
# Quasi-Poisson with log link (page 5) -------------------------
# glmmadmb -----------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
fecmodADMBr <- glmmadmb(
  Egg ~ 1 + (1 | Population) + (1 | Container),
  family = "nbinom1", data = DataFemale
)
# Fit alternative model including fixed and all random effects
fecmodADMBf <- glmmadmb(
  Egg ~ Treatment + Habitat + (1 | Population) + (1 | Container),
  family = "nbinom1", data = DataFemale
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(model.matrix(fecmodADMBf) %*% fixef(fecmodADMBf)))
# getting the observation-level variance Null model
omegaN <- fecmodADMBr$alpha # overdispersion omega is alpha in glmmadmb
lambda <- as.numeric(exp(fixef(fecmodADMBr) + 0.5 * (as.numeric(VarCorr(fecmodADMBr)[1]) + as.numeric(VarCorr(fecmodADMBr)[2]))))
# lambda2 <- mean(DataFemale$Egg) # for lambda we use the mean of all
# observations
VarOdN <- omegaN / lambda # the delta method
VarOlN <- log(1 + omegaN / lambda) # log-normal approximation
VarOtN <- trigamma(lambda / omegaN) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

# Full model
omegaF <- fecmodADMBf$alpha # overdispersion omega is alpha in glmmadmb
VarOdF <- omegaF / lambda # the delta method
VarOlF <- log(1 + omegaF / lambda) # log-normal approximation
VarOtF <- trigamma(lambda / omegaF) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(fecmodADMBf))) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(fecmodADMBf)))) / (VarF + sum(as.numeric(VarCorr(fecmodADMBf))) + VarOlF)
# Raw unadjusted ICC[Population]
ICCrawPop <- VarCorr(fecmodADMBr)$Population[1] / (sum(as.numeric(VarCorr(fecmodADMBr))) + VarOlN)
# adjusted ICC[Population]
ICCadjPop <- VarCorr(fecmodADMBf)$Population[1] / (sum(as.numeric(VarCorr(fecmodADMBf))) + VarOlF)
# Raw unadjusted ICC[Container]
ICCrawCont <- VarCorr(fecmodADMBr)$Container[1] / (sum(as.numeric(VarCorr(fecmodADMBr))) + VarOlN)
# adjusted ICC[Container]
ICCadjCont <- VarCorr(fecmodADMBf)$Container[1] / (sum(as.numeric(VarCorr(fecmodADMBf))) + VarOlF)
# comparing the results
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC, ICCrawPop = ICCrawPop, ICCadjPop = ICCadjPop, ICCrawCont = ICCrawCont, ICCadjCont = ICCadjCont)

performance::r2_nakagawa(fecmodADMBf)


# ==============================================================
# Quasi-Poisson with log link (page 7) -------------------------
# glmmPQL- -----------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
fecmodPQLr <- glmmPQL(Egg ~ 1,
  random = list(~ 1 | Population, ~ 1 | Container),
  family = "quasipoisson", data = DataFemale
)
# Fit alternative model including fixed and all random effects
fecmodPQLf <- glmmPQL(Egg ~ Treatment + Habitat, random = list(
  ~ 1 | Population,
  ~ 1 | Container
), family = "quasipoisson", data = DataFemale)

# Calculation of the variance in fitted values
VarF <- var(as.vector(model.matrix(~ Treatment + Habitat, data = DataFemale) %*% fixef(fecmodPQLf)))
# getting the observation-level variance Null model
omegaN <- as.numeric(VarCorr(fecmodPQLr)[5, 1]) # overdispersion omega is residual variance in glmmPQL
lambda <- as.numeric(exp(fixef(fecmodPQLr) + 0.5 * (as.numeric(VarCorr(fecmodPQLr)[2, 1]) + as.numeric(VarCorr(fecmodPQLr)[4, 1]))))
# lambda2 <- mean(DataFemale$Egg)
VarOdN <- omegaN / lambda

VarOlN <- log(1 + omegaN / lambda)
VarOtN <- trigamma(lambda / omegaN)
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

omegaF <- as.numeric(VarCorr(fecmodPQLf)[5, 1]) # overdispersion omega is residual variance in glmmPQL
VarOdF <- omegaF / lambda
VarOlF <- log(1 + omegaF / lambda)
VarOtF <- trigamma(lambda / omegaF)
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(fecmodPQLf)[c(2, 4), 1])) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(fecmodPQLf)[c(2, 4), 1]))) / (VarF + sum(as.numeric(VarCorr(fecmodPQLf)[c(2, 4), 1])) + VarOlF)
# Raw unadjusted ICC[Population]
ICCrawPop <- as.numeric(VarCorr(fecmodPQLr)[2, 1]) / (sum(as.numeric(VarCorr(fecmodPQLr)[c(2, 4), 1])) + VarOlN)
# adjusted ICC[Population]
ICCadjPop <- as.numeric(VarCorr(fecmodPQLf)[2, 1]) / (sum(as.numeric(VarCorr(fecmodPQLf)[c(2, 4), 1])) + VarOlF)
# Raw unadjusted ICC[Container]
ICCrawCont <- as.numeric(VarCorr(fecmodPQLr)[4, 1]) / (sum(as.numeric(VarCorr(fecmodPQLr)[c(2, 4), 1])) + VarOlN)
# adjusted ICC[Container]
ICCadjCont <- as.numeric(VarCorr(fecmodPQLf)[4, 1]) / (sum(as.numeric(VarCorr(fecmodPQLf)[c(2, 4), 1])) + VarOlF)
# comparing the results
c(
  R2glmmM = R2glmmM, R2glmmC = R2glmmC, ICCrawPop = ICCrawPop, ICCadjPop = ICCadjPop,
  ICCrawCont = ICCrawCont, ICCadjCont = ICCadjCont
)

performance::r2_nakagawa(fecmodPQLf, null_model = fecmodPQLr)


# ==============================================================
# Neg bin with log link (page 9) -------------------------
# glmmadmb- -----------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
parmodADMBr <- glmmadmb(
  Parasite ~ 1 + (1 | Population) + (1 | Container),
  family = "nbinom2", data = DataAll
)
# Fit alternative model including fixed and all random effects
parmodADMBf <- glmmadmb(
  Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  family = "nbinom2", data = DataAll
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(model.matrix(parmodADMBf) %*% fixef(parmodADMBf)))
# getting the observation-level variance Null model
thetaN <- parmodADMBr$alpha # note that theta is called alpha in glmmadmb
lambda <- as.numeric(exp(fixef(parmodADMBr) + 0.5 * (as.numeric(VarCorr(parmodADMBr)[1]) + as.numeric(VarCorr(parmodADMBr)[2]))))
# lambda2 <- mean(DataAll$Parasite)
VarOdN <- 1 / lambda + 1 / thetaN # the delta method
VarOlN <- log(1 + (1 / lambda) + (1 / thetaN)) # log-normal approximation
VarOtN <- trigamma((1 / lambda + 1 / thetaN)^(-1)) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

thetaF <- parmodADMBf$alpha # note that theta is called alpha in glmmadmb
VarOdF <- 1 / lambda + 1 / thetaF # the delta method
VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
VarOtF <- trigamma((1 / lambda + 1 / thetaF)^(-1)) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(parmodADMBf))) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(parmodADMBf)))) / (VarF + sum(as.numeric(VarCorr(parmodADMBf))) + VarOlF)
# Raw unadjusted ICC[Population]
ICCrawPop <- VarCorr(parmodADMBr)$Population[1] / (sum(as.numeric(VarCorr(parmodADMBr))) + VarOlN)
# adjusted ICC[Population]
ICCadjPop <- VarCorr(parmodADMBf)$Population[1] / (sum(as.numeric(VarCorr(parmodADMBf))) + VarOlF)
# Raw unadjusted ICC[Container]
ICCrawCont <- VarCorr(parmodADMBr)$Container[1] / (sum(as.numeric(VarCorr(parmodADMBr))) + VarOlN)
# adjusted ICC[Container]
ICCadjCont <- VarCorr(parmodADMBf)$Container[1] / (sum(as.numeric(VarCorr(parmodADMBf))) + VarOlF)
# comparing the results
c(
  R2glmmM = R2glmmM, R2glmmC = R2glmmC, ICCrawPop = ICCrawPop,
  ICCadjPop = ICCadjPop, ICCrawCont = ICCrawCont, ICCadjCont = ICCadjCont
)

performance::r2_nakagawa(parmodADMBf, null_model = parmodADMBr)


# ==============================================================
# Neg bin with log link (page 9) -------------------------
# glmmTMB ------------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
glmmTMBr <- glmmTMB(
  Parasite ~ 1 + (1 | Population) + (1 | Container),
  family = "nbinom2", data = DataAll, REML = TRUE
)
# Fit alternative model including fixed and all random effects
glmmTMBf <- glmmTMB(
  Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  family = "nbinom2", data = DataAll, REML = TRUE
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% fixef(glmmTMBf)$cond))
# getting the observation-level variance Null model
thetaN <- sigma(glmmTMBr)
lambda <- as.numeric(exp(fixef(glmmTMBr)$cond + 0.5 * (as.numeric(VarCorr(glmmTMBr)$cond[1]) + as.numeric(VarCorr(glmmTMBr)$cond[2]))))
# lambda2 <- mean(DataAll$Parasite)
VarOdN <- 1 / lambda + 1 / thetaN # the delta method
VarOlN <- log(1 + (1 / lambda) + (1 / thetaN)) # log-normal approximation
VarOtN <- trigamma((1 / lambda + 1 / thetaN)^(-1)) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
VarOdF <- 1 / lambda + 1 / thetaF # the delta method
VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
VarOtF <- trigamma((1 / lambda + 1 / thetaF)^(-1)) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC)

performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)


# ==============================================================
# Neg bin1 with log link (page 9) -------------------------
# glmmTMB ------------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
glmmTMBr <- glmmTMB(
  Parasite ~ 1 + (1 | Population) + (1 | Container),
  family = "nbinom1", data = DataAll, REML = TRUE
)
# Fit alternative model including fixed and all random effects
glmmTMBf <- glmmTMB(
  Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  family = "nbinom1", data = DataAll, REML = TRUE
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% fixef(glmmTMBf)$cond))
# getting the observation-level variance Null model
thetaN <- sigma(glmmTMBr)
lambda <- as.numeric(exp(fixef(glmmTMBr)$cond + 0.5 * (as.numeric(VarCorr(glmmTMBr)$cond[1]) + as.numeric(VarCorr(glmmTMBr)$cond[2]))))
# lambda2 <- mean(DataAll$Parasite)
VarOdN <- 1 / lambda + 1 / thetaN # the delta method
VarOlN <- log(1 + (1 / lambda) + (1 / thetaN)) # log-normal approximation
VarOtN <- trigamma((1 / lambda + 1 / thetaN)^(-1)) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
VarOdF <- 1 / lambda + 1 / thetaF # the delta method
VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
VarOtF <- trigamma((1 / lambda + 1 / thetaF)^(-1)) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC)

performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)


# ==============================================================
# Neg bin with log link (page 12) -------------------------
# glmer.nb ------------------------------------------------------
# ==============================================================

# Fit null model without fixed effects (but including all random effects)
parmodGLMERr <- glmer.nb(Parasite ~ (1 | Population) + (1 | Container),
  data = DataAll
)
# Fit alternative model including fixed and all random effects
parmodGLMERf <- glmer.nb(
  Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  data = DataAll
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(model.matrix(parmodGLMERf) %*% fixef(parmodGLMERf)))
# getting the observation-level variance Null model
thetaN <- getME(parmodGLMERr, "glmer.nb.theta")
lambda <- as.numeric(exp(fixef(parmodGLMERr) + 0.5 * (as.numeric(VarCorr(parmodGLMERr)$Population) + as.numeric(VarCorr(parmodGLMERr)$Container))))
# lambda2 <- mean(DataAll$Parasite)
VarOdN <- 1 / lambda + 1 / thetaN # the delta method
VarOlN <- log(1 + (1 / lambda) + (1 / thetaN)) # log-normal approximation
VarOtN <- trigamma((1 / lambda + 1 / thetaN)^(-1)) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)
# Full model
thetaF <- getME(parmodGLMERf, "glmer.nb.theta")
VarOdF <- 1 / lambda + 1 / thetaF # the delta method
VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
VarOtF <- trigamma((1 / lambda + 1 / thetaF)^-1) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(parmodGLMERf))) + VarOlF)
R2glmmC <- (VarF + sum(as.numeric(VarCorr(parmodGLMERf)))) / (VarF + sum(as.numeric(VarCorr(parmodGLMERf)) + VarOlF))
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC)

performance::r2_nakagawa(parmodGLMERf)
MuMIn::r.squaredGLMM(parmodGLMERf)






# Fit null model without fixed effects (but including all random effects)
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
# Calculation of the variance in fitted values
VarF <- var(as.vector(get_modelmatrix(glmmTMBf) %*% fixef(glmmTMBf)$cond))
# getting the observation-level variance Null model
thetaN <- sigma(glmmTMBr)
lambda <- as.numeric(exp(fixef(glmmTMBr)$cond + 0.5 * (as.numeric(VarCorr(glmmTMBr)$cond[1]))))
# lambda2 <- mean(DataAll$Parasite)
VarOdN <- 1 / lambda + 1 / thetaN # the delta method
VarOlN <- log(1 + (1 / lambda) + (1 / thetaN)) # log-normal approximation
VarOtN <- trigamma((1 / lambda + 1 / thetaN)^(-1)) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)

thetaF <- sigma(glmmTMBf) # note that theta is called alpha in glmmadmb
VarOdF <- 1 / lambda + 1 / thetaF # the delta method
VarOlF <- log(1 + (1 / lambda) + (1 / thetaF)) # log-normal approximation
VarOtF <- trigamma((1 / lambda + 1 / thetaF)^(-1)) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)

# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond))) / (VarF + sum(as.numeric(VarCorr(glmmTMBf)$cond)) + VarOlF)
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC)

performance::r2_nakagawa(glmmTMBf, null_model = glmmTMBr)
MuMIn::r.squaredGLMM(glmmTMBf)
get_variance(glmmTMBf, null_model = glmmTMBr)





sizemodeGLMERr <- glmer(
  BodyL ~ 1 + (1 | Population) + (1 | Container),
  family = Gamma(link = log),
  data = DataAll
)
# Fit alternative model including fixed and all random effects
sizemodeGLMERf <- glmer(
  BodyL ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  family = Gamma(link = log), data = DataAll
)


VarF <- var(as.vector(model.matrix(sizemodeGLMERf) %*% fixef(sizemodeGLMERf)))
# getting the observation-level variance Null model
nuN <- 1 / attr(VarCorr(sizemodeGLMERr), "sc")^2 # note that glmer report 1/nu not nu as resiudal varian
VarOdN <- 1 / nuN # the delta method
VarOlN <- log(1 + 1 / nuN) # log-normal approximation
VarOtN <- trigamma(nuN) # trigamma function
# comparing the three
c(VarOdN = VarOdN, VarOlN = VarOlN, VarOtN = VarOtN)
## VarOdN VarOlN VarOtN
## 0.008370998 0.008336156 0.008406133
# Full model
nuF <- 1 / attr(VarCorr(sizemodeGLMERf), "sc")^2 # note that glmer report 1/nu not nu as resiudal varian
VarOdF <- 1 / nuF # the delta method
VarOlF <- log(1 + 1 / nuF) # log-normal approximation
VarOtF <- trigamma(nuF) # trigamma function
# comparing the three
c(VarOdF = VarOdF, VarOlF = VarOlF, VarOtF = VarOtF)
## VarOdF VarOlF VarOtF
## 0.006750704 0.006728020 0.006773541
# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(sizemodeGLMERf))) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(sizemodeGLMERf)))) / (VarF + sum(as.numeric(VarCorr(sizemodeGLMERf))) + VarOlF)
c(R2glmmM = R2glmmM, R2glmmC = R2glmmC)
performance::r2_nakagawa(sizemodeGLMERf, null_model = sizemodeGLMERr)




# Fit null model without fixed effects (but including all random effects)
parmodCPr <- cpglmm(
  Parasite ~ 1 + (1 | Population) + (1 | Container),
  link = 0,
  data = DataAll
)
# Fit alternative model including fixed and all random effects
parmodCPf <- cpglmm(
  Parasite ~ Sex + Treatment + Habitat + (1 | Population) + (1 | Container),
  link = 0,
  data = DataAll
)

# Calculation of the variance in fitted values
VarF <- var(as.vector(model.matrix(parmodCPf) %*% fixef(parmodCPf)))
phiF <- parmodCPf@phi # the dispersion parameter
pF <- parmodCPf@p # the index parameter
VarOdF <- phiF * mu^(pF - 2) # the delta method
VarOlF <- log(1 + (phiF * mu^(pF - 2)))

R2glmmM <- VarF / (VarF + sum(as.numeric(VarCorr(parmodCPf))) + VarOlF)
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(parmodCPf)))) / (VarF + sum(as.numeric(VarCorr(parmodCPf))) + VarOlF)

performance::r2_nakagawa(parmodCPf, null_model = parmodCPr)

model_info(parmodCPf)
get_sigma(parmodCPf)
parse(text = safe_deparse(parmodCPf@call))[[1]]$link
x <- parmodCPf
x
