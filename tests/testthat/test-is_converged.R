skip_if_not_installed("lme4")

data(cbpp, package = "lme4")
data(sleepstudy, package = "lme4")
set.seed(1)
cbpp$x <- rnorm(nrow(cbpp))
cbpp$x2 <- runif(nrow(cbpp))

model <- suppressMessages(suppressWarnings(lme4::glmer(
  cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
  data = cbpp,
  family = binomial()
)))

test_that("is_converged", {
  expect_true(is_converged(model))
  expect_equal(is_converged(model), structure(TRUE, gradient = 0.000280307452338331), tolerance = 1e-3)
})

model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

test_that("is_converged", {
  expect_true(is_converged(model))
})


skip_on_os("mac") # error: FreeADFunObject
skip_on_cran() ## FIXME: check with win-devel
skip_if_not_installed("glmmTMB")
skip_if_not_installed("TMB")

data(sleepstudy, package = "lme4")
model <- glmmTMB::glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
test_that("is_converged, glmmTMB", {
  expect_true(is_converged(model))
})
