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
  expect_equal(
    is_converged(model),
    structure(TRUE, gradient = NA_real_),
    tolerance = 1e-3
  )
})

model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

test_that("is_converged", {
  expect_true(is_converged(model))
})

# Test singular model (from the issue)
d <- data.frame(y = 1:10, f = factor(rep(1:3, length.out = 10)))
model_singular <- suppressWarnings(lme4::lmer(y ~ 1 + (1 | f), data = d))

test_that("is_converged handles singular models", {
  # Should not error
  result <- is_converged(model_singular)
  # Result should be either TRUE (if optimizer converged) or NA (if derivs unavailable)
  expect_true(is.logical(result) || is.na(result))
  # If it's TRUE or NA, gradient attribute should be NA
  if (is.na(result) || isTRUE(result)) {
    expect_true(is.na(attr(result, "gradient")))
  }
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
