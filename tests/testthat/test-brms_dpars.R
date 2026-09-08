skip_if_not_installed("brms")

# Minimal "brmsfit"-mockups, to test the detection of auxiliary (distributional)
# parameters without the need of fitting (or downloading) a model.
# `find_auxiliary()` and `clean_parameters()` only require the model's formula
# and the parameter names of the related stan-model.
.brmsfit_mock <- function(formula, parameters) {
  fit <- array(
    0,
    dim = c(1, 1, length(parameters)),
    dimnames = list(iterations = NULL, chains = NULL, parameters = parameters)
  )
  structure(list(formula = formula, fit = fit, family = NULL), class = "brmsfit")
}


test_that("find_auxiliary, sigma estimated as constant parameter", {
  m <- .brmsfit_mock(
    brms::bf(mpg ~ hp),
    c("b_Intercept", "b_hp", "sigma", "Intercept", "lprior", "lp__")
  )
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_auxiliary, sigma modelled as distributional parameter", {
  m <- .brmsfit_mock(
    brms::bf(mpg ~ hp, sigma ~ cyl),
    c("b_Intercept", "b_hp", "b_sigma_Intercept", "b_sigma_cyl", "Intercept_sigma")
  )
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_auxiliary, sigma in multivariate models", {
  m <- .brmsfit_mock(
    brms::bf(Sepal.Length ~ Petal.Length) + brms::bf(Sepal.Width ~ Species),
    c(
      "b_SepalLength_Intercept", "b_SepalLength_Petal.Length",
      "b_SepalWidth_Intercept", "b_SepalWidth_Speciesversicolor",
      "sigma_SepalLength", "sigma_SepalWidth"
    )
  )
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_auxiliary, sigma in mixture models", {
  m <- .brmsfit_mock(
    brms::bf(mpg ~ hp),
    c("b_mu1_Intercept", "b_mu2_Intercept", "sigma1", "sigma2", "theta1", "theta2")
  )
  expect_identical(find_auxiliary(m), "sigma")
})


test_that("find_auxiliary, no sigma for models without residual SD", {
  m <- .brmsfit_mock(
    brms::bf(count ~ Trt),
    c("b_Intercept", "b_Trt1", "Intercept", "lprior", "lp__")
  )
  expect_null(find_auxiliary(m))
})


test_that("find_auxiliary does not mistake custom dpars for sigma, #1224", {
  # custom families may have auxiliary parameters that only *contain* the
  # word "sigma" - these must not be detected as "sigma"
  m <- .brmsfit_mock(
    brms::bf(rt ~ Condition, boundary ~ Condition, bias ~ 1, ndt ~ 1),
    c(
      "b_Intercept", "b_boundary_Intercept", "b_bias_Intercept",
      "b_ndt_Intercept", "sigmadrift", "sigmabias", "sigmandt", "poutlier"
    )
  )
  expect_identical(find_auxiliary(m), c("boundary", "bias", "ndt"))

  # "sigmabias" is modelled, but there still is no "sigma" in the model
  m <- .brmsfit_mock(
    brms::bf(rt ~ Condition, sigmabias ~ Condition, boundary ~ 1, ndt ~ 1),
    c(
      "b_Intercept", "b_sigmabias_Intercept", "b_boundary_Intercept",
      "b_ndt_Intercept", "sigmadrift", "poutlier"
    )
  )
  expect_identical(find_auxiliary(m), c("sigmabias", "boundary", "ndt"))
})


test_that("find_auxiliary, default method", {
  expect_warning(
    find_auxiliary(lm(mpg ~ hp, data = mtcars)),
    regex = "only works for"
  )
  expect_null(find_auxiliary(lm(mpg ~ hp, data = mtcars), verbose = FALSE))
})


test_that("clean_parameters does not lump custom dpars into sigma, #1224", {
  # "sigmabias" is a distributional parameter of its own, and must not be
  # assigned to the "sigma" component, although the model *does* have a sigma
  m <- .brmsfit_mock(
    brms::bf(rt ~ Condition, sigmabias ~ Condition, boundary ~ Condition, ndt ~ 1),
    c(
      "b_Intercept", "b_ConditionSpeed",
      "b_sigmabias_Intercept", "b_sigmabias_ConditionSpeed",
      "b_boundary_Intercept", "b_boundary_ConditionSpeed",
      "b_ndt_Intercept", "sigma"
    )
  )
  out <- clean_parameters(m)
  expect_identical(
    out$Component,
    c(
      "conditional", "conditional", "ndt", "sigma", "sigmabias", "sigmabias",
      "boundary", "boundary"
    )
  )
  expect_identical(
    out$Cleaned_Parameter,
    c(
      "(Intercept)", "ConditionSpeed", "(Intercept)", "sigma", "(Intercept)",
      "ConditionSpeed", "(Intercept)", "ConditionSpeed"
    )
  )
})


test_that("clean_parameters keeps sigma and its random effects together", {
  m <- .brmsfit_mock(
    brms::bf(y ~ x, sigma ~ x + (1 | id)),
    c(
      "b_Intercept", "b_x", "b_sigma_Intercept", "b_sigma_x",
      "sd_id__sigma_Intercept", "r_id__sigma[1,Intercept]"
    )
  )
  out <- clean_parameters(m)
  expect_identical(
    out$Component,
    c("conditional", "conditional", "sigma", "sigma", "sigma", "sigma")
  )
  expect_identical(
    out$Effects,
    c("fixed", "fixed", "fixed", "fixed", "random", "random")
  )
})


test_that(".get_stan_params maps component names for all supported classes", {
  # element names that `find_parameters()` returns for the model classes that
  # share this helper (brmsfit, stanreg, stanfit, stanmvreg and bamlss), plus
  # their group-level ("_random") counterparts
  elements <- c(
    "conditional", "random", "conditional_random", "sigma", "sigma_random",
    "smooth_terms", "smooth_terms_random", "auxiliary", "alpha", "priors",
    "dispersion", "dispersion_random", "zi", "zi_random", "car", "sdcar",
    # auxiliary parameters of custom families that merely *contain* the name
    # of a known component
    "sigmabias", "sigmabias_random", "sigmadrift"
  )
  pars <- stats::setNames(as.list(elements), elements)
  out <- do.call(rbind, insight:::.get_stan_params(pars))

  expect_identical(
    out$Component,
    c(
      "conditional", "conditional", "conditional", "sigma", "sigma",
      "smooth_terms", "smooth_terms", "auxiliary", "alpha", "priors",
      "dispersion", "dispersion", "zi", "zi", "car", "car",
      "sigmabias", "sigmabias", "sigmadrift"
    )
  )
  expect_identical(
    out$Effects[endsWith(elements, "_random") | elements == "random"],
    rep("random", 7)
  )
})
