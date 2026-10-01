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
      "b_SepalLength_Intercept",
      "b_SepalLength_Petal.Length",
      "b_SepalWidth_Intercept",
      "b_SepalWidth_Speciesversicolor",
      "sigma_SepalLength",
      "sigma_SepalWidth"
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
      "b_Intercept",
      "b_boundary_Intercept",
      "b_bias_Intercept",
      "b_ndt_Intercept",
      "sigmadrift",
      "sigmabias",
      "sigmandt",
      "poutlier"
    )
  )
  expect_identical(find_auxiliary(m), c("boundary", "bias", "ndt"))

  # "sigmabias" is modelled, but there still is no "sigma" in the model
  m <- .brmsfit_mock(
    brms::bf(rt ~ Condition, sigmabias ~ Condition, boundary ~ 1, ndt ~ 1),
    c(
      "b_Intercept",
      "b_sigmabias_Intercept",
      "b_boundary_Intercept",
      "b_ndt_Intercept",
      "sigmadrift",
      "poutlier"
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
      "b_Intercept",
      "b_ConditionSpeed",
      "b_sigmabias_Intercept",
      "b_sigmabias_ConditionSpeed",
      "b_boundary_Intercept",
      "b_boundary_ConditionSpeed",
      "b_ndt_Intercept",
      "sigma"
    )
  )
  out <- clean_parameters(m)
  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "ndt",
      "sigma",
      "sigmabias",
      "sigmabias",
      "boundary",
      "boundary"
    )
  )
  expect_identical(
    out$Cleaned_Parameter,
    c(
      "(Intercept)",
      "ConditionSpeed",
      "(Intercept)",
      "sigma",
      "(Intercept)",
      "ConditionSpeed",
      "(Intercept)",
      "ConditionSpeed"
    )
  )
})


test_that("clean_parameters keeps sigma and its random effects together", {
  m <- .brmsfit_mock(
    brms::bf(y ~ x, sigma ~ x + (1 | id)),
    c(
      "b_Intercept",
      "b_x",
      "b_sigma_Intercept",
      "b_sigma_x",
      "sd_id__sigma_Intercept",
      "r_id__sigma[1,Intercept]"
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


test_that("clean_parameters, correlated group-level terms of non-linear parameters, #1076", {
  # non-linear parameters "a" and "b" share a correlated group-level term
  m <- .brmsfit_mock(
    brms::bf(y ~ a * exp(b * x), a ~ 1 + (1 | p | id), b ~ 1 + (1 | p | id), nl = TRUE),
    c(
      "b_a_Intercept",
      "b_b_Intercept",
      "sd_id__a_Intercept",
      "sd_id__b_Intercept",
      "cor_id__a_Intercept__b_Intercept",
      "r_id__a[1,Intercept]",
      "r_id__b[1,Intercept]",
      "sigma"
    )
  )
  expect_identical(find_auxiliary(m), "sigma")
  out <- clean_parameters(m)
  rows <- match(
    c("b_a_Intercept", "sd_id__a_Intercept", "cor_id__a_Intercept__b_Intercept", "r_id__b[1,Intercept]"),
    out$Parameter
  )
  expect_identical(
    out$Cleaned_Parameter[rows],
    c("a_Intercept", "a_Intercept", "a_Intercept ~ b_Intercept", "id.1")
  )
  expect_identical(
    out$Group[rows],
    c("", "SD/Cor: id", "SD/Cor: id", "b_Intercept: id")
  )
  expect_true(all(out$Component[out$Parameter != "sigma"] == "conditional"))
})


test_that("clean_parameters, non-linear parameter names that end in 'sd', 'cor' or 'sigma', #1076", {
  # "ksd", "kcor" and "lsigma" contain the strings "sd_", "cor_" and "sigma_",
  # which are cleaned for group-level and sigma parameters
  m <- .brmsfit_mock(
    brms::bf(y ~ ksd * exp(kcor * x) + lsigma, ksd ~ 1, kcor ~ 1, lsigma ~ 1, nl = TRUE),
    c("b_ksd_Intercept", "b_kcor_Intercept", "b_lsigma_Intercept", "sigma")
  )
  out <- clean_parameters(m)
  rows <- match(c("b_ksd_Intercept", "b_kcor_Intercept", "b_lsigma_Intercept"), out$Parameter)
  expect_identical(
    out$Cleaned_Parameter[rows],
    c("ksd_Intercept", "kcor_Intercept", "lsigma_Intercept")
  )
  # no group-level terms, so there is no "SD/Cor" group
  expect_true(is.null(out$Group) || all(out$Group[rows] == ""))
  expect_identical(out$Component[rows], rep("conditional", 3))
})


test_that("clean_parameters, non-linear parameter name that starts with a dpar name, #1076", {
  # "sigmaA" starts with "sigma", and "sigma" has its own group-level term
  m <- .brmsfit_mock(
    brms::bf(
      y ~ a * exp(sigmaA * x),
      a ~ 1 + (1 | id),
      sigmaA ~ 1 + (1 | id),
      sigma ~ 1 + (1 | id),
      nl = TRUE
    ),
    c(
      "b_a_Intercept",
      "b_sigmaA_Intercept",
      "b_sigma_Intercept",
      "sd_id__a_Intercept",
      "sd_id__sigmaA_Intercept",
      "sd_id__sigma_Intercept",
      "r_id__a[1,Intercept]",
      "r_id__sigmaA[1,Intercept]",
      "r_id__sigma[1,Intercept]"
    )
  )
  out <- clean_parameters(m)
  rows <- match(
    c("b_sigmaA_Intercept", "sd_id__sigmaA_Intercept", "r_id__sigmaA[1,Intercept]"),
    out$Parameter
  )
  expect_identical(
    out$Cleaned_Parameter[rows],
    c("sigmaA_Intercept", "sigmaA_Intercept", "id.1")
  )
  expect_identical(out$Group[rows], c("", "SD/Cor: id", "sigmaA_Intercept: id"))
  expect_identical(out$Component[rows], rep("conditional", 3))
  # the group-level term of "sigma" itself keeps its labels
  sigma_row <- out[out$Parameter == "r_id__sigma[1,Intercept]", ]
  expect_identical(sigma_row$Group, "Intercept: id")
  expect_identical(sigma_row$Component, "sigma")
})


test_that("clean_parameters, non-linear parameter names that share a prefix ('a', 'ab'), #1076", {
  m <- .brmsfit_mock(
    brms::bf(y ~ a * exp(ab * x), a ~ 1 + (1 | id), ab ~ 1 + (1 | id), nl = TRUE),
    c(
      "b_a_Intercept",
      "b_ab_Intercept",
      "sd_id__a_Intercept",
      "sd_id__ab_Intercept",
      "r_id__a[1,Intercept]",
      "r_id__ab[1,Intercept]",
      "sigma"
    )
  )
  out <- clean_parameters(m)
  rows <- match(
    c("b_ab_Intercept", "sd_id__ab_Intercept", "r_id__a[1,Intercept]", "r_id__ab[1,Intercept]"),
    out$Parameter
  )
  expect_identical(
    out$Cleaned_Parameter[rows],
    c("ab_Intercept", "ab_Intercept", "id.1", "id.1")
  )
  expect_identical(
    out$Group[rows],
    c("", "SD/Cor: id", "a_Intercept: id", "ab_Intercept: id")
  )
})


test_that("find_auxiliary, nested non-linear parameters from nlf() are not auxiliary, #1076", {
  # "c" and "d" are non-linear parameters of the non-linear parameter "a"
  m <- .brmsfit_mock(
    brms::bf(
      y ~ a * exp(b * x),
      brms::nlf(a ~ c + d),
      c ~ 1 + (1 | g),
      d ~ 1,
      b ~ 1,
      nl = TRUE
    ),
    c(
      "b_c_Intercept",
      "b_d_Intercept",
      "b_b_Intercept",
      "sd_g__c_Intercept",
      "r_g__c[1,Intercept]",
      "sigma"
    )
  )
  expect_identical(find_auxiliary(m), "sigma")
  out <- find_parameters(m, effects = "full")
  expect_setequal(out$conditional, c("b_c_Intercept", "b_d_Intercept", "b_b_Intercept"))
  expect_setequal(out$random, c("r_g__c[1,Intercept]", "sd_g__c_Intercept"))
  expect_false(any(c("a", "b", "c", "d") %in% names(out)))
  cp <- clean_parameters(m)
  row <- cp[cp$Parameter == "r_g__c[1,Intercept]", ]
  expect_identical(row$Group, "c_Intercept: g")
  expect_identical(row$Component, "conditional")
})


test_that("find_auxiliary, non-linear parameters of sigma from nlf() stay auxiliary, #1076", {
  # only non-linear parameters of "mu" are conditional, "s" belongs to "sigma"
  m <- .brmsfit_mock(
    brms::bf(y ~ a * exp(b * x), a ~ 1, b ~ 1, brms::nlf(sigma ~ s * x), s ~ 1, nl = TRUE),
    c("b_a_Intercept", "b_b_Intercept", "b_s_Intercept")
  )
  expect_setequal(find_auxiliary(m), c("sigma", "s"))
})


test_that("find_auxiliary, non-linear model without auxiliary parameters returns NULL, #1076", {
  # no formula for a distributional parameter, and no "sigma" (e.g. poisson)
  m <- .brmsfit_mock(
    brms::bf(y ~ a * exp(b * x), a ~ 1, b ~ 1, nl = TRUE),
    c("b_a_Intercept", "b_b_Intercept")
  )
  expect_null(find_auxiliary(m))
})


test_that("clean_parameters, correlation of a non-linear and a 'sigma' group-level term, #1076", {
  m <- .brmsfit_mock(
    brms::bf(y ~ a * exp(b * x), a ~ 1 + (1 | p | id), b ~ 1, sigma ~ 1 + (1 | p | id), nl = TRUE),
    c(
      "b_a_Intercept",
      "b_b_Intercept",
      "b_sigma_Intercept",
      "sd_id__a_Intercept",
      "sd_id__sigma_Intercept",
      "cor_id__sigma_Intercept__a_Intercept",
      "r_id__a[1,Intercept]",
      "r_id__sigma[1,Intercept]"
    )
  )
  out <- clean_parameters(m)
  cor_row <- out[out$Parameter == "cor_id__sigma_Intercept__a_Intercept", ]
  expect_identical(cor_row$Cleaned_Parameter, "sigma_Intercept ~ a_Intercept")
  expect_identical(cor_row$Group, "SD/Cor: id")
})


test_that("clean_parameters, non-linear parameters named 'zi' and 'sd', #1076", {
  m <- .brmsfit_mock(
    brms::bf(y ~ zi * exp(sd * x), zi ~ 1 + (1 | id), sd ~ 1, nl = TRUE),
    c(
      "b_zi_Intercept",
      "b_sd_Intercept",
      "sd_id__zi_Intercept",
      "r_id__zi[1,Intercept]",
      "sigma"
    )
  )
  out <- clean_parameters(m)
  rows <- match(
    c("b_zi_Intercept", "b_sd_Intercept", "sd_id__zi_Intercept", "r_id__zi[1,Intercept]"),
    out$Parameter
  )
  expect_identical(
    out$Cleaned_Parameter[rows],
    c("zi_Intercept", "sd_Intercept", "zi_Intercept", "id.1")
  )
  expect_identical(out$Group[rows], c("", "", "SD/Cor: id", "zi_Intercept: id"))
  expect_identical(out$Component[rows], rep("conditional", 4))
})


test_that("find_parameters keeps parameters that start with a dpar name, #1226", {
  # a custom family with a distributional parameter "c" must not drop
  # conditional parameters of a predictor named, e.g., "condition"
  m <- .brmsfit_mock(
    brms::bf(N ~ condition + (1 + condition | ID), c ~ condition + (1 | ID)),
    c(
      "b_Intercept",
      "b_condition2",
      "b_c_Intercept",
      "b_c_condition2",
      "sd_ID__Intercept",
      "sd_ID__condition2",
      "sd_ID__c_Intercept",
      "cor_ID__Intercept__condition2",
      "r_ID[1,Intercept]",
      "r_ID[1,condition2]",
      "r_ID__c[1,Intercept]",
      "Intercept",
      "Intercept_c",
      "lprior",
      "lp__"
    )
  )
  out <- find_parameters(m, effects = "full")
  expect_identical(out$conditional, c("b_Intercept", "b_condition2"))
  expect_identical(
    out$random,
    c(
      "r_ID[1,Intercept]",
      "r_ID[1,condition2]",
      "sd_ID__Intercept",
      "sd_ID__condition2",
      "cor_ID__Intercept__condition2"
    )
  )
  expect_identical(out$c, c("b_c_Intercept", "b_c_condition2"))
  expect_identical(out$c_random, c("r_ID__c[1,Intercept]", "sd_ID__c_Intercept"))
})


test_that(".get_stan_params maps component names for all supported classes", {
  # element names that `find_parameters()` returns for the model classes that
  # share this helper (brmsfit, stanreg, stanfit, stanmvreg and bamlss), plus
  # their group-level ("_random") counterparts
  elements <- c(
    "conditional",
    "random",
    "conditional_random",
    "sigma",
    "sigma_random",
    "smooth_terms",
    "smooth_terms_random",
    "auxiliary",
    "alpha",
    "priors",
    "dispersion",
    "dispersion_random",
    "zi",
    "zi_random",
    "car",
    "sdcar",
    # auxiliary parameters of custom families that merely *contain* the name
    # of a known component
    "sigmabias",
    "sigmabias_random",
    "sigmadrift"
  )
  pars <- stats::setNames(as.list(elements), elements)
  out <- do.call(rbind, insight:::.get_stan_params(pars))

  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "sigma",
      "sigma",
      "smooth_terms",
      "smooth_terms",
      "auxiliary",
      "alpha",
      "priors",
      "dispersion",
      "dispersion",
      "zi",
      "zi",
      "car",
      "car",
      "sigmabias",
      "sigmabias",
      "sigmadrift"
    )
  )
  expect_identical(
    out$Effects[endsWith(elements, "_random") | elements == "random"],
    rep("random", 7)
  )
})
