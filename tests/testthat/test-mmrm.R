skip_if_not_installed("mmrm")

# see https://github.com/georgheinze/logistf/pull/54
skip_if(
  "as.character.formula" %in% methods(as.character),
  "Package `logistf` is loaded and breaks `mmrm::mmrm()`"
)

data(fev_data, package = "mmrm")
mod_mmrm <- mmrm::mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

test_that("model_info", {
  expect_true(model_info(mod_mmrm)$is_linear)
  expect_true(model_info(mod_mmrm)$is_mixed)
})

test_that("loglik", {
  expect_equal(get_loglikelihood(mod_mmrm, estimator = "REML"), logLik(mod_mmrm), ignore_attr = TRUE)
  expect_equal(get_loglikelihood(mod_mmrm), logLik(mod_mmrm), ignore_attr = TRUE)
})

test_that("get_df", {
  expect_equal(get_df(mod_mmrm), c(
    218.8018, 168.6657, 157.1382, 166.1324, 145.552, 143.8758,
    155.5575, 138.4708, 138.5643, 158.1651, 129.7204
  ), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(get_df(mod_mmrm, type = "model"), 12, ignore_attr = TRUE)
})

test_that("n_parameters", {
  expect_identical(n_parameters(mod_mmrm), 11L)
})

test_that("find_offset", {
  model_off <- mmrm::mmrm(
    log(FEV1) ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID) + offset(log(FEV1_BL)),
    data = fev_data
  )
  expect_identical(find_offset(model_off), "FEV1_BL")
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(mod_mmrm, effects = "all"),
    list(conditional = c("RACE", "SEX", "ARMCD", "AVISIT"), random = "USUBJID")
  )
  expect_identical(
    find_predictors(mod_mmrm, effects = "all", flatten = TRUE),
    c("RACE", "SEX", "ARMCD", "AVISIT", "USUBJID")
  )
  expect_identical(
    find_predictors(mod_mmrm, effects = "fixed"),
    list(conditional = c("RACE", "SEX", "ARMCD", "AVISIT"))
  )
  expect_identical(
    find_predictors(mod_mmrm, effects = "fixed", flatten = TRUE),
    c("RACE", "SEX", "ARMCD", "AVISIT")
  )
  expect_identical(
    find_predictors(mod_mmrm, effects = "random"),
    list(random = "USUBJID")
  )
  expect_identical(
    find_predictors(mod_mmrm, effects = "random", flatten = TRUE),
    "USUBJID"
  )
  expect_null(find_predictors(mod_mmrm, effects = "all", component = "zi"))
})

test_that("find_random", {
  expect_identical(find_random(mod_mmrm), list(random = "USUBJID"))
  expect_identical(find_random(mod_mmrm, flatten = TRUE), "USUBJID")
})

test_that("find_response", {
  expect_identical(find_response(mod_mmrm), "FEV1")
})

test_that("get_response", {
  expect_equal(get_response(mod_mmrm), na.omit(fev_data$FEV1), ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("link_inverse", {
  expect_identical(link_inverse(mod_mmrm)(0.2), 0.2)
})

test_that("get_data", {
  expect_identical(colnames(get_data(mod_mmrm)), c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT", "USUBJID"))
  expect_identical(colnames(get_data(mod_mmrm, effects = "fixed")), c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT"))
  expect_identical(colnames(get_data(mod_mmrm, effects = "random")), "USUBJID")
  expect_identical(dim(get_data(mod_mmrm)), c(537L, 6L))
})

test_that("find_formula", {
  expect_length(find_formula(mod_mmrm), 2)
  expect_equal(
    find_formula(mod_mmrm, component = "conditional"),
    list(
      conditional = as.formula("FEV1 ~ RACE + SEX + ARMCD * AVISIT"),
      random = as.formula("~AVISIT | USUBJID")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(mod_mmrm),
    list(response = "FEV1", conditional = c(
      "RACE", "SEX", "ARMCD",
      "AVISIT"
    ), random = c("AVISIT", "USUBJID"))
  )
  expect_identical(
    find_terms(mod_mmrm, flatten = TRUE),
    c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT", "USUBJID")
  )
})

test_that("get_predictors", {
  expect_identical(colnames(get_predictors(mod_mmrm)), c("RACE", "SEX", "ARMCD", "AVISIT"))
})

test_that("get_random", {
  expect_identical(colnames(get_random(mod_mmrm)), "USUBJID")
})

test_that("linkfun", {
  expect_false(is.null(link_function(mod_mmrm)))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(mod_mmrm),
    list(conditional = c(
      "(Intercept)", "RACEBlack or African American",
      "RACEWhite", "SEXFemale", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3",
      "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
    ))
  )
  expect_identical(nrow(get_parameters(mod_mmrm)), 11L)
  expect_identical(
    get_parameters(mod_mmrm)$Parameter,
    c(
      "(Intercept)", "RACEBlack or African American", "RACEWhite",
      "SEXFemale", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4",
      "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(mod_mmrm))
})

test_that("find_random_slopes", {
  expect_identical(find_random_slopes(mod_mmrm), list(random = "AVISIT"))
})

test_that("get_varcov", {
  expect_identical(dim(get_varcov(mod_mmrm)), c(11L, 11L))
})

test_that("find_statistic", {
  expect_identical(find_statistic(mod_mmrm), "t-statistic")
})
