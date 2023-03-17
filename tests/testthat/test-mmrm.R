.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && getRversion() >= "4.0.0") {
  skip_if_not_or_load_if_installed("mmrm")

  data(fev_data)
  m1 <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
    expect_true(model_info(m1)$is_mixed)
  })

  test_that("loglik", {
    expect_equal(get_loglikelihood(m1, estimator = "REML"), logLik(m1), ignore_attr = TRUE)
    expect_equal(get_loglikelihood(m1), logLik(m1), ignore_attr = TRUE)
  })

  test_that("get_df", {
    expect_equal(get_df(m1), c(
      218.8018, 168.6657, 157.1382, 166.1324, 145.552, 143.8758,
      155.5575, 138.4708, 138.5643, 158.1651, 129.7204
    ), ignore_attr = TRUE, tolerance = 1e-4)
    expect_equal(get_df(m1, type = "model"), 12, ignore_attr = TRUE)
  })

  test_that("n_parameters", {
    expect_identical(n_parameters(m1), 11L)
  })

  test_that("find_offset", {
    model_off <- mmrm(
      log(FEV1) ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID) + offset(log(FEV1_BL)),
      data = fev_data
    )
    expect_identical(find_offset(model_off), "FEV1_BL")
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1, effects = "all"),
      list(conditional = c("RACE", "SEX", "ARMCD", "AVISIT"), random = "USUBJID")
    )
    expect_identical(
      find_predictors(m1, effects = "all", flatten = TRUE),
      c("RACE", "SEX", "ARMCD", "AVISIT", "USUBJID")
    )
    expect_identical(
      find_predictors(m1, effects = "fixed"),
      list(conditional = c("RACE", "SEX", "ARMCD", "AVISIT"))
    )
    expect_identical(
      find_predictors(m1, effects = "fixed", flatten = TRUE),
      c("RACE", "SEX", "ARMCD", "AVISIT")
    )
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "USUBJID")
    )
    expect_identical(
      find_predictors(m1, effects = "random", flatten = TRUE),
      "USUBJID"
    )
    expect_null(find_predictors(m1, effects = "all", component = "zi"))
  })

  test_that("find_random", {
    expect_identical(find_random(m1), list(random = "USUBJID"))
    expect_identical(find_random(m1, flatten = TRUE), "USUBJID")
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "FEV1")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), na.omit(fev_data$FEV1), ignore_attr = TRUE, tolerance = 1e-4)
  })

  test_that("link_inverse", {
    expect_identical(link_inverse(m1)(0.2), 0.2)
  })

  test_that("get_data", {
    expect_identical(colnames(get_data(m1)), c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT", "USUBJID"))
    expect_identical(colnames(get_data(m1, effects = "fixed")), c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT"))
    expect_identical(colnames(get_data(m1, effects = "random")), "USUBJID")
    expect_identical(dim(get_data(m1)), c(537L, 6L))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1, component = "conditional"),
      list(
        conditional = as.formula("FEV1 ~ RACE + SEX + ARMCD * AVISIT"),
        random = as.formula("~AVISIT | USUBJID")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_identical(
      find_terms(m1),
      list(response = "FEV1", conditional = c(
        "RACE", "SEX", "ARMCD",
        "AVISIT"
      ), random = c("AVISIT", "USUBJID"))
    )
    expect_identical(
      find_terms(m1, flatten = TRUE),
      c("FEV1", "RACE", "SEX", "ARMCD", "AVISIT", "USUBJID")
    )
  })

  test_that("get_predictors", {
    expect_identical(colnames(get_predictors(m1)), c("RACE", "SEX", "ARMCD", "AVISIT"))
  })

  test_that("get_random", {
    expect_identical(colnames(get_random(m1)), "USUBJID")
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_identical(
      find_parameters(m1),
      list(conditional = c(
        "(Intercept)", "RACEBlack or African American",
        "RACEWhite", "SEXFemale", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3",
        "AVISITVIS4", "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
      ))
    )
    expect_identical(nrow(get_parameters(m1)), 11L)
    expect_identical(
      get_parameters(m1)$Parameter,
      c(
        "(Intercept)", "RACEBlack or African American", "RACEWhite",
        "SEXFemale", "ARMCDTRT", "AVISITVIS2", "AVISITVIS3", "AVISITVIS4",
        "ARMCDTRT:AVISITVIS2", "ARMCDTRT:AVISITVIS3", "ARMCDTRT:AVISITVIS4"
      )
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_random_slopes", {
    expect_identical(find_random_slopes(m1), list(random = "AVISIT"))
  })

  test_that("get_varcov", {
    expect_identical(dim(get_varcov(m1)), c(11L, 11L))
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "t-statistic")
  })
}
