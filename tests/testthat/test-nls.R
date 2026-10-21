dnase_run1 <- subset(datasets::DNase, Run == 1)

# selfStart model
nls_dnase1 <- stats::nls(
  density ~ SSlogis(log(conc), Asym, xmid, scal),
  data = dnase_run1
)

# partially linear model, the linear parameter is `.lin`
nls_dnase2 <- stats::nls(
  density ~ 1 / (1 + exp((xmid - log(conc)) / scal)),
  data = dnase_run1,
  start = list(xmid = 0, scal = 1),
  algorithm = "plinear"
)

test_that("find_parameters, n_parameters, nls", {
  expect_identical(
    find_parameters(nls_dnase1),
    list(conditional = c("Asym", "xmid", "scal"))
  )
  expect_identical(
    find_parameters(nls_dnase2),
    list(conditional = c("xmid", "scal", ".lin"))
  )
  expect_identical(n_parameters(nls_dnase1), 3L)
  expect_identical(n_parameters(nls_dnase2), 3L)
})

test_that("find_predictors, find_variables, find_terms, clean_names, nls", {
  for (m in list(nls_dnase1, nls_dnase2)) {
    expect_identical(find_predictors(m), list(conditional = "conc"))
    expect_identical(
      find_variables(m),
      list(response = "density", conditional = "conc")
    )
    expect_identical(
      find_terms(m),
      list(response = "density", conditional = "conc")
    )
    expect_identical(clean_names(m), c("density", "conc"))
  }
})

test_that("find_terms keeps the transformed response, nls", {
  # log-transformed response, the model is fitted on the log scale
  nls_dnase_log <- stats::nls(
    log(density) ~ log(Asym / (1 + exp((xmid - log(conc)) / scal))),
    data = dnase_run1,
    start = list(Asym = 2.3, xmid = 1.5, scal = 1)
  )
  expect_identical(
    find_terms(nls_dnase_log),
    list(response = "log(density)", conditional = "conc")
  )
  expect_identical(find_transformation(nls_dnase_log), "log")
  # log-likelihood on the scale of the response: subtract the log-Jacobian
  expect_equal(
    as.numeric(get_loglikelihood(nls_dnase_log, check_response = TRUE)),
    as.numeric(stats::logLik(nls_dnase_log)) - sum(log(dnase_run1$density)),
    tolerance = 1e-8
  )
})
