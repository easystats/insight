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
