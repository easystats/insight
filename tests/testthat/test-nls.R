nls_dnase <- stats::nls(
  log(density) ~ log(Asym / (1 + exp((xmid - log(conc)) / scal))),
  data = subset(datasets::DNase, Run == 1),
  start = list(Asym = 2.3, xmid = 1.5, scal = 1)
)

test_that("find_parameters, nls", {
  expect_identical(
    find_parameters(nls_dnase),
    list(conditional = c("Asym", "xmid", "scal"))
  )
})

test_that("find_variables, find_terms, nls", {
  expect_identical(
    find_variables(nls_dnase),
    list(response = "density", conditional = "conc")
  )
  expect_identical(
    find_terms(nls_dnase),
    list(response = "log(density)", conditional = "conc")
  )
})
