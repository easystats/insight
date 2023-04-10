skip_on_os(c("mac", "linux"))
skip_if_not_installed("MCMCglmm")

# see https://github.com/georgheinze/logistf/pull/54
skip_if(
  "as.character.formula" %in% methods(as.character),
  "Package `logistf` is loaded and breaks `MCMCglmm::MCMCglmm()`"
)

data(PlodiaPO, package = "MCMCglmm")
mod_mcmcglmm <- MCMCglmm::MCMCglmm(
  PO ~ plate,
  random = ~FSfamily,
  data = PlodiaPO,
  verbose = FALSE,
  nitt = 1300,
  burnin = 300,
  thin = 1
)

test_that("model_info", {
  expect_true(model_info(mod_mcmcglmm)$is_mixed)
  expect_true(model_info(mod_mcmcglmm)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(mod_mcmcglmm), list(conditional = "plate"))
  expect_identical(find_predictors(mod_mcmcglmm, flatten = TRUE), "plate")
  expect_identical(
    find_predictors(mod_mcmcglmm, effects = "random"),
    list(random = "FSfamily")
  )
})

test_that("find_random", {
  expect_equal(find_random(mod_mcmcglmm), list(random = "FSfamily"))
})

test_that("get_random", {
  expect_equal(get_random(mod_mcmcglmm), data.frame(FSfamily = PlodiaPO$FSfamily), ignore_attr = TRUE)
})

test_that("find_response", {
  expect_identical(find_response(mod_mcmcglmm), "PO")
})

test_that("get_response", {
  expect_equal(get_response(mod_mcmcglmm), PlodiaPO$PO)
})

test_that("get_predictors", {
  expect_equal(colnames(get_predictors(mod_mcmcglmm)), "plate")
})

test_that("link_inverse", {
  expect_equal(link_inverse(mod_mcmcglmm)(0.5), 0.5, tolerance = 1e-1)
})

test_that("get_data", {
  expect_equal(nrow(get_data(mod_mcmcglmm, verbose = FALSE)), 511)
  expect_equal(colnames(get_data(mod_mcmcglmm, verbose = FALSE)), c("FSfamily", "PO", "plate"))
})

test_that("find_formula", {
  expect_length(find_formula(mod_mcmcglmm), 2)
  expect_equal(
    find_formula(mod_mcmcglmm),
    list(
      conditional = as.formula("PO ~ plate"),
      random = as.formula("~FSfamily")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(mod_mcmcglmm),
    list(
      response = "PO",
      conditional = "plate",
      random = "FSfamily"
    )
  )
  expect_equal(find_terms(mod_mcmcglmm, flatten = TRUE), c("PO", "plate", "FSfamily"))
})

test_that("n_obs", {
  expect_null(n_obs(mod_mcmcglmm))
})

test_that("linkfun", {
  expect_equal(link_function(mod_mcmcglmm)(0.5), 0.5, tolerance = 1e-1)
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(mod_mcmcglmm),
    list(
      conditional = c("(Intercept)", "plate"),
      random = "FSfamily"
    )
  )
  expect_equal(nrow(get_parameters(mod_mcmcglmm, summary = TRUE)), 2)
  expect_equal(nrow(get_parameters(mod_mcmcglmm, summary = FALSE)), 1000)
  expect_equal(get_parameters(mod_mcmcglmm, summary = TRUE)$Parameter, c("(Intercept)", "plate"))
  expect_equal(colnames(get_parameters(mod_mcmcglmm, summary = FALSE)), c("(Intercept)", "plate"))
  expect_equal(
    get_parameters(mod_mcmcglmm, effects = "random", summary = TRUE)$Parameter,
    "FSfamily"
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(mod_mcmcglmm))
})

test_that("find_statistic", {
  expect_null(find_statistic(mod_mcmcglmm))
})
