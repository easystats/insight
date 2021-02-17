osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)


if (!osx && require("testthat") &&
  require("insight") &&
  require("MCMCglmm")) {
  data(PlodiaPO)
  m1 <- MCMCglmm(
    PO ~ plate,
    random = ~FSfamily,
    data = PlodiaPO,
    verbose = FALSE,
    nitt = 1300,
    burnin = 300,
    thin = 1
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_mixed)
    expect_true(model_info(m1)$is_linear)
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = "plate"))
    expect_identical(find_predictors(m1, flatten = TRUE), "plate")
    expect_identical(
      find_predictors(m1, effects = "random"),
      list(random = "FSfamily")
    )
  })

  test_that("find_random", {
    expect_equal(find_random(m1), list(random = "FSfamily"))
  })

  test_that("get_random", {
    expect_equal(get_random(m1), data.frame(FSfamily = PlodiaPO$FSfamily))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "PO")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), PlodiaPO$PO)
  })

  test_that("get_predictors", {
    expect_equal(colnames(get_predictors(m1)), "plate")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.5), .5, tolerance = 1e-1)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 511)
    expect_equal(colnames(get_data(m1)), c("FSfamily", "PO", "plate"))
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("PO ~ plate"),
        random = as.formula("~FSfamily")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "PO",
        conditional = "plate",
        random = "FSfamily"
      )
    )
    expect_equal(find_terms(m1, flatten = TRUE), c("PO", "plate", "FSfamily"))
  })

  test_that("n_obs", {
    expect_null(n_obs(m1))
  })

  test_that("linkfun", {
    expect_equal(link_function(m1)(.5), .5, tolerance = 1e-1)
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "plate"),
        random = "FSfamily"
      )
    )
    expect_equal(nrow(get_parameters(m1, summary = TRUE)), 2)
    expect_equal(nrow(get_parameters(m1, summary = FALSE)), 1000)
    expect_equal(get_parameters(m1, summary = TRUE)$Parameter, c("(Intercept)", "plate"))
    expect_equal(colnames(get_parameters(m1, summary = FALSE)), c("(Intercept)", "plate"))
    expect_equal(
      get_parameters(m1, effects = "random", summary = TRUE)$Parameter,
      "FSfamily"
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_null(find_statistic(m1))
  })
}
