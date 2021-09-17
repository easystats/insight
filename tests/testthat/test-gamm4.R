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

unloadNamespace("gam")

.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && !osx && requiet("testthat") && requiet("insight") && requiet("gamm4")) {
  set.seed(0)
  dat <- gamSim(1, n = 400, scale = 2) ## simulate 4 term additive truth
  dat$fac <- fac <- as.factor(sample(1:20, 400, replace = TRUE))
  dat$y <- dat$y + model.matrix(~ fac - 1) %*% rnorm(20) * .5

  m1 <- gamm4(y ~ s(x0) + x1 + s(x2),
    data = dat,
    random = ~ (1 | fac)
  )

  test_that("model_info", {
    expect_true(model_info(m1)$is_linear)
  })

  test_that("clean_names", {
    expect_equal(clean_names(m1), c("y", "x0", "x1", "x2", "fac"))
  })

  test_that("find_predictors", {
    expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2")))
    expect_identical(find_predictors(m1, flatten = TRUE), c("x0", "x1", "x2"))
    expect_identical(find_predictors(m1, effects = "random"), list(random = "fac"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "y")
  })

  test_that("get_response", {
    expect_equal(get_response(m1), unname(dat$y[, 1]))
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 400)
    expect_equal(
      colnames(get_data(m1)),
      c(
        "y",
        "x1",
        "x0",
        "x2",
        "fac",
        "y.0",
        "Xr",
        "Xr.0",
        "X.(Intercept)",
        "X.x1",
        "X.s(x0)Fx1",
        "X.s(x2)Fx1"
      )
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("y ~ s(x0) + x1 + s(x2)"),
        random = as.formula("~1 | fac")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(find_terms(m1), list(
      response = "y",
      conditional = c("s(x0)", "x1", "s(x2)"),
      random = "fac"
    ))
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("y", "s(x0)", "x1", "s(x2)", "fac")
    )
  })

  test_that("find_variables", {
    expect_equal(find_variables(m1), list(
      response = "y",
      conditional = c("x0", "x1", "x2"),
      random = "fac"
    ))
    expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2", "fac"))
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 400)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c("(Intercept)", "x1"),
        smooth_terms = c("s(x0)", "s(x2)")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 4)
    expect_equal(
      get_parameters(m1)$Parameter,
      c("(Intercept)", "x1", "s(x0)", "s(x2)")
    )
  })

  test_that("is_multivariate", {
    expect_false(is_multivariate(m1))
  })

  test_that("find_statistic", {
    expect_null(find_statistic(m1))
  })
}
