skip_on_cran()
skip_if_not_installed("lavaan")
skip_if_not_installed("blavaan")
skip_if_not_installed("Rcpp")

suppressPackageStartupMessages(require(
  "blavaan",
  quietly = TRUE,
  warn.conflicts = FALSE
)) # nolint

test_that("find_parameters and get_parameters, multiple groups with equality constraints", {
  suppressWarnings(capture.output({
    bfit <- blavaan::bcfa(
      "visual =~ x1 + x2 + x3",
      data = lavaan::HolzingerSwineford1939,
      group = "school",
      group.equal = "loadings",
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
  }))

  params <- find_parameters(bfit, flatten = TRUE)
  expected <- c(
    "visual=~x2 (group 1)",
    "visual=~x3 (group 1)",
    "visual=~x2 (group 2)",
    "visual=~x3 (group 2)"
  )
  expect_identical(grep("=~", params, fixed = TRUE, value = TRUE), expected)
  expect_length(params, 18)

  out <- get_parameters(bfit)
  expect_shape(out, ncol = 18L)
  expect_identical(
    grep("=~", colnames(out), fixed = TRUE, value = TRUE),
    expected
  )
  expect_true(all(colnames(out) %in% params))

  out <- clean_parameters(bfit)
  expect_true(all(params %in% out$Parameter))
  expect_identical(
    out$Component[match(params, out$Parameter)],
    rep(c("latent", "residual", "intercept"), c(4, 8, 6))
  )

  # standardized draws also include the fixed loadings
  out <- get_parameters(bfit, standardize = TRUE)
  expect_shape(out, ncol = 22L)
  expect_identical(
    grep("=~", colnames(out), fixed = TRUE, value = TRUE),
    c(
      "visual=~x1 (group 1)",
      "visual=~x2 (group 1)",
      "visual=~x3 (group 1)",
      "visual=~x1 (group 2)",
      "visual=~x2 (group 2)",
      "visual=~x3 (group 2)"
    )
  )

  # other types of standardization keep the names of the std.all draws. lavaan
  # warns for std.nox in models without exogenous covariates, where std.nox
  # equals std.all, so for std.nox this only checks names and shape
  for (type in c("std.lv", "std.nox")) {
    out_type <- suppressWarnings(get_parameters(bfit, standardize = TRUE, type = type))
    expect_identical(colnames(out_type), colnames(out))
    expect_equal(
      unname(as.matrix(out_type)),
      unname(suppressWarnings(blavaan::standardizedPosterior(bfit, type = type))),
      tolerance = 1e-8
    )
  }
})

test_that("get_parameters, standardized draws with type", {
  suppressWarnings(capture.output({
    bfit <- blavaan::bcfa(
      "visual =~ x1 + x2 + x3",
      data = lavaan::HolzingerSwineford1939,
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
    bfit_x <- blavaan::bsem(
      "visual =~ x1 + x2 + x3
       visual ~ ageyr",
      data = lavaan::HolzingerSwineford1939,
      fixed.x = TRUE,
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
  }))

  for (fit in list(bfit, bfit_x)) {
    out_all <- get_parameters(fit, standardize = TRUE)
    expect_equal(
      unname(as.matrix(out_all)),
      unname(blavaan::standardizedPosterior(fit)),
      tolerance = 1e-8
    )
    # lavaan warns for std.nox in models without exogenous covariates
    for (type in c("std.lv", "std.nox")) {
      out <- suppressWarnings(get_parameters(fit, standardize = TRUE, type = type))
      expect_identical(colnames(out), colnames(out_all))
      expect_equal(
        unname(as.matrix(out)),
        unname(suppressWarnings(blavaan::standardizedPosterior(fit, type = type))),
        tolerance = 1e-8
      )
    }
  }

  # std.lv differs from std.all for the loadings
  out_all <- get_parameters(bfit, standardize = TRUE)
  out_lv <- get_parameters(bfit, standardize = TRUE, type = "std.lv")
  expect_gt(max(abs(out_lv[["visual=~x1"]] - out_all[["visual=~x1"]])), 0.01)

  # std.nox differs from std.all for the exogenous covariate
  out_all <- get_parameters(bfit_x, standardize = TRUE)
  out_nox <- get_parameters(bfit_x, standardize = TRUE, type = "std.nox")
  expect_gt(max(abs(out_nox[["visual~ageyr"]] - out_all[["visual~ageyr"]])), 1e-4)
  expect_gt(max(abs(out_nox[["ageyr~~ageyr"]] - out_all[["ageyr~~ageyr"]])), 0.01)

  # summary of standardized parameters
  out <- get_parameters(bfit, summary = TRUE, standardize = TRUE, type = "std.lv")
  expect_equal(
    out$Estimate,
    lavaan::standardizedSolution(bfit, type = "std.lv")$est.std,
    tolerance = 1e-8
  )
  out_all <- get_parameters(bfit, summary = TRUE, standardize = TRUE)
  expect_equal(
    out_all$Estimate,
    lavaan::standardizedSolution(bfit)$est.std,
    tolerance = 1e-8
  )
  expect_gt(max(abs(out$Estimate - out_all$Estimate)), 0.01)

  # type has no effect without standardization
  expect_identical(get_parameters(bfit, type = "std.lv"), get_parameters(bfit))
  expect_identical(
    get_parameters(bfit, summary = TRUE, type = "std.lv"),
    get_parameters(bfit, summary = TRUE)
  )

  # other arguments in ... are not passed to blavaan or lavaan
  expect_identical(
    get_parameters(bfit, standardize = TRUE, type = "std.lv", verbose = FALSE),
    get_parameters(bfit, standardize = TRUE, type = "std.lv")
  )
  expect_identical(
    get_parameters(bfit, summary = TRUE, standardize = TRUE, type = "std.lv", verbose = FALSE),
    get_parameters(bfit, summary = TRUE, standardize = TRUE, type = "std.lv")
  )
})

test_that("find_parameters and get_parameters, labeled parameters", {
  data("PoliticalDemocracy", package = "lavaan")
  model <- "
    dem60 =~ y1 + a*y2
    dem65 =~ y5 + a*y6
    dem65 ~ dem60
  "
  suppressWarnings(capture.output({
    bfit <- blavaan::bsem(
      model,
      data = PoliticalDemocracy,
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
  }))

  expect_identical(
    find_parameters(bfit)$latent,
    c("dem60=~y2", "dem65=~y6")
  )
  out <- get_parameters(bfit)
  expect_identical(colnames(out)[1:2], c("dem60=~y2", "dem65=~y6"))
  expect_false(anyNA(colnames(out)))

  out <- get_parameters(bfit, standardize = TRUE)
  expect_identical(
    colnames(out)[1:5],
    c("dem60=~y1", "dem60=~y2", "dem65=~y5", "dem65=~y6", "dem65~dem60")
  )
  expect_false(anyNA(colnames(out)))
})

test_that("get_parameters and clean_parameters, defined parameters", {
  model <- "
    visual =~ x1 + a*x2 + b*x3
    ab := a*b
  "
  suppressWarnings(capture.output({
    bfit <- blavaan::bcfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
  }))

  out <- get_parameters(bfit)
  expect_shape(out, ncol = 7L)
  expect_identical(colnames(out)[7], "ab:=a*b")
  expect_false(anyNA(colnames(out)))
  expect_identical(
    find_parameters(bfit, flatten = TRUE),
    colnames(out)[1:6]
  )

  out <- get_parameters(bfit, standardize = TRUE)
  expect_identical(colnames(out)[ncol(out)], "ab:=a*b")
  expect_false(anyNA(colnames(out)))

  out_lv <- get_parameters(bfit, standardize = TRUE, type = "std.lv")
  expect_identical(colnames(out_lv), colnames(out))
  expect_equal(
    unname(as.matrix(out_lv)),
    unname(blavaan::standardizedPosterior(bfit, type = "std.lv")),
    tolerance = 1e-8
  )

  out <- clean_parameters(bfit)
  expect_identical(out$Parameter[nrow(out)], "ab:=a*b")
  expect_false(any(grepl("(group", out$Parameter, fixed = TRUE)))
})

test_that("get_parameters and clean_parameters, defined parameters and multiple groups", {
  model <- "
    visual =~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
    ab := a1*b1
  "
  suppressWarnings(capture.output({
    bfit <- blavaan::bcfa(
      model,
      data = lavaan::HolzingerSwineford1939,
      group = "school",
      n.chains = 1,
      burnin = 50,
      sample = 50,
      seed = 123
    )
  }))

  out <- get_parameters(bfit)
  expect_shape(out, ncol = 19L)
  expect_identical(
    colnames(out)[c(1, 10, 19)],
    c("visual=~x2 (group 1)", "visual=~x2 (group 2)", "ab:=a1*b1")
  )

  out <- clean_parameters(bfit)
  expect_identical(out$Parameter[nrow(out)], "ab:=a1*b1")
  expect_false(any(grepl("(group 0)", out$Parameter, fixed = TRUE)))
})
