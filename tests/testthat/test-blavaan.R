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
  out <- get_parameters(bfit)
  expect_identical(grep("=~", colnames(out), fixed = TRUE, value = TRUE), expected)
  expect_setequal(colnames(out), params)

  out <- clean_parameters(bfit)
  expect_identical(
    out$Component[match(params, out$Parameter)],
    rep(c("latent", "residual", "intercept"), c(4, 8, 6))
  )

  # standardized draws also include the fixed loadings
  out <- get_parameters(bfit, standardize = TRUE)
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
  }))

  out <- get_parameters(bfit, standardize = TRUE, type = "std.lv")
  expect_identical(colnames(out), colnames(get_parameters(bfit, standardize = TRUE)))
  expect_equal(
    unname(as.matrix(out)),
    unname(blavaan::standardizedPosterior(bfit, type = "std.lv")),
    tolerance = 1e-8
  )

  # summary of standardized parameters
  out <- get_parameters(bfit, summary = TRUE, standardize = TRUE, type = "std.lv")
  expect_equal(
    out$Estimate,
    lavaan::standardizedSolution(bfit, type = "std.lv")$est.std,
    tolerance = 1e-8
  )

  # values other than TRUE give the unstandardized parameters
  expect_identical(
    get_parameters(bfit, summary = TRUE, standardize = 1),
    get_parameters(bfit, summary = TRUE)
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
  expect_identical(
    find_parameters(bfit, flatten = TRUE),
    colnames(out)[1:6]
  )

  out <- get_parameters(bfit, standardize = TRUE)
  expect_identical(colnames(out)[ncol(out)], "ab:=a*b")
  expect_false(anyNA(colnames(out)))

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
