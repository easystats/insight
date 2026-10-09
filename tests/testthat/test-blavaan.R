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
  expect_identical(params[grepl("=~", params, fixed = TRUE)], expected)
  expect_length(params, 18)

  out <- get_parameters(bfit)
  expect_identical(ncol(out), 18L)
  expect_identical(
    colnames(out)[grepl("=~", colnames(out), fixed = TRUE)],
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
  expect_identical(ncol(out), 22L)
  expect_identical(
    colnames(out)[grepl("=~", colnames(out), fixed = TRUE)],
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
