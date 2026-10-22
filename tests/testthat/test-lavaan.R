skip_on_cran()
skip_if_not_installed("lavaan")

test_that("is_converged", {
  model <- lavaan::cfa(" visual  =~ x1 ", data = lavaan::HolzingerSwineford1939[1:100, ])
  expect_true(is_converged(model))
})

test_that("get_varcov", {
  model <- lavaan::cfa(" visual  =~ x1 ", data = lavaan::HolzingerSwineford1939[1:100, ])
  expect_equal(
    get_varcov(model),
    lavaan::vcov(model),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  # Create simple example data
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- 0.3 * x + rnorm(n, 0, 0.8)
  data <- data.frame(x = x, y = y)

  # Fit simple lavaan model
  model <- "y ~ x"
  fit <- lavaan::sem(model, data = data)
  expect_equal(
    get_varcov(fit),
    lavaan::vcov(fit),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("get_parameters, standardized parameters with type", {
  model <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939
  )
  out <- get_parameters(model, standardize = TRUE, type = "std.lv")
  expect_equal(
    out$Estimate,
    lavaan::standardizedSolution(model, type = "std.lv")$est.std,
    tolerance = 1e-8
  )
  # other arguments in ... are not passed to lavaan, and not evaluated
  expect_identical(
    get_parameters(model, standardize = TRUE, type = "std.lv", foo = stop("not evaluated", call. = FALSE)),
    out
  )
})

test_that("get_parameters, only standardize = TRUE standardizes", {
  model <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939
  )
  expect_identical(get_parameters(model, standardize = 1), get_parameters(model))
})

test_that("get_parameters, standardized estimates without partial matching", {
  model <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939
  )
  withr::local_options(warnPartialMatchDollar = TRUE)
  # the warning text is translated, and the pattern below is English
  withr::local_language("en")
  # the column of standardized estimates is "est.std", not "est"
  expect_no_warning(
    get_parameters(model, standardize = TRUE),
    message = "[Pp]artial match of 'est'"
  )
  out <- get_parameters(model, standardize = TRUE)
  expect_identical(
    out$Estimate,
    lavaan::standardizedSolution(model)[["est.std"]]
  )
})

test_that("get_data, model frame of models with multiple groups", {
  model <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    group = "school"
  )
  out <- get_data(model, source = "mf", verbose = FALSE)
  expect_named(out, c("x1", "x2", "x3", "school"))
  expect_identical(
    as.vector(table(out$school)[c("Pasteur", "Grant-White")]),
    c(156L, 145L)
  )
})
