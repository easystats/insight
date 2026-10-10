skip_on_cran()

# new data with NA values or unknown factor levels ----

# data names that no other test file uses
pnd_fixture <- function() {
  set.seed(5)
  d_pnd <- data.frame(
    x = rnorm(120),
    f = factor(sample(c("a", "b", "c"), 120, replace = TRUE)),
    grp = factor(rep(1:12, each = 10))
  )
  d_pnd$y <- 1 + d_pnd$x + as.numeric(d_pnd$f) + rnorm(120)
  d_pnd$yo <- cut(d_pnd$y, 4, labels = c("q1", "q2", "q3", "q4"), ordered_result = TRUE)
  d_pnd
}

# 5 rows: rows 1 and 5 complete, row 2 NA in `x`, row 3 NA in `f`,
# row 4 a level of `f` ("d") that the model data does not have
pnd_newdata <- function(d_pnd) {
  data.frame(
    x = c(0.5, NA, 1, -0.2, 0.8),
    f = factor(c("a", "b", NA, "d", "c"), levels = c("a", "b", "c", "d")),
    grp = factor(c(1, 2, 3, 4, 2), levels = levels(d_pnd$grp)),
    y = 0
  )
}

test_that("get_predicted - lme and gls, new data rows with NA or unknown levels are NA", {
  skip_if_not_installed("nlme")
  d_pnd <- pnd_fixture()
  nd_pnd <- pnd_newdata(d_pnd)
  # the complete rows, with the factor levels of the model data
  nd2_pnd <- nd_pnd[c(1, 5), ]
  nd2_pnd$f <- factor(nd2_pnd$f, levels = levels(d_pnd$f))
  X <- stats::model.matrix(~ x + f, nd2_pnd)

  models <- list(
    lme = nlme::lme(y ~ x + f, random = ~ 1 | grp, data = d_pnd),
    gls = nlme::gls(y ~ x + f, data = d_pnd)
  )
  for (m in models) {
    warnings <- character()
    out <- withCallingHandlers(
      as.data.frame(get_predicted(m, data = nd_pnd, ci = 0.95)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(nrow(out), 5L)
    for (column in c("Predicted", "SE", "CI_low", "CI_high")) {
      expect_true(all(is.na(out[[column]][2:4])))
      expect_false(anyNA(out[[column]][c(1, 5)]))
    }
    # nlme predictions at the default level, which includes the random
    # effects for lme models
    expect_equal(
      out$Predicted[c(1, 5)],
      as.vector(stats::predict(m, newdata = nd2_pnd)),
      tolerance = 1e-10
    )
    expect_equal(
      out$SE[c(1, 5)],
      sqrt(diag(X %*% stats::vcov(m) %*% t(X))),
      tolerance = 1e-10,
      ignore_attr = TRUE
    )
    # one warning that names both columns
    expect_length(warnings, 1)
    expect_match(warnings, "`x`", fixed = TRUE)
    expect_match(warnings, "`f`", fixed = TRUE)
  }
})

test_that("get_predicted - lme, new data without the grouping column gives population-level predictions", {
  skip_if_not_installed("nlme")
  d_pnd <- pnd_fixture()
  nd_pnd <- pnd_newdata(d_pnd)[c(1, 5), ]
  nd_pnd$grp <- NULL
  nd_pnd$f <- factor(nd_pnd$f, levels = levels(d_pnd$f))
  m <- nlme::lme(y ~ x + f, random = ~ 1 | grp, data = d_pnd)
  out <- as.data.frame(get_predicted(m, data = nd_pnd, ci = 0.95))
  expect_identical(nrow(out), 2L)
  expect_equal(
    out$Predicted,
    as.vector(stats::predict(m, newdata = nd_pnd, level = 0)),
    tolerance = 1e-10
  )
})

test_that("get_predicted - clmm, no fitted values of the model data for new data", {
  skip_if_not_installed("ordinal")
  d_pnd <- pnd_fixture()
  nd_pnd <- pnd_newdata(d_pnd)[c(1, 5), ]
  m <- ordinal::clmm(yo ~ x + f + (1 | grp), data = d_pnd)
  expect_warning(
    out <- get_predicted(m, data = nd_pnd),
    "Could not compute predictions for model of class `clmm`.",
    fixed = TRUE
  )
  expect_null(out)
  expect_warning(
    out <- get_predicted(m, newdata = nd_pnd),
    "Could not compute predictions for model of class `clmm`.",
    fixed = TRUE
  )
  expect_null(out)
  # without new data, the fitted values are still returned
  out <- get_predicted(m)
  expect_length(out, nrow(d_pnd))
  expect_equal(as.vector(out), as.vector(stats::fitted(m)), tolerance = 1e-10)
})
