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
pnd_newdata <- function(d_pnd, character = FALSE) {
  f <- c("a", "b", NA, "d", "c")
  data.frame(
    x = c(0.5, NA, 1, -0.2, 0.8),
    f = if (character) f else factor(f, levels = c("a", "b", "c", "d")),
    grp = factor(c(1, 2, 3, 4, 2), levels = levels(d_pnd$grp)),
    y = 0,
    yo = d_pnd$yo[rep(1, 5)],
    stringsAsFactors = FALSE
  )
}

test_that("get_predicted - lme and gls, new data rows with NA or unknown levels are NA", {
  skip_if_not_installed("nlme")
  d_pnd <- pnd_fixture()
  # the complete rows, with the factor levels of the model data
  nd2_pnd <- pnd_newdata(d_pnd)[c(1, 5), ]
  nd2_pnd$f <- factor(nd2_pnd$f, levels = levels(d_pnd$f))
  X <- stats::model.matrix(~ x + f, nd2_pnd)

  models <- list(
    lme = nlme::lme(y ~ x + f, random = ~ 1 | grp, data = d_pnd),
    gls = nlme::gls(y ~ x + f, data = d_pnd)
  )
  # `f` as factor with the extra level "d", and as character vector
  for (as_character in c(FALSE, TRUE)) for (m in models) {
    nd_pnd <- pnd_newdata(d_pnd, character = as_character)
    warning_messages <- character()
    out <- withCallingHandlers(
      as.data.frame(get_predicted(m, data = nd_pnd, ci = 0.95)),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
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
      as.vector(stats::predict(m, newdata = pnd_newdata(d_pnd)[c(1, 5), ])),
      tolerance = 1e-10
    )
    expect_equal(
      out$SE[c(1, 5)],
      sqrt(diag(X %*% stats::vcov(m) %*% t(X))),
      tolerance = 1e-10,
      ignore_attr = TRUE
    )
    # one warning that names both columns
    expect_length(warning_messages, 1)
    expect_match(warning_messages, "`x`", fixed = TRUE)
    expect_match(warning_messages, "`f`", fixed = TRUE)
  }
})

test_that("get_predicted - lme, new data without the grouping column gives population-level predictions", {
  skip_if_not_installed("nlme")
  d_pnd <- pnd_fixture()
  # `f` keeps the unused extra level "d"
  nd_pnd <- pnd_newdata(d_pnd)[c(1, 5), ]
  nd_pnd$grp <- NULL
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
    get_predicted(m, data = nd_pnd),
    "Could not compute predictions for model of class `clmm`.",
    fixed = TRUE
  )
  expect_null(suppressWarnings(get_predicted(m, data = nd_pnd)))
  expect_warning(
    get_predicted(m, newdata = nd_pnd),
    "Could not compute predictions for model of class `clmm`.",
    fixed = TRUE
  )
  expect_null(suppressWarnings(get_predicted(m, newdata = nd_pnd)))
  # without new data, the fitted values are still returned. As before, the
  # inverse link is applied to them.
  expect_no_warning({
    out <- get_predicted(m)
  })
  expect_length(out, nrow(d_pnd))
  expect_equal(as.vector(out), stats::plogis(as.vector(stats::fitted(m))), tolerance = 1e-10)
})

test_that("get_predicted - gnls, new data gives predictions", {
  skip_if_not_installed("nlme")
  d_pnd_soy <- nlme::Soybean
  m <- nlme::gnls(weight ~ SSlogis(Time, Asym, xmid, scal), data = d_pnd_soy)
  out <- get_predicted(m, data = d_pnd_soy[1:3, ])
  expect_length(out, 3)
  expect_equal(
    as.vector(out),
    as.vector(stats::predict(m, newdata = d_pnd_soy[1:3, ])),
    tolerance = 1e-10
  )
})

test_that("get_predicted - lme, `ci_type` does not drop the intervals", {
  skip_if_not_installed("nlme")
  d_pnd <- pnd_fixture()
  nd_pnd <- pnd_newdata(d_pnd)[c(1, 5), ]
  m <- nlme::lme(y ~ x + f, random = ~ 1 | grp, data = d_pnd)
  out <- as.data.frame(get_predicted(m, data = nd_pnd, ci = 0.95, ci_type = "confidence"))
  expect_false(anyNA(out[c("SE", "CI_low", "CI_high")]))
  expect_equal(out, as.data.frame(get_predicted(m, data = nd_pnd, ci = 0.95)), tolerance = 1e-10)
})

test_that("get_predicted - lme, intervals use the smallest df, also without new data", {
  skip_if_not_installed("nlme")
  # `Sex` is constant within `Subject`, so its df differ from those of `age`
  d_pnd_orth <- nlme::Orthodont
  m <- nlme::lme(distance ~ age + Sex, random = ~ 1 | Subject, data = d_pnd_orth)
  dof <- min(get_df(m, type = "wald"))
  expect_gt(max(get_df(m, type = "wald")), dof)
  out <- as.data.frame(get_predicted(m, ci = 0.95))
  expect_identical(nrow(out), nrow(d_pnd_orth))
  expect_equal(
    out$CI_high - out$CI_low,
    2 * stats::qt(0.975, dof) * out$SE,
    tolerance = 1e-10
  )
})
