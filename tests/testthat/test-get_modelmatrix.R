test_that("Issue #612: factor padding", {
  # stats::model.matrix() breaks on contrasts when a column of `data` has
  # only 1 factor level

  # no factor
  mod <- glm(vs ~ cyl, data = mtcars, family = binomial)
  mm <- get_modelmatrix(mod)
  expect_identical(nrow(mm), 32L)
  mm <- get_modelmatrix(mod, data = mtcars)
  expect_identical(nrow(mm), 32L)
  mm <- get_modelmatrix(mod, data = head(mtcars))
  expect_identical(nrow(mm), 6L)

  # one factor
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  mod <- glm(vs ~ cyl, data = dat, family = binomial)

  # no data argument
  mm <- get_modelmatrix(mod)
  expect_identical(nrow(mm), 32L)

  # enough factor levels
  mm <- get_modelmatrix(mod, data = head(dat))
  expect_identical(nrow(mm), 6L)

  # not enough factor levels
  mm <- get_modelmatrix(mod, data = dat[3, ])
  expect_identical(nrow(mm), 1L)
})


# iv_robust --------------------------------------------------------------
# =========================================================================

test_that("get_modelmatrix - iv_robust", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("estimatr")
  data(Kmenta, package = "ivreg")

  x <- estimatr::iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(terms(x), data = Kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "P"))
  out2 <- model.matrix(
    terms(x),
    data = get_datagrid(x, by = "P", include_response = TRUE)
  )
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "P")), nrow(out2))
})


# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - ivreg", {
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("ivreg")
  data(Kmenta, package = "ivreg")
  d_kmenta <<- Kmenta

  set.seed(15)
  x <- ivreg::ivreg(Q ~ P + D | D + F + A, data = d_kmenta)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = d_kmenta)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "P"))
  out2 <- model.matrix(
    terms(x),
    data = get_datagrid(x, by = "P", include_response = TRUE)
  )
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "P")), nrow(out2))
})


# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_modelmatrix - lm_robust", {
  skip_if_not_installed("estimatr")

  set.seed(15)
  N <- 1:40
  dat <<- data.frame(
    N = N,
    y = rpois(N, lambda = 4),
    x = rnorm(N),
    z = rbinom(N, 1, prob = 0.4)
  )

  x <- estimatr::lm_robust(y ~ x + z, data = dat)

  out1 <- get_modelmatrix(x)
  out2 <- model.matrix(x, data = dat)
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- get_modelmatrix(x, data = get_datagrid(x, by = "x"))
  out2 <- model.matrix(x, data = get_datagrid(x, by = "x", include_response = TRUE))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(nrow(get_datagrid(x, by = "x")), nrow(out2))
})


test_that("Issue #693", {
  set.seed(12345)
  n <- 500
  x <- sample.int(3, n, replace = TRUE)
  w <- sample.int(4, n, replace = TRUE)
  y <- rnorm(n)
  z <- as.numeric(x + y + rlogis(n) > 1.5)
  dat <<- data.frame(x = factor(x), w = factor(w), y = y, z = z)
  m <- glm(z ~ x + w + y, family = binomial, data = dat)
  nd <- head(dat, 2)
  mm <- get_modelmatrix(m, data = head(dat, 1))
  expect_true(all(c("x2", "x3", "w2", "w3", "w4") %in% colnames(mm)))
})


test_that("get_modelmatrix - lme uses model contrasts, Issue #483", {
  skip_if_not_installed("nlme")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  m <- nlme::lme(
    mpg ~ cyl,
    data = d,
    random = ~ 1 | gear,
    contrasts = list(cyl = contr.sum)
  )
  expected <- stats::model.matrix(
    ~cyl,
    data = d,
    contrasts.arg = list(cyl = contr.sum)
  )
  out <- get_modelmatrix(m)
  expect_identical(colnames(out), c("(Intercept)", "cyl1", "cyl2"))
  expect_equal(out, expected, ignore_attr = TRUE)
  expect_identical(colnames(out), names(nlme::fixef(m)))

  # new data keeps the model contrasts
  out <- get_modelmatrix(m, data = head(d, 5))
  expect_equal(out, head(expected, 5), ignore_attr = TRUE)

  # new data with only some of the factor levels
  nd <- data.frame(mpg = 0, cyl = c("6", "8"), gear = 4)
  out <- get_modelmatrix(m, data = nd)
  expect_equal(out, expected[c(1, 5), ], ignore_attr = TRUE)

  # user-provided contrasts, also NULL, replace the model contrasts
  out <- get_modelmatrix(m, contrasts.arg = NULL)
  expect_identical(colnames(out), c("(Intercept)", "cyl6", "cyl8"))

  # default contrasts are unchanged
  m <- nlme::lme(mpg ~ cyl, data = d, random = ~ 1 | gear)
  out <- get_modelmatrix(m)
  expect_identical(colnames(out), c("(Intercept)", "cyl6", "cyl8"))
})


test_that("get_modelmatrix - lme, new data with character predictor", {
  skip_if_not_installed("nlme")
  d <- mtcars
  d$cyl <- as.character(d$cyl)
  m <- nlme::lme(mpg ~ cyl, data = d, random = ~ 1 | gear)
  expected <- stats::model.matrix(~cyl, data = d)
  # new data with only some of the levels
  nd <- data.frame(mpg = 0, cyl = c("6", "8"), gear = 4)
  out <- get_modelmatrix(m, data = nd)
  expect_identical(colnames(out), c("(Intercept)", "cyl6", "cyl8"))
  expect_equal(out, expected[c(1, 5), ], ignore_attr = TRUE)
})


test_that("get_modelmatrix - gls uses model contrasts", {
  skip_if_not_installed("nlme")
  # not `d`: with the global `d` that test-coxme.R leaves, get_data() returns
  # the wrong data for this model
  d_gls <- mtcars
  d_gls$cyl <- factor(d_gls$cyl)
  m <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    nlme::gls(mpg ~ cyl, data = d_gls)
  )
  expected <- stats::model.matrix(
    ~cyl,
    data = d_gls,
    contrasts.arg = list(cyl = contr.sum)
  )
  out <- get_modelmatrix(m)
  expect_identical(colnames(out), names(stats::coef(m)))
  expect_equal(out, expected, ignore_attr = TRUE)

  # new data with only some of the factor levels
  nd <- data.frame(mpg = 0, cyl = c("6", "8"))
  out <- get_modelmatrix(m, data = nd)
  expect_equal(out, expected[c(1, 5), ], ignore_attr = TRUE)
})


test_that("get_modelmatrix - lme ignores model.matrix() methods for lme objects", {
  skip_if_not_installed("nlme")
  # MuMIn registers a model.matrix() method for lme objects that ignores the
  # `data` and `contrasts.arg` arguments
  local_mocked_s3_method(
    "model.matrix",
    "lme",
    function(object, ...) stop("model.matrix.lme() was called", call. = FALSE)
  )
  d_lme <- mtcars
  d_lme$cyl <- factor(d_lme$cyl)
  m <- nlme::lme(
    mpg ~ cyl,
    data = d_lme,
    random = ~ 1 | gear,
    contrasts = list(cyl = contr.sum)
  )
  expected <- stats::model.matrix(
    ~cyl,
    data = d_lme,
    contrasts.arg = list(cyl = contr.sum)
  )
  out <- get_modelmatrix(m, data = head(d_lme, 5))
  expect_equal(out, head(expected, 5), ignore_attr = TRUE)
})


test_that("get_modelmatrix - lme and gls, new data keeps the basis of poly()", {
  skip_if_not_installed("nlme")
  d_lme <- mtcars
  d_lme$cyl <- factor(d_lme$cyl)
  expected <- stats::model.matrix(stats::lm(mpg ~ poly(wt, 2) + cyl, data = d_lme))
  m <- nlme::lme(mpg ~ poly(wt, 2) + cyl, data = d_lme, random = ~ 1 | gear)
  out <- get_modelmatrix(m, data = head(d_lme, 5))
  expect_identical(colnames(out), colnames(expected))
  expect_equal(out, head(expected, 5), ignore_attr = TRUE)
  m <- nlme::gls(mpg ~ poly(wt, 2) + cyl, data = d_lme)
  out <- get_modelmatrix(m, data = head(d_lme, 5))
  expect_equal(out, head(expected, 5), ignore_attr = TRUE)
})


test_that("get_modelmatrix works with NA columns, Issue #1147", {
  skip_if(getRversion() < "4.5.0")
  data(penguins)
  mod <- lm(flipper_len ~ bill_dep * sex, data = penguins, weights = body_mass)
  nd <- get_datagrid(mod)
  nd$flipper_len <- 0
  out <- get_modelmatrix(mod, data = nd)

  expect_identical(
    colnames(out),
    c("(Intercept)", "bill_dep", "sexmale", "bill_dep:sexmale")
  )
  expect_identical(dim(out), c(18L, 4L))

  # these examples should work without error
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("modelbased")

  mod_tmb <- glmmTMB::glmmTMB(
    flipper_len ~ bill_dep * sex,
    data = penguins
  )
  expect_silent(modelbased::estimate_relation(mod_tmb, by = c("bill_dep", "sex")))

  mod_tmb <- glmmTMB::glmmTMB(
    flipper_len ~ bill_dep * sex,
    data = penguins,
    weights = body_mass
  )
  expect_silent(modelbased::estimate_relation(mod_tmb, by = c("bill_dep", "sex")))
})


# clmm and brmsfit: model contrasts, new data, user contrasts, Issue #1237 ----

# compare column names, then values of the plain matrices
expect_modelmatrix <- function(out, expected) {
  expect_identical(colnames(out), colnames(expected))
  plain <- function(x) matrix(as.vector(x), nrow = nrow(x), ncol = ncol(x))
  expect_equal(plain(out), plain(expected))
}

clmm_fixture <- function() {
  w2 <- ordinal::wine
  w2$ch <- as.character(w2$contact)
  m_c <- ordinal::clmm(
    rating ~ temp + ch + (1 | judge),
    data = w2,
    contrasts = list(temp = "contr.sum")
  )
  list(data = w2, model = m_c)
}

# not `d`: with the global `d` that test-coxme.R leaves, get_data() returns
# the wrong data for brmsfit models
brms_fixture <- function() {
  d_brms <- mtcars
  d_brms$cyl <- factor(d_brms$cyl)
  contrasts(d_brms$cyl) <- contr.sum(3)
  m_b <- suppressMessages(suppressWarnings(
    brms::brm(mpg ~ cyl + wt, data = d_brms, empty = TRUE)
  ))
  list(data = d_brms, model = m_b)
}

test_that("get_modelmatrix - clmm with sum contrasts, no data", {
  skip_if_not_installed("ordinal")
  fx <- clmm_fixture()
  expected <- stats::model.matrix(
    ~ temp + ch,
    fx$data,
    contrasts.arg = list(temp = "contr.sum")
  )
  expect_modelmatrix(get_modelmatrix(fx$model), expected)
})

test_that("get_variance - clmm var.fixed with sum contrasts equals default contrasts", {
  skip_if_not_installed("ordinal")
  fx <- clmm_fixture()
  m_default <- ordinal::clmm(rating ~ temp + ch + (1 | judge), data = fx$data)
  out <- get_variance(fx$model)
  ref <- get_variance(m_default)
  expect_type(out, "list")
  expect_equal(out$var.fixed, ref$var.fixed, tolerance = 1e-4)
})

test_that("get_modelmatrix - clmm, new data with a subset of levels", {
  skip_if_not_installed("ordinal")
  fx <- clmm_fixture()
  s <- fx$data$temp == "cold" & fx$data$ch == "no"
  expected <- get_modelmatrix(fx$model)[which(s), , drop = FALSE]
  # (a) rows of the model data, all factor levels kept
  nd <- fx$data[s, ]
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
  # (b) unused factor levels dropped
  nd <- droplevels(fx$data[s, ])
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
  # (c) as (b), without the response column
  nd$rating <- NULL
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
})

test_that("get_modelmatrix - clmm, user contrasts replace model contrasts", {
  skip_if_not_installed("ordinal")
  fx <- clmm_fixture()
  expected <- stats::model.matrix(
    ~ temp + ch,
    fx$data,
    contrasts.arg = list(temp = "contr.helmert")
  )
  expect_no_warning({
    out <- get_modelmatrix(fx$model, contrasts.arg = list(temp = "contr.helmert"))
  })
  expect_modelmatrix(out, expected)
})

test_that("get_modelmatrix - brmsfit with sum contrasts, new data with a subset of levels", {
  skip_if_not_installed("brms")
  fx <- brms_fixture()
  # the full model matrix uses the sum contrasts of the factor
  expect_modelmatrix(
    get_modelmatrix(fx$model),
    stats::model.matrix(~ cyl + wt, fx$data, contrasts.arg = list(cyl = "contr.sum"))
  )
  s <- fx$data$cyl %in% c("4", "6")
  expected <- get_modelmatrix(fx$model)[which(s), , drop = FALSE]
  # (a) rows of the model data, all factor levels and the contrasts kept
  nd <- fx$data[s, ]
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
  # (b) unused factor levels dropped
  nd <- droplevels(fx$data[s, ])
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
  # (c) as (b), factor re-made with all levels and no contrasts attribute
  nd$cyl <- factor(as.character(nd$cyl), levels = levels(fx$data$cyl))
  expect_modelmatrix(get_modelmatrix(fx$model, data = nd), expected)
})

test_that("get_modelmatrix - brmsfit, user contrasts replace model contrasts", {
  skip_if_not_installed("brms")
  fx <- brms_fixture()
  expected <- stats::model.matrix(
    ~ cyl + wt,
    fx$data,
    contrasts.arg = list(cyl = "contr.helmert")
  )
  expect_no_warning({
    out <- get_modelmatrix(fx$model, contrasts.arg = list(cyl = "contr.helmert"))
  })
  expect_modelmatrix(out, expected)
})

test_that("get_modelmatrix - brmsfit, no warning for contrasts of a grouping factor", {
  skip_if_not_installed("brms")
  d_brms <- mtcars
  d_brms$cyl <- factor(d_brms$cyl)
  contrasts(d_brms$cyl) <- contr.sum(3)
  m <- suppressMessages(suppressWarnings(
    brms::brm(mpg ~ wt + (1 | cyl), data = d_brms, empty = TRUE)
  ))
  expect_no_warning({
    out <- get_modelmatrix(m)
  })
  expect_identical(colnames(out), c("(Intercept)", "wt"))
})

test_that("get_modelmatrix - clmm fitted with model = FALSE", {
  skip_if_not_installed("ordinal")
  fx <- clmm_fixture()
  w3 <- fx$data
  m <- ordinal::clmm(
    rating ~ temp + ch + (1 | judge),
    data = w3,
    contrasts = list(temp = "contr.sum"),
    model = FALSE
  )
  expect_modelmatrix(get_modelmatrix(m), get_modelmatrix(fx$model))
})
