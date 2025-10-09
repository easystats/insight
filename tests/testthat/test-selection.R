skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("sampleSelection")
skip_if_not_installed("mvtnorm")

set.seed(0)
vc <- diag(3)
vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)
vc[upper.tri(vc)] <- vc[lower.tri(vc)]
eps <- mvtnorm::rmvnorm(500, c(0, 0, 0), vc)
xs <- runif(500)
ys <- xs + eps[, 1] > 0
xo1 <- runif(500)
yo1 <- xo1 + eps[, 2]
xo2 <- runif(500)
yo2 <- xo2 + eps[, 3]
yo <- ifelse(ys, yo2, yo1)
ys <- as.numeric(ys) + 1
dat_sel <<- data.frame(ys, yo, yo1, yo2, xs, xo1, xo2)
m1 <- sampleSelection::selection(ys ~ xs, list(yo1 ~ xo1, yo2 ~ xo2), data = dat_sel)

set.seed(0)
xs1 <- runif(500)
xs2 <- runif(500)
ys <- xs1 + xs2 + eps[, 1] > 0
xo11 <- runif(500)
xo12 <- runif(500)
yo1 <- xo11 + xo12 + eps[, 2]
xo2 <- runif(500)
yo2 <- xo2 + eps[, 3]
yo <- ifelse(ys, yo2, yo1)
ys <- as.numeric(ys) + 1
dat_sel2 <<- data.frame(ys, yo, yo1, yo2, xs1, xs2, xo11, xo12, xo2)
m3 <- sampleSelection::selection(
  ys ~ xs1 + xs2,
  list(yo1 ~ xo11 + xo12, yo2 ~ xo2),
  data = dat_sel2
)

data(Mroz87, package = "sampleSelection")
Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0)
m2 <- sampleSelection::selection(
  lfp ~ age + I(age^2) + faminc + kids + educ,
  wage ~ exper + I(exper^2) + educ + city,
  data = Mroz87
)


test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
  expect_true(model_info(m2)$is_linear)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1),
    list(conditional = list(selection = "xs", outcome = list(yo1 = "xo1", yo2 = "xo2")))
  )
  expect_identical(
    find_predictors(m3),
    list(
      conditional = list(
        selection = c("xs1", "xs2"),
        outcome = list(yo1 = c("xo11", "xo12"), yo2 = "xo2")
      )
    )
  )
  expect_identical(
    find_predictors(m2),
    list(
      conditional = list(
        selection = c("age", "faminc", "kids", "educ"),
        outcome = c("exper", "educ", "city")
      )
    )
  )
})

test_that("find_response", {
  expect_identical(find_response(m1), c("ys", "yo1", "yo2"))
  expect_identical(find_response(m3), c("ys", "yo1", "yo2"))
  expect_identical(find_response(m2), c("lfp", "wage"))
})

test_that("get_response", {
  expect_equal(get_response(m1), dat_sel[c("ys", "yo1", "yo2")], ignore_attr = TRUE)
  expect_equal(get_response(m3), dat_sel2[c("ys", "yo1", "yo2")], ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_named(get_predictors(m1), c("xs", "xo1", "xo2"))
  expect_named(get_predictors(m3), c("xs1", "xs2", "xo11", "xo12", "xo2"))
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 500)
  expect_named(get_data(m1, verbose = FALSE), c("ys", "yo1", "yo2", "xs", "xo1", "xo2"))
  expect_named(
    get_data(m3, verbose = FALSE),
    c("ys", "yo1", "yo2", "xs1", "xs2", "xo11", "xo12", "xo2")
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(
      conditional = list(
        selection = ys ~ xs,
        outcome = list(yo1 ~ xo1, yo2 ~ xo2)
      )
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m3),
    list(
      conditional = list(
        selection = ys ~ xs1 + xs2,
        outcome = list(yo1 ~ xo11 + xo12, yo2 ~ xo2)
      )
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_formula(m2),
    list(
      conditional = list(
        selection = lfp ~ age + I(age^2) + faminc + kids + educ,
        outcome = wage ~ exper + I(exper^2) + educ + city
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(conditional = list(selection = "ys  xs", outcome = c("yo1  xo1", "yo2  xo2")))
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("ys  xs", "yo1  xo1", "yo2  xo2")
  )
  expect_equal(
    find_terms(m2),
    list(
      conditional = list(
        selection = c("lfp  age", "I(age^2)", "faminc", "kids", "educ"),
        outcome = c("wage  exper", "I(exper^2)", "educ", "city")
      )
    )
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 500)
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      selection = c("(Intercept)", "xs"),
      outcome = c("(Intercept)", "xo1", "(Intercept)", "xo2"),
      auxiliary = c("sigma1", "rho1", "sigma2", "rho2")
    )
  )
  expect_equal(
    find_parameters(m2),
    list(
      selection = c("(Intercept)", "age", "I(age^2)", "faminc", "kidsTRUE", "educ"),
      outcome = c("(Intercept)", "exper", "I(exper^2)", "educ", "city"),
      auxiliary = c("sigma", "rho")
    )
  )
})

test_that("get_parameters", {
  out <- get_parameters(m1)
  expect_identical(
    out$Component,
    c(
      "selection",
      "selection",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary"
    )
  )
  out <- get_parameters(m2)
  expect_identical(
    out$Component,
    c(
      "selection",
      "selection",
      "selection",
      "selection",
      "selection",
      "selection",
      "outcome",
      "outcome",
      "outcome",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary"
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})

test_that("get_statistic", {
  out <- get_statistic(m1)
  expect_identical(
    out$Component,
    c(
      "selection",
      "selection",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary"
    )
  )
  expect_equal(
    out$Statistic,
    as.vector(summary(m1)$estimate[, 3]),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  out <- get_statistic(m2)
  expect_identical(
    out$Component,
    c(
      "selection",
      "selection",
      "selection",
      "selection",
      "selection",
      "selection",
      "outcome",
      "outcome",
      "outcome",
      "outcome",
      "outcome",
      "auxiliary",
      "auxiliary"
    )
  )
  expect_equal(
    out$Statistic,
    as.vector(summary(m2)$estimate[, 3]),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})
