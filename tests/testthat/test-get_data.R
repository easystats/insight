skip_on_os("mac")

test_that("retrieve from same environment", {
  foo <- data.frame(x = 1:10, y = 2:11)
  fit <- lm(y ~ x, data = foo)

  expect_no_warning({
    cols <- names(get_data(fit))
  })
  expect_setequal(cols, c("x", "y"))
})

test_that("retrieve from correct environment", {
  foo <- function() {
    foo <- data.frame(x = 1:10, y = 2:11)
    lm(y ~ x, data = foo)
  }

  # There should be no warning about "Could not recover model data from
  # environment"
  expect_no_warning({
    cols <- names(get_data(foo()))
  })
  expect_setequal(cols, c("x", "y"))
})

test_that("fetch from local, not global, environment", {
  # See #760. If the local environment has a modified version of data also in
  # the global environment, we should find the local version first, not the
  # global version.

  foo <- function() {
    mtcars$cylinders <- factor(mtcars$cyl)
    lm(mpg ~ cylinders + disp, data = mtcars)
  }

  expect_setequal(
    names(get_data(foo())),
    c("mpg", "disp", "cylinders")
  )
})

test_that("retrieve from call formula environment", {
  skip_if_not_installed("AER")

  foo <- function() {
    d <- data.frame(
      y = rnorm(100),
      x = rnorm(100)
    )

    # find_formula(fit)$conditional happens to not have an environment for tobit
    # models, so get_data() should check environment(get_call(fit)$formula). See
    # #666
    AER::tobit(y ~ x, data = d, right = 1.5)
  }

  expect_setequal(
    names(get_data(foo())),
    c("x", "y")
  )
})

test_that("lme", {
  skip_if_not_installed("nlme")
  data("Orthodont", package = "nlme")
  m <- nlme::lme( # a model of variance only
    distance ~ 1,
    data = Orthodont, # grand mean
    weights = nlme::varConstPower(form = ~ age | Sex)
  )
  expect_identical(dim(get_data(m, source = "mf")), c(108L, 3L))
  expect_identical(colnames(get_data(m, source = "mf")), c("distance", "age", "Sex"))
})


test_that("lme4", {
  skip_if_not_installed("lme4")
  data("cbpp", package = "lme4")
  set.seed(123)
  cbpp$cont <- rnorm(nrow(cbpp))
  m <- lme4::glmer(cbind(incidence, size - incidence) ~ poly(cont, 2) + (1 | herd),
    data = cbpp, family = binomial
  )
  expect_s3_class(get_data(m), "data.frame")
})


test_that("additional_variables = TRUE", {
  k <- mtcars
  k$qsec[1:10] <- NA
  k <- k
  mod <- lm(mpg ~ hp, k)
  n1 <- nrow(k)
  n2 <- nrow(insight::get_data(mod))
  n3 <- nrow(insight::get_data(mod, additional_variables = TRUE))
  expect_identical(n1, n2)
  expect_identical(n1, n3)
})


test_that("lm", {
  set.seed(1023)
  x <- rnorm(1000, sd = 4)
  y <- cos(x) + rnorm(1000)

  dat <- data.frame(x, y)
  mod1 <- lm(y ~ x, data = dat)
  mod2 <- lm(y ~ cos(x), data = dat)
  expect_equal(get_data(mod1), get_data(mod2), ignore_attr = TRUE)
  expect_equal(get_data(mod1)$x, dat$x, ignore_attr = TRUE)
  expect_equal(get_data(mod2)$x, dat$x, ignore_attr = TRUE)
})


test_that("get_data lavaan", {
  skip_if_not_installed("lavaan")
  data(PoliticalDemocracy, package = "lavaan")
  model <- "
    # latent variable definitions
      ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + a*y2 + b*y3 + c*y4
      dem65 =~ y5 + a*y6 + b*y7 + c*y8

    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60

    # residual correlations
      y1 ~~ y5
      y2 ~~ y4 + y6
      y3 ~~ y7
      y4 ~~ y8
      y6 ~~ y8
  "
  m <- lavaan::sem(model, data = PoliticalDemocracy)
  expect_s3_class(get_data(m, verbose = FALSE), "data.frame")
  expect_equal(head(get_data(m, verbose = FALSE)), head(PoliticalDemocracy), ignore_attr = TRUE, tolerance = 1e-3)

  # works when data not in environment
  holz_data <<- lavaan::HolzingerSwineford1939
  HS.model <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  m_holz <- lavaan::lavaan(HS.model,
    data = holz_data, auto.var = TRUE, auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )

  skip_on_os(c("mac", "linux"))
  out1 <- get_data(m_holz)
  expect_named(
    out1,
    c(
      "id", "sex", "ageyr", "agemo", "school", "grade", "x1", "x2",
      "x3", "x4", "x5", "x6", "x7", "x8", "x9"
    )
  )
  expect_identical(nrow(out1), 301L)

  # rm(holz_data)
  # out2 <- get_data(m_holz)
  # expect_named(
  #   out2,
  #   c("x1", "x2","x3", "x4", "x5", "x6", "x7", "x8", "x9")
  # )
  # expect_identical(nrow(out2), 301L)
})


test_that("get_data include weights, even if ones", {
  set.seed(123)
  y <- rnorm(100)
  x <- rnorm(100)
  wn <- runif(100)
  w1 <- rep(1, 100)

  # Model with nonuniform weights
  fn <- lm(y ~ x, weights = wn)
  expect_identical(colnames(get_data(fn, verbose = FALSE)), c("y", "x", "(weights)", "wn"))

  # Model with weights equal to 1
  f1 <- lm(y ~ x, weights = w1)
  expect_identical(colnames(get_data(f1, verbose = FALSE)), c("y", "x", "(weights)", "w1"))

  # Model with no weights
  f0 <- lm(y ~ x)
  expect_identical(colnames(get_data(f0, verbose = FALSE)), c("y", "x"))

  # check get_weights still works
  expect_null(get_weights(f0))
  expect_identical(get_weights(f0, null_as_ones = TRUE), w1)
})


test_that("lm with transformations", {
  d <- data.frame(
    time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
    group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50)
  )
  m <- lm(log(sum + 1) ~ as.numeric(time) * group, data = d)
  expect_identical(colnames(get_data(m)), c("sum", "time", "group"))
})


test_that("lm with poly and NA in response", {
  d <- iris
  d[1:25, "Sepal.Length"] <- NA
  d2 <- d
  m <- lm(Sepal.Length ~ Species / poly(Petal.Width, 2), data = d2)
  expect_equal(get_data(m), iris[26:150, c("Sepal.Length", "Species", "Petal.Width")], ignore_attr = TRUE)
})


test_that("mgcv", {
  skip_if_not_installed("mgcv")

  # mgcv::gam() deliberately does not keep its environment, so get_data() has to
  # fall back to the model frame. See
  # https://github.com/cran/mgcv/blob/a4e69cf44a49c84a41a42e90c86995a843733968/R/mgcv.r#L2156-L2159
  d <- iris
  d$NewFac <- rep_len(c(1, 2), 150)
  model <- mgcv::gam(Sepal.Length ~ s(Petal.Length, by = interaction(Species, NewFac)), data = d)

  # There should be two warnings: One for failing to get the data from the
  # environment, and one for not recovering interaction() accurately
  expect_warning(expect_warning({
    model_data <- get_data(model)
  }))
  expect_equal(
    head(model_data),
    head(d[c("Sepal.Length", "Petal.Length", "Species", "NewFac")]),
    ignore_attr = TRUE
  )
})

test_that("lm with poly and NA in response", {
  s1 <- summary(iris$Sepal.Length)
  model <- lm(Petal.Length ~ log(Sepal.Width) + Sepal.Length,
    data = iris
  )
  # Same min-max
  s2 <- summary(insight::get_data(model)$Sepal.Length)

  model <- lm(Petal.Length ~ log(1 + Sepal.Width) + Sepal.Length,
    data = iris
  )
  s3 <- summary(insight::get_data(model)$Sepal.Length)

  model <- lm(Petal.Length ~ log(Sepal.Width + 1) + Sepal.Length,
    data = iris
  )
  s4 <- summary(insight::get_data(model)$Sepal.Length)

  model <- lm(Petal.Length ~ log1p(Sepal.Width) + Sepal.Length,
    data = iris
  )
  s5 <- summary(insight::get_data(model)$Sepal.Length)

  expect_equal(s1, s2, tolerance = 1e-4)
  expect_equal(s1, s3, tolerance = 1e-4)
  expect_equal(s1, s4, tolerance = 1e-4)
  expect_equal(s1, s5, tolerance = 1e-4)
  expect_equal(s2, s3, tolerance = 1e-4)
  expect_equal(s2, s4, tolerance = 1e-4)
  expect_equal(s2, s5, tolerance = 1e-4)
  expect_equal(s3, s4, tolerance = 1e-4)
  expect_equal(s3, s5, tolerance = 1e-4)
  expect_equal(s4, s5, tolerance = 1e-4)
})


mod <- lm(mpg ~ as.logical(am) + factor(cyl) + as.factor(gear), mtcars)
out <- get_data(mod)
test_that("logicals", {
  expect_equal(out$am, mtcars$am, ignore_attr = TRUE)
})


# See #689
test_that("get_data() log transform", {
  set.seed(123)
  x <- abs(rnorm(100, sd = 5)) + 5
  y <- exp(2 + 0.3 * x + rnorm(100, sd = 0.4))
  dat <- data.frame(y, x)

  mod <- lm(log(y) ~ log(x), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_identical(find_response(mod), "y")
  expect_identical(find_response(mod, combine = FALSE), "y")

  mod <- lm(log(y) ~ x, data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_identical(find_response(mod), "y")

  mod <- lm(y ~ log(x), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_identical(find_response(mod), "y")

  mod <- lm(y ~ log(1 + x), data = dat)
  expect_equal(
    head(insight::get_data(mod)[c("y", "x")]),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_identical(find_response(mod), "y")

  mod <- lm(y ~ log(x + 1), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  mod <- lm(log(y) ~ log(1 + x), data = dat)
  expect_equal(
    head(insight::get_data(mod)[c("y", "x")]),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  mod <- lm(log(y) ~ log(x + 1), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  mod <- lm(log(1 + y) ~ log(1 + x), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_identical(find_response(mod), "y")
  expect_identical(find_response(mod, combine = FALSE), "y")

  mod <- lm(log(y + 1) ~ log(x + 1), data = dat)
  expect_equal(
    head(insight::get_data(mod)),
    head(dat),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("log-offset", {
  skip_if_not_installed("MASS")
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = c(31, 35, 21, 30, 37, 26, 45, 21, 74, 27, 37, 37, 31, 37, 25),
    offset_1 = c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17)
  )
  moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)
  out <- get_data(moff, source = "frame")
  expect_equal(
    out$offset_1,
    c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17),
    tolerance = 1e-3
  )
})


skip_on_cran()

m <- lm(Sepal.Length ~ Sepal.Width, data = iris)
out <- get_data(m)
test_that("subsets", {
  expect_identical(colnames(out), c("Sepal.Length", "Sepal.Width"))
  expect_identical(nrow(out), 150L)
})

m <- lm(Sepal.Length ~ Sepal.Width, data = iris, subset = Species == "versicolor")
out <- get_data(m)
test_that("subsets", {
  expect_identical(colnames(out), c("Sepal.Length", "Sepal.Width", "Species"))
  expect_identical(nrow(out), 50L)
})

# d <- iris
# m <- lm(Petal.Length ~ poly(Sepal.Length), data = d)
# d <<- mtcars
# expect_warning(expect_warning(out <- get_data(m)))
# expect_equal(colnames(out), c("Petal.Length", "Sepal.Length"))

test_that("log", {
  m <- lm(log(Sepal.Length) ~ sqrt(Sepal.Width), data = iris)
  out <- get_data(m)
  expect_equal(out, iris[c("Sepal.Length", "Sepal.Width")], ignore_attr = TRUE)
})

test_that("log II", {
  m <- lm(log(Sepal.Length) ~ scale(Sepal.Width), data = iris)
  out <- get_data(m)
  expect_equal(out, iris[c("Sepal.Length", "Sepal.Width")], ignore_attr = TRUE)
})


test_that("workaround bug in estimatr", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("estimatr")
  data("CigaretteDemand", package = "ivreg")
  m <- estimatr::iv_robust(
    log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
    data = CigaretteDemand
  )
  out <- get_data(m)
  expect_equal(
    head(out$packs),
    c(101.08543, 111.04297, 71.95417, 56.85931, 82.58292, 79.47219),
    tolerance = 1e-3
  )
  expect_equal(
    colnames(out),
    c("packs", "rprice", "rincome", "salestax"),
    tolerance = 1e-3
  )
})


test_that("get_data colnames", {
  skip_on_os("windows")
  skip_if_not_installed("brms")
  m <- suppressMessages(suppressWarnings(brms::brm(mpg ~ hp + mo(cyl), data = mtcars, refresh = 0, iter = 200, chains = 1)))
  out <- get_data(m)
  expect_type(out$cyl, "double")
  expect_true(all(colnames(out) %in% c("mpg", "hp", "cyl")))
  out <- get_data(m, additional_variables = TRUE)
  expect_true("qsec" %in% colnames(out))
})


test_that("get_data works for fixest inside functions", {
  skip_if_not_installed("fixest")
  data(mtcars)

  # fit within function
  fixest_wrapper1 <- function(data) {
    data$cylinders <- factor(data$cyl)
    fit <- fixest::feglm(mpg ~ cylinders * disp + hp, data = data)
    fit
  }
  global_fixest1 <- fixest_wrapper1(data = mtcars)
  data <- mtcars[, c("mpg", "disp")]
  expect_named(
    get_data(global_fixest1),
    c("mpg", "cylinders", "disp", "hp")
  )

  # fit within function, subset
  fixest_wrapper2 <- function(data) {
    data$cylinders <- factor(data$cyl)
    fit <- fixest::feglm(mpg ~ cylinders * disp + hp, data = data)
    fit
  }
  data <- mtcars
  global_fixest2 <- fixest_wrapper2(data = data[1:20, ])
  expect_identical(nrow(get_data(global_fixest2)), 20L)
  expect_named(
    get_data(global_fixest2),
    c("mpg", "cylinders", "disp", "hp")
  )

  data(mtcars)
  d_cyl <- mtcars
  d_cyl$cylinders <- factor(d_cyl$cyl)
  global_fixest3 <- fixest::feglm(mpg ~ cylinders * disp + hp, data = d_cyl)
  expect_named(
    get_data(global_fixest3),
    c("mpg", "cylinders", "disp", "hp")
  )

  # regular example
  data(iris)
  res <- fixest::feglm(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris, "poisson")
  expect_named(
    get_data(res),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species")
  )
})


test_that("get_data, division in I()", {
  skip_if_not_installed("betareg")
  data("FoodExpenditure", package = "betareg")
  m2 <- betareg::betareg(I(food / income) ~ income + persons, data = FoodExpenditure)
  expect_named(
    get_data(m2, source = "mf"),
    c("I(food/income)", "income", "persons", "food", "income.1")
  )
})


test_that("get_data, can't parse subset", {
  data(mtcars)
  fit_mod <- function(formula, data, subset) {
    lm(
      as.formula(formula),
      data = data,
      subset = eval(parse(text = subset))
    )
  }
  m <- fit_mod(formula = "mpg~gear", data = mtcars, subset = "cyl != 8")
  expect_warning(
    {
      out <- get_data(m)
    },
    regex = "Looks like the original"
  )
  expect_named(out, c("mpg", "gear"))
  expect_identical(nrow(out), 32L)

  m <- lm(mpg ~ gear, data = mtcars, subset = cyl != 8)
  out <- get_data(m)
  expect_named(out, c("mpg", "gear", "cyl"))
  expect_identical(nrow(out), 18L)
})
