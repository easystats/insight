test_that("get_datagrid - data from models", {
  m1 <- lm(hp ~ ordered(cyl), data = mtcars)
  m2 <- lm(hp ~ as.ordered(cyl), data = mtcars)
  m3 <- lm(hp ~ as.factor(cyl), data = mtcars)
  m4 <- lm(hp ~ factor(cyl), data = mtcars)
  expect_identical(get_datagrid(m1)$cyl, c(4, 6, 8))
  expect_identical(get_datagrid(m2)$cyl, c(4, 6, 8))
  expect_identical(get_datagrid(m3)$cyl, c(4, 6, 8))
  expect_identical(get_datagrid(m4)$cyl, c(4, 6, 8))
})

# get_datagrid() preserves all factor levels #695
test_that("get_datagrid - preserve factor levels #695", {
  dat <<- transform(mtcars, cyl = factor(cyl))
  mod <- lm(mpg ~ cyl + am + hp, data = dat)
  grid <- get_datagrid(mod, by = "hp")
  expect_identical(levels(grid$cyl), c("4", "6", "8"))
})

# adjusted for works
test_that("get_datagrid - adjusted for works", {
  data(iris)
  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width + Species, data = iris)
  dg <- insight::get_datagrid(m, "Species")
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Petal.Width"))
})

# bracket tokens
test_that("get_datagrid - terciles, quartiles, mean-sd", {
  data(iris)
  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width + Species, data = iris)
  dg <- insight::get_datagrid(m, "Petal.Width = [quartiles]")
  expect_equal(dg$Petal.Width, unname(quantile(iris$Petal.Width)[2:4]), tolerance = 1e-4)
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  dg <- insight::get_datagrid(m, "Petal.Width = [meansd]")
  .center <- mean(iris$Petal.Width)
  .spread <- sd(iris$Petal.Width)
  expect_equal(dg$Petal.Width, round(.center + c(-1, 0, 1) * .spread, 3), tolerance = 1e-4)
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  dg <- insight::get_datagrid(m, "Petal.Width = [terciles]")
  expect_equal(dg$Petal.Width, unname(round(quantile(iris$Petal.Width, probs = (1:2) / 3), 3)), tolerance = 1e-4)
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  dg <- insight::get_datagrid(m, "Petal.Width = [terciles2]")
  expect_equal(dg$Petal.Width, unname(round(quantile(iris$Petal.Width, probs = (0:3) / 3), 3)), tolerance = 1e-4)
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  dg <- insight::get_datagrid(m, "Petal.Width = [fivenum]")
  expect_equal(dg$Petal.Width, unname(quantile(iris$Petal.Width)), tolerance = 1e-4)
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  dg <- insight::get_datagrid(iris, by = "Petal.Width = [meansd]")
  expect_equal(dg$Petal.Width, c(0.437, 1.199, 1.962), tolerance = 1e-5)
  dg <- insight::get_datagrid(iris, by = "Petal.Width = [meansd]", digits = 1)
  expect_equal(dg$Petal.Width, c(0.4, 1.2, 2.0), tolerance = 1e-5)

  set.seed(123)
  dg <- insight::get_datagrid(m, "Petal.Width = [sample 8]")
  set.seed(123)
  expect_equal(dg$Petal.Width, sample(iris$Petal.Width, 8), tolerance = 1e-4)

  expect_error(
    insight::get_datagrid(m, "Petal.Width = [sample a]"),
    regex = "must be followed"
  )

  dg <- insight::get_datagrid(m, "Species=[setosa]")
  expect_identical(dim(dg), c(1L, 3L))

  dg <- insight::get_datagrid(m, "Species=[setosa,versicolor]")
  expect_identical(dim(dg), c(2L, 3L))

  expect_error(
    insight::get_datagrid(m, "Species=[setosa,wersicolor]"),
    regex = "should either indicate"
  )

  expect_error(
    insight::get_datagrid(m, "Species=[petosa]"),
    regex = "should either indicate"
  )

  skip_if_not_installed("datawizard")
  data(efc_insight, package = "insight")
  efc_insight$c161sex <- datawizard::to_factor(efc_insight$c161sex)
  efc_insight$e16sex <- datawizard::to_factor(efc_insight$e16sex)
  model <- lm(neg_c_7 ~ barthtot + c160age * c161sex, data = efc_insight)
  out <- insight::get_datagrid(model, by = c("c160age=[pretty]", "c161sex"))
  expect_identical(out$c160age, c(20, 40, 60, 80, 20, 40, 60, 80))
})

# bracket tokens
test_that("get_datagrid - range = grid", {
  data(iris)
  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width + Species, data = iris)
  dg <- insight::get_datagrid(m, "Petal.Width", range = "grid")
  expect_equal(
    dg$Petal.Width,
    c(0.1, 0.367, 0.633, 0.9, 1.167, 1.433, 1.7, 1.967, 2.233, 2.5),
    tolerance = 1e-2
  )
  expect_identical(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width * Species, data = iris)
  dg <- insight::get_datagrid(m, c("Species", "Petal.Width"), range = "grid", preserve_range = FALSE)
  expect_equal(
    dg$Petal.Width,
    c(
      `-1 SD` = 0.4371, Mean = 1.1993, `+1 SD` = 1.9616, `-1 SD` = 0.4371, # nolint
      Mean = 1.1993, `+1 SD` = 1.9616, `-1 SD` = 0.4371, Mean = 1.1993, # nolint
      `+1 SD` = 1.9616 # nolint
    ),
    tolerance = 1e-3
  )
  expect_identical(attributes(dg)$adjusted_for, "Petal.Length")
})

# order of columns
test_that("get_datagrid - column order", {
  data(iris)
  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width * Species, data = iris)
  dg <- insight::get_datagrid(m, c("Petal.Width", "Species"))
  expect_identical(colnames(dg), c("Petal.Width", "Species", "Petal.Length"))
  dg <- insight::get_datagrid(m, c("Species", "Petal.Width"))
  expect_identical(colnames(dg), c("Species", "Petal.Width", "Petal.Length"))
})


# list-argument
test_that("get_datagrid - list-argument", {
  data(iris)
  by <- list(Sepal.Length = c(3, 5), Species = c("versicolor", "virginica"))
  dg1 <- get_datagrid(iris, by = by)
  by <- c("Sepal.Length = c(3, 5)", "Species = c('versicolor', 'virginica')")
  dg2 <- get_datagrid(iris, by = by)

  expect_equal(dg1, dg2, tolerance = 1e-4)
})


test_that("get_datagrid - data", {
  skip_if_not_installed("bayestestR")
  data(iris)

  # Factors
  expect_length(get_datagrid(iris$Species), 3)
  expect_length(get_datagrid(c("A", "A", "B")), 2)
  expect_length(get_datagrid(x = iris$Species, by = "c('versicolor')"), 1)
  expect_length(get_datagrid(iris$Species, by = "A = c('versicolor')"), 1)
  expect_length(get_datagrid(c("A", "A", "B"), by = "dupa = 'A'"), 1)
  expect_length(get_datagrid(iris$Species, by = "['versicolor', 'virginica']"), 2)
  expect_length(get_datagrid(iris$Species, by = "[versicolor, virginica]"), 2)

  # Numerics
  expect_length(get_datagrid(x = iris$Sepal.Length), 10)
  expect_length(get_datagrid(x = iris$Sepal.Length, length = 5), 5)
  expect_length(get_datagrid(x = iris$Sepal.Length, length = NA), length(unique(iris$Sepal.Length)))
  expect_identical(min(get_datagrid(x = iris$Sepal.Length, range = "iqr", digits = 15)), as.numeric(quantile(iris$Sepal.Length, 0.025))) # nolint
  expect_identical(min(get_datagrid(x = iris$Sepal.Length, range = "hdi", digits = 15)), as.numeric(bayestestR::hdi(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2]) # nolint
  expect_identical(min(get_datagrid(x = iris$Sepal.Length, range = "eti", digits = 15)), as.numeric(bayestestR::eti(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2]) # nolint
  expect_length(get_datagrid(iris$Sepal.Length, by = "c(1, 3, 4)"), 3)
  expect_length(get_datagrid(iris$Sepal.Length, by = "A = c(1, 3, 4)"), 3)
  expect_length(get_datagrid(iris$Sepal.Length, by = "[1, 3, 4]"), 3)
  expect_length(get_datagrid(iris$Sepal.Length, by = "[1:4]"), 10)
  expect_length(get_datagrid(iris$Sepal.Length, by = "[1,4]"), 2)
  expect_length(get_datagrid(iris$Sepal.Length, range = "sd", length = 10), 10)
  expect_identical(as.numeric(get_datagrid(iris$Sepal.Length, range = "sd", length = 3)[2]), round(mean(iris$Sepal.Length), 3))
  expect_identical(as.numeric(get_datagrid(iris$Sepal.Length, range = "mad", length = 4)[2]), median(iris$Sepal.Length))

  # interaction of variables
  expect_identical(dim(get_datagrid(iris, "Species*Petal.Length")), c(30L, 5L))

  # Dataframes
  expect_identical(nrow(get_datagrid(iris, length = 2)), 48L)
  expect_identical(nrow(get_datagrid(iris, by = "Species", length = 2, numerics = 0)), 3L)
  expect_identical(nrow(get_datagrid(iris, by = "Sepal.Length", length = 3)), 3L)
  expect_identical(dim(get_datagrid(iris, by = 1:2, length = 3)), c(9L, 5L))
  expect_identical(dim(get_datagrid(iris, by = 1:2, length = c(3, 2))), c(6L, 5L))
  expect_identical(dim(get_datagrid(iris, by = 1:2, length = c(NA, 2))), c(70L, 5L))
  expect_identical(dim(get_datagrid(iris, by = "Sepal.Length = c(1, 2)", length = NA)), c(2L, 5L))
  expect_error(get_datagrid(iris, by = 1:2, length = c(3, 2, 4)))
  expect_error(get_datagrid(iris, by = 1:2, length = "yes"))
  expect_identical(as.numeric(get_datagrid(iris, by = 1:2, range = c("range", "mad"), length = c(2, 3))[4, "Sepal.Width"]), median(iris$Sepal.Width)) # nolint

  expect_identical(nrow(get_datagrid(data.frame(
    X = c("A", "A", "B"),
    Y = c(1, 5, 2),
    stringsAsFactors = FALSE
  ), by = "Y", factors = "mode", length = 5)), 3L)

  expect_identical(nrow(get_datagrid(data.frame(
    X = c("A", "A", "B"),
    Y = c(1.2, 5.5, 2),
    stringsAsFactors = FALSE
  ), by = "Y", factors = "mode", length = 5)), 5L)

  expect_identical(nrow(get_datagrid(iris, by = c("Sepal.Length = 3", "Species"))), 3L)
  expect_identical(nrow(get_datagrid(iris, by = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))), 2L)

  x1 <- get_datagrid(iris, by = c("Species", "Sepal.Length"), length = 30, preserve_range = TRUE)
  expect_identical(dim(x1), c(55L, 5L))
  x2 <- get_datagrid(iris[c("Species", "Sepal.Length")], length = 30, preserve_range = TRUE)
  expect_identical(dim(x2), c(55L, 2L))
})


test_that("get_datagrid - models", {
  # see https://github.com/georgheinze/logistf/pull/54
  skip_if(
    "as.character.formula" %in% methods(as.character),
    "Some package uses `formula.tools::as.character.formula()` which breaks `find_formula()`."
  )

  skip_if_not_installed("gamm4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("TMB")
  # GLM
  mod <- glm(Petal.Length ~ Petal.Width * Sepal.Length, data = iris)
  expect_identical(dim(get_datagrid(mod)), c(100L, 2L))

  mod <- glm(Petal.Length ~ Petal.Width * Species, data = iris)
  expect_identical(dim(get_datagrid(mod)), c(10L, 2L))


  # LMER4
  mod <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), c(10L, 2L))
  expect_identical(unique(get_datagrid(mod, include_random = FALSE)$Species), 0)

  # GLMMTMB
  skip_on_os("mac") # error: FreeADFunObject
  mod <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), c(10L, 2L))
  expect_identical(unique(get_datagrid(mod, include_random = FALSE)$Species), NA)

  # MGCV
  mod <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), c(100L, 2L))
  expect_identical(dim(get_datagrid(mod, include_smooth = FALSE)), c(10L, 1L))
  expect_identical(dim(get_datagrid(mod, include_smooth = "fixed")), c(10L, 2L))

  mod <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), c(63L, 3L))
  expect_identical(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10L, 1L))

  # GAMM4
  mod <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), c(63L, 3L))
  expect_identical(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10L, 2L))
  expect_identical(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10L, 1L))

  # MGCV, splines with variables, see #678

  mod <- mgcv::gam(mpg ~ s(wt, k = 3), data = mtcars)
  out1 <- insight::get_datagrid(mod)
  k <- 3
  mod <- mgcv::gam(mpg ~ s(wt, k = k), data = mtcars)
  out2 <- insight::get_datagrid(mod)
  expect_equal(out1, out2, ignore_attr = TRUE, tolerance = 1e-4)


  # STAN_GAMM4
  mod <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris, iter = 100, chains = 2, refresh = 0)) # nolint
  expect_identical(dim(get_datagrid(mod, include_random = TRUE)), as.integer(c(63, 3)))
  expect_identical(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), as.integer(c(10, 2)))
  expect_identical(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), as.integer(c(10, 1)))
})


test_that("get_datagrid - emmeans", {
  skip_if_not_installed("emmeans")

  data("mtcars")

  mod1 <- glm(am ~ hp + factor(cyl), family = binomial("logit"), data = mtcars)
  mod2 <- lm(mpg ~ hp + factor(cyl), data = mtcars)

  hp_vals <- c(50, 100)

  em1 <- emmeans::emmeans(mod1, ~ cyl | hp, at = list(hp = hp_vals))
  em2 <- emmeans::regrid(em1)
  em3 <- emmeans::emmeans(mod2, ~ cyl | hp, at = list(hp = hp_vals))

  res <- get_datagrid(em1)
  expect_identical(res, get_datagrid(em2))
  expect_identical(res, get_datagrid(em3))
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(6L, 2L))
  expect_true(all(c(4, 6, 8) %in% res[[1]]))
  expect_true(all(hp_vals %in% res[[2]]))

  res <- get_datagrid(emmeans::contrast(em1, method = "poly", max.degree = 2))
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(4L, 2L))
  expect_true("contrast" %in% colnames(res))
  expect_true(all(c("linear", "quadratic") %in% res[["contrast"]]))
  expect_true(all(hp_vals %in% res[["hp"]]))

  # emm_list
  em1 <- emmeans::emmeans(mod1, pairwise ~ cyl | hp, at = list(hp = hp_vals))
  em2 <- emmeans::emmeans(mod1, pairwise ~ cyl | hp, at = list(hp = hp_vals), regrid = TRUE)
  em3 <- emmeans::emmeans(mod2, pairwise ~ cyl | hp, at = list(hp = hp_vals))

  res <- get_datagrid(em1)
  expect_identical(res, get_datagrid(em2))
  expect_identical(res, get_datagrid(em3))
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(12L, 3L))
  expect_true("contrast" %in% colnames(res))
  expect_true(anyNA(res[["contrast"]]))
  expect_true(all(c(4, 6, 8, NA) %in% res[["cyl"]]))
  expect_true(all(hp_vals %in% res[["hp"]]))
})


test_that("get_datagrid - marginaleffects", {
  skip_if_not_installed("marginaleffects")

  data("mtcars")

  mod1 <- glm(am ~ hp + factor(cyl), family = binomial("logit"), data = mtcars)
  mod2 <- lm(mpg ~ hp + factor(cyl), data = mtcars)

  mp1 <- marginaleffects::avg_predictions(mod1,
    variables = list(hp = c(50, 100)),
    by = c("cyl", "hp")
  )
  mp2 <- marginaleffects::avg_predictions(mod2, variables = list(
    hp = c(50, 100),
    cyl = unique
  ))

  res <- get_datagrid(mp1)
  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(6L, 2L))
  expect_true(all(c(4, 6, 8) %in% res[[1]]))
  expect_true(all(c(50, 100) %in% res[[2]]))

  res2 <- get_datagrid(mp2)
  expect_s3_class(res2, "data.frame")
  expect_identical(dim(res2), c(6L, 2L))
  expect_true(all(c(4, 6, 8) %in% res2[[2]]))
  expect_true(all(c(50, 100) %in% res2[[1]]))


  mod <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  myme <- marginaleffects::comparisons(mod,
    variables = c("wt", "hp"),
    cross = TRUE,
    newdata = marginaleffects::datagrid(qsec = range)
  )
  res <- get_datagrid(myme)
  expect_true(all(c("wt", "mpg", "hp", "qsec") %in% colnames(res)))
  expect_true(all(c("contrast_hp", "contrast_wt") %in% colnames(res)))
})


test_that("get_datagrid - factor levels as reference / non-focal terms works", {
  d <- structure(list(
    lfp = structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L,
      2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
    ), levels = c(
      "no",
      "yes"
    ), class = "factor"), k5 = c(
      1L, 0L, 1L, 0L, 1L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 0L
    ),
    k618 = c(
      0L, 2L, 3L, 3L, 2L, 0L, 2L, 0L, 2L, 2L, 1L, 2L,
      0L, 0L, 2L, 1L, 1L, 0L, 0L, 1L, 0L, 0L
    ), age = c(
      32L, 30L,
      35L, 34L, 31L, 54L, 37L, 54L, 48L, 39L, 33L, 52L, 50L, 33L,
      44L, 41L, 45L, 53L, 53L, 42L, 32L, 56L
    ), wc = structure(c(
      1L,
      1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L,
      1L, 2L, 2L, 1L, 2L, 1L
    ), levels = c("no", "yes"), class = "factor"),
    hc = structure(c(
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L
    ), levels = c(
      "no",
      "yes"
    ), class = "factor"), lwg = c(
      1.2101647, 0.3285041,
      1.5141279, 0.0921151, 1.5242802, 1.5564855, 2.1202636, 2.0596387,
      0.7543364, 1.5448993, 1.4019157, 1.1889068, 0.9058391, 1.4934409,
      1.4913014, 1.2186279, 0.8697261, 1.0620506, 1.1860113, 0.8703095,
      1.2301208, 1.3053159
    ), inc = c(
      10.9100008, 19.5, 12.039999,
      6.8000002, 20.1000004, 9.8590002, 9.1520004, 10.9000006,
      17.3050003, 12.9250002, 24.3000011, 14.6000004, 21.6000004,
      24, 20.8829994, 19.5, 42.7999992, 41.5, 18.9650002, 16.1000004,
      14.6999998, 18.7999992
    )
  ), row.names = c(
    "1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10", "11", "500", "501", "502", "503",
    "504", "505", "506", "507", "508", "509", "510"
  ), class = "data.frame")

  model <- suppressWarnings(glm(lfp ~ k618 + wc + hc + inc, data = d, family = binomial(link = "logit")))

  expect_warning(
    insight::get_datagrid(
      model,
      by = "k618", range = "grid", preserve_range = FALSE,
      verbose = TRUE, include_response = FALSE
    )
  )

  grid <- insight::get_datagrid(
    model,
    by = "k618", range = "grid", preserve_range = FALSE,
    verbose = FALSE, include_response = TRUE
  )
  expect_identical(dim(grid), c(4L, 5L))
  expect_identical(
    sapply(grid, class),
    c(
      k618 = "numeric", lfp = "factor", wc = "factor", hc = "factor",
      inc = "numeric"
    )
  )

  out <- get_modelmatrix(model, data = grid)
  expect_identical(
    colnames(out),
    c("(Intercept)", "k618", "wcyes", "hcyes", "inc")
  )
})


test_that("get_datagrid - multiple weight variables", {
  skip_if_not_installed("nlme")
  data("Orthodont", package = "nlme")
  m <- nlme::lme( # a model of variance only
    distance ~ 1,
    data = Orthodont, # grand mean
    weights = nlme::varConstPower(form = ~ age | Sex)
  )
  out <- get_datagrid(m, include_response = TRUE)
  expect_equal(
    out$distance,
    c(
      16.5, 18.16667, 19.83333, 21.5, 23.16667, 24.83333, 26.5, 28.16667,
      29.83333, 31.5
    ),
    tolerance = 1e-3
  )
})


test_that("get_datagrid - include_random works with numeric group factors", {
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  mtcars$vs <- as.factor(mtcars$vs)
  model <- glmmTMB::glmmTMB(
    mpg ~ vs + (1 | cyl),
    data = mtcars
  )
  out <- get_datagrid(model, include_random = TRUE)
  expect_identical(
    out$cyl,
    structure(c(1L, 1L, 2L, 2L, 3L, 3L), levels = c("4", "6", "8"), class = "factor")
  )
  out <- get_datagrid(model, include_random = FALSE)
  expect_identical(out$cyl, c(NA, NA))
})


test_that("get_datagrid - include_random works with interacting random effects", {
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  mtcars[c("cyl", "gear", "vs")] <- lapply(mtcars[c("cyl", "gear", "vs")], as.factor)
  model <- glmmTMB::glmmTMB(mpg ~ hp + (1 | cyl:gear:vs), data = mtcars)
  out <- insight::get_datagrid(model, by = c("cyl", "gear", "vs"))
  expect_named(out, c("cyl", "gear", "vs", "hp"))
  expect_identical(dim(out), c(18L, 4L))
  out <- get_predicted(model, data = out, allow.new.levels = TRUE)
  expect_identical(dim(as.data.frame(out)), c(18L, 3L))
  expect_equal(
    as.numeric(out),
    c(
      20.66442, 20.66442, 17.99213, 20.66442, 19.67974, 20.66442,
      21.59251, 20.91067, 22.51425, 19.88026, 18.87143, 20.66442, 22.66211,
      18.64211, 20.66442, 23.89897, 20.66442, 20.66442
    ),
    tolerance = 1e-3
  )
})


test_that("get_datagrid - informative error when by not found", {
  data(iris)
  m <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  expect_error(
    insight::get_datagrid(m, by = list(Sepal.Something = c(10, 40, 50))),
    regex = "was not found"
  )
  expect_error(
    insight::get_datagrid(m, by = "Sepal.Something"),
    regex = "was not found"
  )
})


test_that("get_datagrid - include weights", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")

  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$wei_factor <- abs(rnorm(nrow(sleepstudy), 1, 0.4))

  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy, weights = wei_factor)
  d <- insight::get_datagrid(m, "Days")
  expect_named(d, c("Days", "Subject", "wei_factor"))

  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  d <- insight::get_datagrid(m, "Days")
  expect_named(d, c("Days", "Subject"))
})


test_that("get_datagrid - correctly handle multiple numerics for grid and range", {
  data(mtcars)
  # range of length 10 for both
  out <- get_datagrid(mtcars, c("mpg", "hp"), range = "range") # 10x10
  expect_identical(dim(out), c(100L, 11L))
  # grid creates range for first focal term and uses SD for remaining
  out <- get_datagrid(mtcars, c("mpg", "hp"), range = "grid") # 10x3
  expect_identical(dim(out), c(30L, 11L))
})


test_that("get_datagrid - handle integers", {
  d <- data.frame(
    x = 1:8,
    y = letters[1:8]
  )
  expect_identical(dim(get_datagrid(d)), c(64L, 2L)) # 8^2
  expect_identical(dim(get_datagrid(d, protect_integers = FALSE)), c(80L, 2L)) # 8 * length
  expect_identical(dim(get_datagrid(d, range = "grid")), c(64L, 2L))
  expect_identical(dim(get_datagrid(d, length = 5)), c(40L, 2L))

  d <- data.frame(
    x = 1:8,
    y = 21:28,
    z = letters[1:8]
  )
  expect_identical(dim(get_datagrid(d)), c(512L, 3L)) # 8^3
  expect_identical(dim(get_datagrid(d, range = "grid")), c(192L, 3L)) # 8 * 3 * 8
  expect_identical(dim(get_datagrid(d, length = 5)), c(200L, 3L))
  expect_identical(dim(get_datagrid(d, length = 5, range = "grid")), c(120L, 3L))
})


test_that("get_datagrid - names for length and range", {
  data(mtcars)
  out <- get_datagrid(mtcars[1:4], by = c("hp", "mpg"))
  expect_identical(dim(out), c(100L, 4L)) # 10x10 = 100, length is 10 by default
  out <- get_datagrid(mtcars[1:4], by = c("hp", "mpg"), length = 2)
  expect_identical(dim(out), c(4L, 4L))
  out <- get_datagrid(mtcars[1:4], by = c("hp", "mpg"), length = c(mpg = 2, hp = 3))
  expect_identical(dim(out), c(6L, 4L))
  expect_identical(n_unique(out$hp), 3L)
  out <- get_datagrid(mtcars[1:4], by = c("hp", "mpg"), length = c(mpg = 3, hp = 2))
  expect_identical(dim(out), c(6L, 4L))
  expect_identical(n_unique(out$hp), 2L)
  out <- get_datagrid(
    mtcars[1:4],
    by = c("hp", "mpg"),
    range = c(mpg = "iqr", hp = "sd"),
    length = c(mpg = 5, hp = 4)
  )
  expect_identical(dim(out), c(20L, 4L))
  expect_identical(n_unique(out$hp), 4L)
  # errors
  expect_error(
    get_datagrid(mtcars[1:4], by = c("hp", "mpg"), length = c(mpa = 3, hp = 2)),
    regex = "Names of"
  )
  expect_error(
    get_datagrid(mtcars[1:4], by = c("hp", "mpg"), range = c(mpa = "iqr", hp = "sd")),
    regex = "Names of"
  )
})


test_that("get_datagrid - colon for ranges, in combination with length", {
  data(iris)
  expect_error(
    get_datagrid(
      iris,
      by = c("Sepal.Width=[1:5]", "Petal.Widht=[1:3]"),
      length = c(Petal.Width = 3, Sepal.Width = 4)
    ),
    regex = "was not found in"
  )

  out <- get_datagrid(
    iris,
    by = c("Sepal.Width=[1:5]", "Petal.Width=[1:3]"),
    length = c(Petal.Width = 3, Sepal.Width = 4)
  )
  expect_identical(dim(out), c(12L, 5L))
  expect_identical(n_unique(out$Sepal.Width), 4L)
  expect_identical(n_unique(out$Petal.Width), 3L)

  out <- get_datagrid(
    iris,
    by = c("Sepal.Width=1:5", "Petal.Width=1:3"),
    length = c(Petal.Width = 3, Sepal.Width = 4)
  )
  expect_identical(dim(out), c(15L, 5L))
  expect_identical(
    out$Sepal.Width,
    c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L)
  )
  expect_identical(
    out$Petal.Width,
    c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L)
  )

  out <- get_datagrid(
    iris,
    by = c("Sepal.Width=1:5", "Petal.Width=1:3")
  )
  expect_identical(dim(out), c(15L, 5L))
  expect_identical(
    out$Sepal.Width,
    c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L)
  )
  expect_identical(
    out$Petal.Width,
    c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L)
  )

  out <- get_datagrid(
    iris,
    by = list(Sepal.Width = 1:5, Petal.Width = 1:3)
  )
  expect_identical(dim(out), c(15L, 5L))
  expect_identical(
    as.numeric(out$Sepal.Width),
    c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  )
  expect_identical(
    as.numeric(out$Petal.Width),
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
  )
})


skip_if_not_installed("withr")
withr::with_environment(
  new.env(),
  test_that("get_datagrid - functions", {
    data(iris)
    fun <<- function(x) x^2
    out <- get_datagrid(iris, by = "Sepal.Width = fun(2:5)")
    expect_identical(out$Sepal.Width, c(4, 9, 16, 25))
  })
)
