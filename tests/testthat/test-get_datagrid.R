if ( requiet("insight") && getRversion() >= "4.0.0") {
  m1 <- lm(hp ~ ordered(cyl), data = mtcars)
  m2 <- lm(hp ~ as.ordered(cyl), data = mtcars)
  m3 <- lm(hp ~ as.factor(cyl), data = mtcars)
  m4 <- lm(hp ~ factor(cyl), data = mtcars)

  test_that("get_datagrid - data from models", {
    expect_true(attributes(get_data(m1)$cyl)$factor)
    expect_equal(get_datagrid(m1)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m2)$cyl)$factor)
    expect_equal(get_datagrid(m2)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m3)$cyl)$factor)
    expect_equal(get_datagrid(m3)$cyl, c(4, 6, 8))
    expect_true(attributes(get_data(m4)$cyl)$factor)
    expect_equal(get_datagrid(m4)$cyl, c(4, 6, 8))
  })

  # get_datagrid() preserves all factor levels #695
  test_that("get_datagrid - preserve factor levels #695", {
    dat <<- transform(mtcars, cyl = factor(cyl))
    mod <- lm(mpg ~ cyl + am + hp, data = dat)
    grid <- get_datagrid(mod, at = "hp")
    expect_equal(levels(grid$cyl), c("4", "6", "8"))
  })

  m <- lm(Sepal.Width ~ Petal.Length + Petal.Width + Species, data = iris)
  # adjusted for works
  test_that("get_datagrid - adjusted for works", {
    dg <- insight::get_datagrid(m, "Species")
    expect_equal(attributes(dg)$adjusted_for, c("Petal.Length", "Petal.Width"))
  })

  # bracket tokens
  test_that("get_datagrid - terciles, quartiles, mean-sd", {
    dg <- insight::get_datagrid(m, "Petal.Width = [quartiles]")
    expect_equal(dg$Petal.Width, unname(quantile(iris$Petal.Width)), tolerance = 1e-4)
    expect_equal(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

    dg <- insight::get_datagrid(m, "Petal.Width = [meansd]")
    .center <- mean(iris$Petal.Width)
    .spread <- sd(iris$Petal.Width)
    expect_equal(dg$Petal.Width, .center + c(-1, 0, 1) * .spread, tolerance = 1e-4)
    expect_equal(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

    dg <- insight::get_datagrid(m, "Petal.Width = [terciles]")
    expect_equal(dg$Petal.Width, unname(quantile(iris$Petal.Width, probs = (1:2) / 3)), tolerance = 1e-4)
    expect_equal(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))
  })

  # bracket tokens
  test_that("get_datagrid - range = grid", {
    dg <- insight::get_datagrid(m, "Petal.Width", range = "grid")
    expect_equal(
      dg$Petal.Width,
      c(
        `-4 SD` = -1.8496, `-3 SD` = -1.0874, `-2 SD` = -0.3251, `-1 SD` = 0.4371,
        Mean = 1.1993, `+1 SD` = 1.9616, `+2 SD` = 2.7238, `+3 SD` = 3.486,
        `+4 SD` = 4.2483, `+5 SD` = 5.0105
      ),
      tolerance = 1e-3
    )
    expect_equal(attributes(dg)$adjusted_for, c("Petal.Length", "Species"))

    m <- lm(Sepal.Width ~ Petal.Length + Petal.Width * Species, data = iris)
    dg <- insight::get_datagrid(m, c("Species", "Petal.Width"), range = "grid", preserve_range = FALSE)
    expect_equal(
      dg$Petal.Width,
      c(
        `-1 SD` = 0.4371, Mean = 1.1993, `+1 SD` = 1.9616, `-1 SD` = 0.4371,
        Mean = 1.1993, `+1 SD` = 1.9616, `-1 SD` = 0.4371, Mean = 1.1993,
        `+1 SD` = 1.9616
      ),
      tolerance = 1e-3
    )
    expect_equal(attributes(dg)$adjusted_for, "Petal.Length")
  })

  # order of columns
  test_that("get_datagrid - column order", {
    m <- lm(Sepal.Width ~ Petal.Length + Petal.Width * Species, data = iris)
    dg <- insight::get_datagrid(m, c("Petal.Width", "Species"))
    expect_equal(colnames(dg), c("Petal.Width", "Species", "Petal.Length"))
    dg <- insight::get_datagrid(m, c("Species", "Petal.Width"))
    expect_equal(colnames(dg), c("Species", "Petal.Width", "Petal.Length"))
  })


  # list-argument
  test_that("get_datagrid - list-argument", {
    at <- list(Sepal.Length = c(3, 5), Species = c("versicolor", "virginica"))
    dg1 <- get_datagrid(iris, at = at)
    at <- c("Sepal.Length = c(3, 5)", "Species = c('versicolor', 'virginica')")
    dg2 <- get_datagrid(iris, at = at)

    expect_equal(dg1, dg2, tolerance = 1e-4)
  })
}


if ( requiet("insight") && requiet("bayestestR") && getRversion() >= "4.0.0") {
  test_that("get_datagrid - data", {
    # Factors
    expect_equal(length(get_datagrid(iris$Species)), 3)
    expect_equal(length(get_datagrid(c("A", "A", "B"))), 2)
    expect_equal(length(get_datagrid(x = iris$Species, at = "c('versicolor')")), 1)
    expect_equal(length(get_datagrid(iris$Species, at = "A = c('versicolor')")), 1)
    expect_equal(length(get_datagrid(c("A", "A", "B"), at = "dupa = 'A'")), 1)
    expect_equal(length(get_datagrid(iris$Species, at = "['versicolor', 'virginica']")), 2)
    expect_equal(length(get_datagrid(iris$Species, at = "[versicolor, virginica]")), 2)

    # Numerics
    expect_equal(length(get_datagrid(x = iris$Sepal.Length)), 10)
    expect_equal(length(get_datagrid(x = iris$Sepal.Length, length = 5)), 5)
    expect_equal(length(get_datagrid(x = iris$Sepal.Length, length = NA)), length(unique(iris$Sepal.Length)))
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "iqr")), as.numeric(quantile(iris$Sepal.Length, 0.025)))
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "hdi")), as.numeric(bayestestR::hdi(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2])
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "eti")), as.numeric(bayestestR::eti(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2])
    expect_equal(length(get_datagrid(iris$Sepal.Length, at = "c(1, 3, 4)")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, at = "A = c(1, 3, 4)")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, at = "[1, 3, 4]")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, at = "[1, 4]")), 10)
    expect_equal(length(get_datagrid(iris$Sepal.Length, range = "sd", length = 10)), 10)
    expect_equal(as.numeric(get_datagrid(iris$Sepal.Length, range = "sd", length = 3)[2]), mean(iris$Sepal.Length))
    expect_equal(as.numeric(get_datagrid(iris$Sepal.Length, range = "mad", length = 4)[2]), median(iris$Sepal.Length))

    # Dataframes
    expect_equal(nrow(get_datagrid(iris, length = 2)), 48)
    expect_equal(nrow(get_datagrid(iris, at = "Species", length = 2, numerics = 0)), 3)
    expect_equal(nrow(get_datagrid(iris, at = "Sepal.Length", length = 3)), 3)
    expect_equal(dim(get_datagrid(iris, at = 1:2, length = 3)), c(9, 5))
    expect_equal(dim(get_datagrid(iris, at = 1:2, length = c(3, 2))), c(6, 5))
    expect_equal(dim(get_datagrid(iris, at = 1:2, length = c(NA, 2))), c(70, 5))
    expect_equal(dim(get_datagrid(iris, at = c("Sepal.Length = c(1, 2)"), length = NA)), c(2, 5))
    expect_error(get_datagrid(iris, at = 1:2, length = c(3, 2, 4)))
    expect_error(get_datagrid(iris, at = 1:2, length = "yes"))
    expect_equal(as.numeric(get_datagrid(iris, at = 1:2, range = c("range", "mad"), length = c(2, 3))[4, "Sepal.Width"]), median(iris$Sepal.Width))


    expect_equal(nrow(get_datagrid(data.frame(
      X = c("A", "A", "B"),
      Y = c(1, 5, 2)
    ), at = "Y", factors = "mode", length = 5)), 5)

    expect_equal(nrow(get_datagrid(iris, at = c("Sepal.Length = 3", "Species"))), 3)
    expect_equal(nrow(get_datagrid(iris, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))), 2)

    x1 <- get_datagrid(iris, at = c("Species", "Sepal.Length"), length = 30, preserve_range = TRUE)
    expect_equal(dim(x1), c(55, 5))
    x2 <- get_datagrid(iris[c("Species", "Sepal.Length")], length = 30, preserve_range = TRUE)
    expect_equal(dim(x2), c(55, 2))
  })
}



if ( requiet("insight") && requiet("gamm4") && getRversion() >= "4.0.0" && requiet("glmmTMB") && requiet("mgcv") && requiet("rstanarm")) {
  test_that("get_datagrid - models", {
    # GLM
    mod <- glm(Petal.Length ~ Petal.Width * Sepal.Length, data = iris)
    expect_equal(dim(get_datagrid(mod)), c(100, 2))

    mod <- glm(Petal.Length ~ Petal.Width * Species, data = iris)
    expect_equal(dim(get_datagrid(mod)), c(10, 2))


    # LMER4
    mod <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(10, 2))
    expect_equal(unique(get_datagrid(mod, include_random = FALSE)$Species), 0)

    # GLMMTMB
    mod <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(10, 2))
    expect_equal(unique(get_datagrid(mod, include_random = FALSE)$Species), NA)

    # MGCV
    mod <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(100, 2))
    expect_equal(dim(get_datagrid(mod, include_smooth = FALSE)), c(10, 1))
    expect_equal(dim(get_datagrid(mod, include_smooth = "fixed")), c(10, 2))

    mod <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))

    # GAMM4
    mod <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10, 2))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))

    # MGCV, splines with variables, see #678
    data(mtcars)
    mod <- mgcv::gam(mpg ~ s(wt, k = 3), data = mtcars)
    out1 <- insight::get_datagrid(mod)
    k <- 3
    mod <- mgcv::gam(mpg ~ s(wt, k = k), data = mtcars)
    out2 <- insight::get_datagrid(mod)
    expect_equal(out1, out2, ignore_attr = TRUE, tolerance = 1e-4)


    # STAN_GAMM4
    mod <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris, iter = 100, chains = 2, refresh = 0))
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10, 2))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))
  })


  # test if factor levels as reference / non-focal terms works
  if ( requiet("insight")) {
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
        at = "k618", range = "grid", preserve_range = FALSE,
        verbose = TRUE, include_response = FALSE
      )
    )

    grid <- insight::get_datagrid(
      model,
      at = "k618", range = "grid", preserve_range = FALSE,
      verbose = FALSE, include_response = TRUE
    )
    expect_equal(
      sapply(grid, class),
      c(
        k618 = "numeric", lfp = "factor", wc = "factor", hc = "factor",
        inc = "numeric"
      )
    )

    out <- get_modelmatrix(model, data = grid)
    expect_equal(
      colnames(out),
      c("(Intercept)", "k618", "wcyes", "hcyes", "inc")
    )
  }
}
