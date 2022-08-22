if (requiet("testthat") && requiet("insight")) {
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
}


if (requiet("testthat") && requiet("insight") && requiet("bayestestR")) {
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



if (requiet("testthat") && requiet("insight") && requiet("gamm4") && requiet("glmmTMB") && requiet("mgcv") && requiet("rstanarm")) {
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


    # STAN_GAMM4
    mod <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris, iter = 100, chains = 2, refresh = 0))
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10, 2))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))
  })
}
