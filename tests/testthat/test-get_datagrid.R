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
}

if (requiet("testthat") && requiet("insight") && requiet("bayestestR")) {
  test_that("get_datagrid - data", {

    # Factors
    expect_equal(length(get_datagrid(iris$Species)), 3)
    expect_equal(length(get_datagrid(c("A", "A", "B"))), 2)
    expect_equal(length(get_datagrid(x = iris$Species, target = "c('versicolor')")), 1)
    expect_equal(length(get_datagrid(iris$Species, target = "A = c('versicolor')")), 1)
    expect_equal(length(get_datagrid(c("A", "A", "B"), target = "dupa = 'A'")), 1)
    expect_equal(length(get_datagrid(iris$Species, target = "['versicolor', 'virginica']")), 2)
    expect_equal(length(get_datagrid(iris$Species, target = "[versicolor, virginica]")), 2)

    # Numerics
    expect_equal(length(get_datagrid(x = iris$Sepal.Length)), 10)
    expect_equal(length(get_datagrid(x = iris$Sepal.Length, length = 5)), 5)
    expect_equal(length(get_datagrid(x = iris$Sepal.Length, length = NA)), length(unique(iris$Sepal.Length)))
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "iqr")), as.numeric(quantile(iris$Sepal.Length, 0.025)))
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "hdi")), as.numeric(bayestestR::hdi(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2])
    expect_equal(min(get_datagrid(x = iris$Sepal.Length, range = "eti")), as.numeric(bayestestR::eti(iris$Sepal.Length, ci = 0.95, verbose = FALSE))[2])
    expect_equal(length(get_datagrid(iris$Sepal.Length, target = "c(1, 3, 4)")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, target = "A = c(1, 3, 4)")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, target = "[1, 3, 4]")), 3)
    expect_equal(length(get_datagrid(iris$Sepal.Length, target = "[1, 4]")), 10)
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
    expect_equal(dim(get_datagrid(mod, include_random=TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))

    # GAMM4
    mod <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
    expect_equal(dim(get_datagrid(mod, include_random=TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10, 2))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))


    # STAN_GAMM4
    mod <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris, iter = 100, chains = 2, refresh = 0))
    expect_equal(dim(get_datagrid(mod, include_random = TRUE)), c(63, 3))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = "fixed")), c(10, 2))
    expect_equal(dim(get_datagrid(mod, include_random = FALSE, include_smooth = FALSE)), c(10, 1))
  })
}


