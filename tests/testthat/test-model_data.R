.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest &&
  requiet("splines") &&
  requiet("TMB") &&
  requiet("glmmTMB") &&
  getRversion() >= "4.0.0") {
  data(iris)

  m1 <- lm(Sepal.Length ~ Species + ns(Petal.Width), data = iris)
  m2 <- lm(Sepal.Length ~ Species + ns(Petal.Width, knots = 2), data = iris)
  m3 <- lm(Sepal.Length ~ Species + bs(Petal.Width, degree = 3), data = iris)
  m4 <- lm(Sepal.Length ~ Species + bs(Petal.Width, degree = 1), data = iris)
  m5 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)


  test_that("get_data", {
    mf1 <- get_data(m1)
    mf2 <- get_data(m2)
    mf3 <- get_data(m3)
    mf4 <- get_data(m4)
    mf5 <- model.frame(m5)

    expect_identical(as.vector(mf1$Petal.Width), as.vector(mf5$Petal.Width))
    expect_identical(as.vector(mf2$Petal.Width), as.vector(mf5$Petal.Width))
    expect_identical(as.vector(mf3$Petal.Width), as.vector(mf5$Petal.Width))
    expect_identical(as.vector(mf4$Petal.Width), as.vector(mf5$Petal.Width))
  })

  data("Salamanders")
  skip_on_os("mac") # error: FreeADFunObject
  m <- glmmTMB(
    count ~ spp + cover + mined + poly(DOP, 3) + (1 | site),
    ziformula = ~ spp + mined,
    dispformula = ~DOY,
    data = Salamanders,
    family = nbinom2
  )

  test_that("get_data", {
    mf <- get_data(m)
    expect_identical(ncol(mf), 7L)
    expect_identical(
      colnames(mf),
      c("count", "spp", "cover", "mined", "DOP", "site", "DOY")
    )
  })
}
