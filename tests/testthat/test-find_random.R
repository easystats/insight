if (require("testthat") && require("insight") && require("mgcv") && require("gamm4") && require("rstanarm")) {

  test_that("find_smooth - mgcv::gamm", {
    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
    # expect_equal(insight::find_random(model, flatten = TRUE), "Species")
  })

  test_that("find_smooth - gamm4::gamm4", {
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~(1|Species), data = iris)
    # expect_equal(insight::find_random(model, flatten = TRUE), "Species")
  })

  test_that("find_smooth - rstanarm::gamm4", {
    model <- rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~(1|Species), data = iris, iter=100, chains=1, refresh=0)
    expect_equal(insight::find_random(model, flatten = TRUE), "Species")
  })
}
