osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (requiet("testthat") && requiet("insight") && requiet("mgcv") && requiet("gamm4") && requiet("rstanarm") && !osx) {
  set.seed(2) ## simulate some data...
  void <- capture.output(
    dat <- mgcv::gamSim(1, n = 400, dist = "normal", scale = 2)
  )

  bt <- mgcv::gam(y ~ te(x0, x1, k = 7) + s(x2) + s(x3),
    data = dat,
    method = "REML"
  )

  test_that("find_smooth - gam", {
    expect_equal(find_smooth(bt), list(smooth_terms = c("te(x0, x1, k = 7)", "s(x2)", "s(x3)")))
    expect_equal(find_smooth(bt, flatten = TRUE), c("te(x0, x1, k = 7)", "s(x2)", "s(x3)"))
  })

  test_that("find_smooth - mgcv::gamm", {
    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
    expect_equal(find_smooth(model, flatten = TRUE), "s(Sepal.Length)")
  })

  test_that("find_smooth - gamm4", {
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
    expect_equal(find_smooth(model, flatten = TRUE), "s(Sepal.Length)")
  })

  .runStanTest <- Sys.getenv("RunAllinsightStanTests") == "yes"
  if (.runStanTest) {
    test_that("find_smooth - stan_gamm4", {
      model <- suppressWarnings(
        rstanarm::stan_gamm4(
          Petal.Length ~ Petal.Width + s(Sepal.Length),
          random =  ~ (1 | Species),
          data = iris,
          iter = 100,
          chains = 1,
          refresh = 0
        )
      )
      expect_equal(find_smooth(model, flatten = TRUE), "s(Sepal.Length)")
    })
  }

  # test_that("find_smooth - brms", {
  #   model <- brms::brm(Petal.Length ~ Petal.Width + s(Sepal.Length) + (1|Species), data = iris, iter=100, chains=1, refresh=0)
  #   expect_equal(find_smooth(model, flatten = TRUE), "s(Sepal.Length)")
  # })
}
