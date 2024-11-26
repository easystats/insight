skip_if_not_installed("phylolm")
skip_if_not_installed("ape")
skip_if_not_installed("MuMIn")
skip_if_not_installed("withr")
skip_on_cran()

withr::with_options(
  list(na.action = "na.fail"),
  test_that("phylolm modelinfo", {
    set.seed(123456)
    tre <- ape::rcoal(60)
    taxa <- sort(tre$tip.label)
    b0 <- 0
    b1 <- 1

    x <- phylolm::rTrait(
      n = 1,
      phy = tre,
      model = "BM",
      parameters = list(ancestral.state = 0, sigma2 = 10)
    )
    y <- b0 + b1 * x +
      phylolm::rTrait(
        n = 1,
        phy = tre,
        model = "lambda",
        parameters = list(
          ancestral.state = 0,
          sigma2 = 1,
          lambda = 0.5
        )
      )
    dat <- data.frame(trait = y[taxa], pred = x[taxa])
    mod <- phylolm::phylolm(
      trait ~ pred,
      data = dat,
      phy = tre,
      model = "lambda"
    )
    out <- model_info(mod)
    expect_true(out$is_linear)

    library(MuMIn) # nolint
    mod.d <- MuMIn::dredge(mod, rank = "AICc")
    mod.avg.fit <- MuMIn::model.avg(mod.d, revised.var = TRUE, fit = TRUE)
    out <- model_info(mod.avg.fit)
    expect_true(out$is_linear)
  })
)

unloadNamespace("MuMIn")
