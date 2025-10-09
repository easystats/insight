skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")
skip_on_os("mac")

test_that("clean_parameters blavaan", {
  skip_if_not_installed("blavaan")
  skip_if_not_installed("lavaan")
  skip_if_not_installed("Rcpp")
  suppressPackageStartupMessages(require(
    "blavaan",
    quietly = TRUE,
    warn.conflicts = FALSE
  )) # nolint

  data("PoliticalDemocracy", package = "lavaan")

  model <- "
    # latent variable definitions
    dem60 =~ y1 + a*y2
    dem65 =~ y5 + a*y6

    # regressions
    dem65 ~ dem60

    # residual correlations
    y1 ~~ y5
  "

  suppressWarnings(capture.output({
    bfit <- blavaan::bsem(
      model,
      data = PoliticalDemocracy,
      n.chains = 1,
      burnin = 50,
      sample = 100
    )
  }))

  out <- clean_parameters(bfit)
  expect_true(inherits(out, "clean_parameters")) # nolint
})


test_that("clean_parameters stanrag", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")

  data(sleepstudy, package = "lme4")
  m <- insight::download_model("stanreg_merMod_6")
  skip_if(is.null(m))

  out <- clean_parameters(m)

  expect_identical(
    out$Group,
    c(
      "",
      "",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Intercept: Subject",
      "Days: Subject",
      "Var/Cov: Subject",
      "Var/Cov: Subject",
      "Var/Cov: Subject",
      ""
    )
  )

  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "sigma"
    )
  )
})


test_that("clean_parameters brms, sigma3", {
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_sigma_3")
  skip_if(is.null(m))

  out <- clean_parameters(m)

  expect_identical(
    out$Effects,
    c(
      "fixed",
      "fixed",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "fixed",
      "fixed",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random"
    )
  )

  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma",
      "sigma"
    )
  )

  expect_identical(
    out$Cleaned_Parameter,
    c(
      "(Intercept)",
      "Petal.Width",
      "Group.G1",
      "Group.G2",
      "Group.G3",
      "Group.G1",
      "Group.G2",
      "Group.G3",
      "(Intercept)",
      "Petal.Width",
      "Intercept ~ Petal.Width",
      "(Intercept)",
      "Petal.Width",
      "Group.G1",
      "Group.G2",
      "Group.G3",
      "Group.G1",
      "Group.G2",
      "Group.G3",
      "(Intercept)",
      "Petal.Width",
      "Intercept ~ Petal.Width"
    )
  )
})


test_that("clean_parameters brms, sigma1", {
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_sigma_1")
  skip_if(is.null(m))

  out <- clean_parameters(m)

  expect_identical(
    out$Effects,
    c("fixed", "fixed", "random", "random", "random", "random", "fixed", "fixed")
  )

  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "sigma",
      "sigma"
    )
  )

  expect_identical(
    out$Cleaned_Parameter,
    c("(Intercept)", "hp", "cyl.4", "cyl.6", "cyl.8", "(Intercept)", "(Intercept)", "cyl")
  )
})


test_that("clean_parameters brms, chocomini", {
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_chocomini_1")
  skip_if(is.null(m))

  out <- clean_parameters(m)

  expect_identical(
    out$Effects,
    c(
      "fixed",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "fixed",
      "fixed",
      "fixed",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random",
      "random"
    )
  )

  expect_identical(
    out$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "conditional",
      "delta",
      "k",
      "phi",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "delta",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k",
      "k"
    )
  )

  expect_identical(
    out$Cleaned_Parameter,
    c(
      "(Intercept)",
      "Participant.S001",
      "Participant.S002",
      "Participant.S003",
      "Participant.S004",
      "Participant.S005",
      "Participant.S006",
      "Participant.S007",
      "Participant.S008",
      "Participant.S009",
      "Participant.S010",
      "Participant.S011",
      "Participant.S012",
      "Participant.S013",
      "Participant.S014",
      "Participant.S015",
      "Participant.S016",
      "Participant.S017",
      "Participant.S018",
      "Participant.S019",
      "Participant.S020",
      "(Intercept)",
      "(Intercept)",
      "(Intercept)",
      "(Intercept)",
      "Participant.S001",
      "Participant.S002",
      "Participant.S003",
      "Participant.S004",
      "Participant.S005",
      "Participant.S006",
      "Participant.S007",
      "Participant.S008",
      "Participant.S009",
      "Participant.S010",
      "Participant.S011",
      "Participant.S012",
      "Participant.S013",
      "Participant.S014",
      "Participant.S015",
      "Participant.S016",
      "Participant.S017",
      "Participant.S018",
      "Participant.S019",
      "Participant.S020",
      "(Intercept)",
      "Participant.S001",
      "Participant.S002",
      "Participant.S003",
      "Participant.S004",
      "Participant.S005",
      "Participant.S006",
      "Participant.S007",
      "Participant.S008",
      "Participant.S009",
      "Participant.S010",
      "Participant.S011",
      "Participant.S012",
      "Participant.S013",
      "Participant.S014",
      "Participant.S015",
      "Participant.S016",
      "Participant.S017",
      "Participant.S018",
      "Participant.S019",
      "Participant.S020",
      "(Intercept)"
    )
  )
})
