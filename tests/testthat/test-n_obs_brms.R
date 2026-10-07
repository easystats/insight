skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("lme4")

data(cbpp, package = "lme4")

test_that("n_obs disaggregates brms models with trials() variable", {
  m <- suppressMessages(suppressWarnings(
    brms::brm(
      incidence | trials(size) ~ period,
      data = cbpp,
      family = "binomial",
      empty = TRUE
    )
  ))
  expect_identical(n_obs(m), 56L)
  expect_identical(n_obs(m, disaggregate = TRUE), 842L)
})

test_that("n_obs disaggregates brms models with constant trials()", {
  m <- suppressMessages(suppressWarnings(
    brms::brm(
      incidence | trials(30) ~ period,
      data = cbpp,
      family = "binomial",
      empty = TRUE
    )
  ))
  expect_identical(n_obs(m), 56L)
  expect_identical(n_obs(m, disaggregate = TRUE), 1680L)
})

test_that("n_obs disaggregates brms beta-binomial models with trials()", {
  m <- suppressMessages(suppressWarnings(
    brms::brm(
      incidence | trials(size) ~ period,
      data = cbpp,
      family = "beta_binomial",
      empty = TRUE
    )
  ))
  expect_identical(n_obs(m, disaggregate = TRUE), 842L)
})

test_that("n_obs ignores disaggregate for brms models without trials()", {
  m <- suppressMessages(suppressWarnings(
    brms::brm(size ~ period, data = cbpp, empty = TRUE)
  ))
  expect_identical(n_obs(m, disaggregate = TRUE), 56L)
})

test_that("n_obs disaggregates brms models with weights() and trials()", {
  cbpp_w <- cbpp
  cbpp_w$w <- 1
  m <- suppressMessages(suppressWarnings(
    brms::brm(
      incidence | weights(w) + trials(size) ~ period,
      data = cbpp_w,
      family = "binomial",
      empty = TRUE
    )
  ))
  expect_identical(n_obs(m), 56L)
  expect_identical(n_obs(m, disaggregate = TRUE), 842L)
})
