skip_on_cran()
skip_if_not_installed("cmdstanr")
skip_if_not_installed("withr")
skip_if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))

# normal model, which converges
cmdstan_normal <- cmdstanr::cmdstan_model(
  cmdstanr::write_stan_file(
    "data { int<lower=0> N; vector[N] y; }
    parameters { real mu; real<lower=0> sigma; }
    model { mu ~ normal(0, 10); sigma ~ exponential(1); y ~ normal(mu, sigma); }"
  ),
  quiet = TRUE
)
cmdstan_normal_data <- list(N = 32L, y = mtcars$mpg)

# centered eight schools model, which has sampling problems
cmdstan_schools <- cmdstanr::cmdstan_model(
  cmdstanr::write_stan_file(
    "data { int<lower=0> J; vector[J] y; vector<lower=0>[J] sigma; }
    parameters { real mu; real<lower=0> tau; vector[J] theta; }
    model {
      mu ~ normal(0, 5); tau ~ cauchy(0, 5);
      theta ~ normal(mu, tau); y ~ normal(theta, sigma);
    }"
  ),
  quiet = TRUE
)
cmdstan_schools_data <- list(
  J = 8L,
  y = c(28, 8, -3, 7, -1, 1, 18, 12),
  sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)

.cmdstan_sample <- function(model, data, ...) {
  suppressMessages(suppressWarnings(model$sample(
    data = data,
    seed = 123,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    ...
  )))
}

cmdstan_fit_normal <- .cmdstan_sample(
  cmdstan_normal,
  cmdstan_normal_data,
  chains = 2,
  save_warmup = TRUE
)
cmdstan_fit_schools <- .cmdstan_sample(
  cmdstan_schools,
  cmdstan_schools_data,
  chains = 3,
  max_treedepth = 3
)


# tests ------------------------------------------------------------------------

test_that("is_converged, CmdStanMCMC without problems", {
  expect_silent(is_converged(cmdstan_fit_normal))
  result <- is_converged(cmdstan_fit_normal)
  expect_true(result)
  expect_identical(
    attr(result, "diagnostics")$Threshold,
    c(0, 0, 0.2, 1.05, 200, 200)
  )
})

test_that("is_converged, CmdStanMCMC with sampling problems", {
  result <- is_converged(cmdstan_fit_schools, verbose = FALSE)
  expect_false(result)
  d <- attr(result, "diagnostics")
  expect_false(d$Passed[d$Diagnostic == "Treedepth"])
  expect_identical(d$Threshold[5:6], c(300, 300))
  expect_message(is_converged(cmdstan_fit_schools), "Treedepth")
  expect_silent(is_converged(cmdstan_fit_schools, verbose = FALSE))
})

test_that("is_converged, CmdStanMCMC with fixed parameters", {
  fit <- .cmdstan_sample(
    cmdstan_normal,
    cmdstan_normal_data,
    chains = 1,
    fixed_param = TRUE
  )
  expect_message(result <- is_converged(fit), "NUTS", fixed = TRUE)
  expect_identical(result, FALSE)
  expect_silent(is_converged(fit, verbose = FALSE))
})

test_that("is_converged, cmdstanr fits without MCMC draws", {
  fit <- suppressMessages(suppressWarnings(cmdstan_normal$optimize(
    data = cmdstan_normal_data,
    seed = 123,
    refresh = 0,
    show_messages = FALSE
  )))
  expect_message(result <- is_converged(fit), "NUTS", fixed = TRUE)
  expect_identical(result, FALSE)
  expect_silent(is_converged(fit, verbose = FALSE))
})

test_that("is_converged, CmdStanMCMC with a failed chain", {
  # chain 2 starts where the model rejects every proposal, so it has no draws.
  # The ESS threshold counts only the chain with draws
  model <- cmdstanr::cmdstan_model(
    cmdstanr::write_stan_file(
      "parameters { real mu; }
      model { if (mu > 1) reject(\"mu > 1\"); mu ~ normal(0, 0.1); }"
    ),
    quiet = TRUE
  )
  fit <- .cmdstan_sample(
    model,
    data = list(),
    chains = 2,
    init = list(list(mu = 0), list(mu = 5))
  )
  result <- is_converged(fit, verbose = FALSE)
  expect_identical(attr(result, "diagnostics")$Threshold[5:6], c(100, 100))
})

test_that("is_converged, CmdStanMCMC with two draws is silent", {
  # cmdstanr warns that E-BFMI cannot be computed from fewer than three draws
  fit <- .cmdstan_sample(
    cmdstan_normal,
    cmdstan_normal_data,
    chains = 1,
    iter_sampling = 2
  )
  expect_silent(is_converged(fit, verbose = FALSE))
  expect_warning(is_converged(fit), "E-BFMI")
  result <- suppressWarnings(is_converged(fit))
  expect_identical(attr(result, "diagnostics")$Value[3], NA_real_)
})

test_that("is_converged, CmdStanMCMC with another draws format", {
  # `$draws()` returns the format of the option `cmdstanr_draws_format`
  result <- withr::with_options(
    list(cmdstanr_draws_format = "draws_df"),
    is_converged(cmdstan_fit_schools, verbose = FALSE)
  )
  expect_identical(result, is_converged(cmdstan_fit_schools, verbose = FALSE))
})

test_that("is_converged, CmdStanMCMC without draws after warmup", {
  fit <- .cmdstan_sample(
    cmdstan_normal,
    cmdstan_normal_data,
    chains = 1,
    iter_sampling = 0
  )
  expect_message(result <- is_converged(fit), "no draws after warmup", fixed = TRUE)
  expect_identical(result, FALSE)
  expect_silent(is_converged(fit, verbose = FALSE))
})
