skip_on_cran()
skip_if_not_installed("cmdstanr")
skip_if_not_installed("rstan")
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


# helpers ----------------------------------------------------------------------

# the fit as stanfit from `rstan::read_stan_csv()`. rstan 2.32.7 stops on the
# values `true` and `false` in the comments of the CSV files of CmdStan 2.40,
# so copies of the files have `1` and `0` in their place
.cmdstan_as_stanfit <- function(fit) {
  files <- vapply(
    fit$output_files(),
    function(file) {
      csv <- readLines(file)
      comments <- startsWith(csv, "#")
      csv[comments] <- sub("= true", "= 1", csv[comments], fixed = TRUE)
      csv[comments] <- sub("= false", "= 0", csv[comments], fixed = TRUE)
      out <- tempfile(fileext = ".csv")
      writeLines(csv, out)
      out
    },
    character(1)
  )
  rstan::read_stan_csv(files)
}

# compare the diagnostics with those of the stanfit from the same CSV files.
# Divergences and Treedepth are counts. E-BFMI uses the same formula in
# cmdstanr and rstan. R-hat and ESS come from posterior (cmdstanr) and rstan
# (stanfit), which can differ slightly. `Passed` is compared for the counts and
# for the other checks whose value is not close to the threshold.
.cmdstan_expect_stanfit <- function(fit) {
  stanfit <- .cmdstan_as_stanfit(fit)
  expect_identical(
    dim(as.array(stanfit))[1],
    as.integer(fit$metadata()$iter_sampling)
  )

  result <- is_converged(fit, verbose = FALSE)
  reference <- is_converged(stanfit, verbose = FALSE)
  d <- attr(result, "diagnostics")
  r <- attr(reference, "diagnostics")

  expect_identical(d$Diagnostic, r$Diagnostic)
  expect_identical(d$Threshold, r$Threshold)
  expect_identical(d$Value[1:2], r$Value[1:2])
  rel_diff <- abs(d$Value[3:6] - r$Value[3:6]) / abs(r$Value[3:6])
  expect_true(all(rel_diff <= 1e-3), info = toString(rel_diff))
  near <- abs(r$Value - r$Threshold) <= 1e-3 * abs(r$Threshold)
  near[1:2] <- FALSE
  expect_identical(d$Passed[!near], r$Passed[!near])
  result
}

# failed checks of is_converged()
.cmdstan_failed <- function(result) {
  d <- attr(result, "diagnostics")
  d$Diagnostic[!d$Passed]
}


# tests ------------------------------------------------------------------------

test_that("is_converged, CmdStanMCMC without problems", {
  result <- .cmdstan_expect_stanfit(cmdstan_fit_normal)
  expect_true(result)
  expect_length(.cmdstan_failed(result), 0)
  expect_identical(
    attr(result, "diagnostics")$Threshold,
    c(0, 0, 0.2, 1.05, 200, 200)
  )
  expect_silent(is_converged(cmdstan_fit_normal))
})

test_that("is_converged, CmdStanMCMC with sampling problems", {
  result <- .cmdstan_expect_stanfit(cmdstan_fit_schools)
  expect_false(result)
  expect_true("Treedepth" %in% .cmdstan_failed(result))
  expect_identical(attr(result, "diagnostics")$Threshold[5:6], c(300, 300))
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
  expect_message(is_converged(fit), "cannot be assessed")
  expect_message(is_converged(fit), "NUTS", fixed = TRUE)
  result <- suppressMessages(is_converged(fit))
  expect_false(result)
  expect_null(attr(result, "diagnostics"))
  expect_silent(is_converged(fit, verbose = FALSE))
  expect_false(is_converged(fit, verbose = FALSE))
})

test_that("is_converged, cmdstanr fits without MCMC draws", {
  fits <- suppressMessages(suppressWarnings(list(
    variational = cmdstan_normal$variational(
      data = cmdstan_normal_data,
      seed = 123,
      refresh = 0,
      show_messages = FALSE
    ),
    optimize = cmdstan_normal$optimize(
      data = cmdstan_normal_data,
      seed = 123,
      refresh = 0,
      show_messages = FALSE
    )
  )))
  expect_s3_class(fits$variational, "CmdStanVB")
  expect_s3_class(fits$optimize, "CmdStanMLE")
  for (fit in fits) {
    expect_message(is_converged(fit), "cannot be assessed")
    expect_message(is_converged(fit), "NUTS", fixed = TRUE)
    result <- suppressMessages(is_converged(fit))
    expect_false(result)
    expect_null(attr(result, "diagnostics"))
    expect_silent(is_converged(fit, verbose = FALSE))
  }
})

test_that("is_converged, CmdStanMCMC with a failed chain", {
  # chain 2 starts where the model rejects every proposal, so it has no draws.
  # The ESS threshold counts only the chain with draws, as for the stanfit
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
  expect_identical(fit$num_chains(), 2L)
  expect_length(fit$output_files(), 1)
  result <- .cmdstan_expect_stanfit(fit)
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
  expected <- is_converged(cmdstan_fit_schools, verbose = FALSE)
  for (format in c("draws_df", "draws_matrix", "draws_list")) {
    old <- options(cmdstanr_draws_format = format)
    result <- is_converged(cmdstan_fit_schools, verbose = FALSE)
    options(old)
    expect_identical(result, expected, info = format)
  }
})

test_that("is_converged, CmdStanMCMC without draws after warmup", {
  fit <- .cmdstan_sample(
    cmdstan_normal,
    cmdstan_normal_data,
    chains = 1,
    iter_sampling = 0
  )
  expect_message(is_converged(fit), "no draws after warmup", fixed = TRUE)
  result <- suppressMessages(is_converged(fit))
  expect_false(result)
  expect_null(attr(result, "diagnostics"))
  expect_silent(is_converged(fit, verbose = FALSE))
})
