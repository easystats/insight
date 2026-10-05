skip_on_cran()
skip_if_not_installed("rstan")
skip_if_not_installed("curl")
skip_if_not_installed("httr2")
skip_if_offline()

conv_brms <- suppressWarnings(download_model("brms_1"))
conv_stanreg <- suppressWarnings(download_model("stanreg_glm_1"))
skip_if(is.null(conv_brms) || is.null(conv_stanreg))

# both are stanfit objects with 4 chains of 1000 draws after warmup.
# conv_fit_stanreg sets `max_treedepth` to 15, conv_fit_brms uses the default 10
conv_fit_brms <- conv_brms$fit
conv_fit_stanreg <- conv_stanreg$stanfit


# helpers ----------------------------------------------------------------------

# rows of the post-warmup draws of a chain
.conv_post_warmup <- function(fit, chain) {
  n <- length(fit@sim$samples[[chain]][[1]])
  (fit@sim$warmup2[chain] + 1):n
}

# set a column of the sampler parameters (e.g. "divergent__") after warmup
.conv_set_sampler <- function(fit, chain, column, rows, value) {
  sp <- attr(fit@sim$samples[[chain]], "sampler_params")
  sp[[column]][.conv_post_warmup(fit, chain)[rows]] <- value
  attr(fit@sim$samples[[chain]], "sampler_params") <- sp
  fit
}

# replace the post-warmup draws of a parameter in a chain
.conv_set_draws <- function(fit, chain, parameter, values) {
  rows <- .conv_post_warmup(fit, chain)
  fit@sim$samples[[chain]][[parameter]][rows] <- values
  fit
}

# post-warmup draws of a parameter in a chain, each draw repeated `k` times,
# so that the draws are autocorrelated and the ESS is low
.conv_repeat_draws <- function(fit, chain, parameter, k) {
  draws <- fit@sim$samples[[chain]][[parameter]][.conv_post_warmup(fit, chain)]
  n <- length(draws)
  .conv_set_draws(fit, chain, parameter, rep(draws, each = k)[seq_len(n)])
}

# keep only some chains of a stanfit
.conv_keep_chains <- function(fit, chains) {
  fit@sim$samples <- fit@sim$samples[chains]
  fit@sim$chains <- length(chains)
  fit@sim$n_save <- fit@sim$n_save[chains]
  fit@sim$warmup2 <- fit@sim$warmup2[chains]
  fit@sim$permutation <- fit@sim$permutation[chains]
  fit@stan_args <- fit@stan_args[chains]
  fit
}

# failed checks of is_converged()
.conv_failed <- function(result) {
  d <- attr(result, "diagnostics")
  d$Diagnostic[!d$Passed]
}

# checks other than E-BFMI for which rstan gives a warning after sampling.
# rstan gives no E-BFMI warning for the downloaded fits, because
# `rstan:::is_sfinstance_valid()` is FALSE for them, and the "pairs()" warning
# belongs to no check.
.conv_rstan_failed <- function(fit) {
  msgs <- character()
  withCallingHandlers(
    rstan:::throw_sampler_warnings(fit),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  patterns <- c(
    Divergences = "divergent transitions after warmup",
    Treedepth = "exceeded the maximum treedepth",
    Rhat = "^The largest R-hat",
    ESS_bulk = "^Bulk Effective Samples Size",
    ESS_tail = "^Tail Effective Samples Size"
  )
  names(patterns)[vapply(
    patterns,
    function(p) any(grepl(p, msgs)),
    logical(1)
  )]
}

# compare the failed checks other than E-BFMI with the rstan warnings
.conv_expect_rstan <- function(fit) {
  result <- is_converged(fit, verbose = FALSE)
  expect_identical(
    setdiff(.conv_failed(result), "E-BFMI"),
    .conv_rstan_failed(fit)
  )
  result
}


# tests ------------------------------------------------------------------------

test_that("is_converged, stanfit without problems", {
  for (fit in list(conv_fit_brms, conv_fit_stanreg)) {
    result <- .conv_expect_rstan(fit)
    expect_true(result)
    expect_length(.conv_failed(result), 0)
  }
})

test_that("is_converged, diagnostics attribute", {
  result <- is_converged(conv_fit_brms)
  d <- attr(result, "diagnostics")
  draws <- as.array(conv_fit_brms)
  expect_named(d, c("Diagnostic", "Value", "Threshold", "Passed"))
  expect_identical(
    d$Diagnostic,
    c("Divergences", "Treedepth", "E-BFMI", "Rhat", "ESS_bulk", "ESS_tail")
  )
  expect_identical(d$Threshold, c(0, 0, 0.2, 1.05, 400, 400))
  expect_identical(d$Value[3], min(rstan::get_bfmi(conv_fit_brms)))
  expect_identical(d$Value[4], max(apply(draws, 3, rstan::Rhat)))
  expect_identical(d$Value[5], min(apply(draws, 3, rstan::ess_bulk)))
  expect_identical(d$Value[6], min(apply(draws, 3, rstan::ess_tail)))
})

test_that("is_converged, divergent transitions", {
  # a divergent transition in the warmup does not count
  fit <- conv_fit_brms
  sp <- attr(fit@sim$samples[[1]], "sampler_params")
  sp$divergent__[500] <- 1
  attr(fit@sim$samples[[1]], "sampler_params") <- sp
  expect_true(.conv_expect_rstan(fit))

  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 500, 1)
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "Divergences")
  expect_identical(attr(result, "diagnostics")$Value[1], 1)
})

test_that("is_converged, maximum treedepth", {
  # default maximum of 10
  fit <- .conv_set_sampler(conv_fit_brms, 2, "treedepth__", 1:3, 10)
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "Treedepth")
  expect_identical(attr(result, "diagnostics")$Value[2], 3)

  # a treedepth of 10 is below the maximum of 15 in conv_fit_stanreg
  fit <- .conv_set_sampler(conv_fit_stanreg, 2, "treedepth__", 1:3, 10)
  expect_true(.conv_expect_rstan(fit))

  fit <- .conv_set_sampler(conv_fit_stanreg, 2, "treedepth__", 1:3, 15)
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "Treedepth")
})

test_that("is_converged, low E-BFMI", {
  # energy as a random walk in chain 3
  set.seed(123)
  fit <- .conv_set_sampler(
    conv_fit_brms,
    3,
    "energy__",
    1:1000,
    165 + cumsum(stats::rnorm(1000))
  )
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "E-BFMI")
  expect_identical(rstan::get_low_bfmi_chains(fit), 3L)
  expect_identical(
    attr(result, "diagnostics")$Value[3],
    min(rstan::get_bfmi(fit))
  )
})

test_that("is_converged, high R-hat", {
  # shift the draws of one chain by about 10 posterior SDs
  draws <- conv_fit_brms@sim$samples[[1]][["b_wt"]]
  rows <- .conv_post_warmup(conv_fit_brms, 1)
  fit <- .conv_set_draws(
    conv_fit_brms,
    1,
    "b_wt",
    draws[rows] + 10 * stats::sd(draws[rows])
  )
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_true("Rhat" %in% .conv_failed(result))
  expect_identical(
    attr(result, "diagnostics")$Value[4],
    max(apply(as.array(fit), 3, rstan::Rhat))
  )
})

test_that("is_converged, low ESS with four chains", {
  fit <- conv_fit_brms
  for (chain in 1:4) {
    fit <- .conv_repeat_draws(fit, chain, "sigma", 20)
  }
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_true(all(c("ESS_bulk", "ESS_tail") %in% .conv_failed(result)))
})

test_that("is_converged, ESS threshold depends on the number of chains", {
  conv_two_chains <- .conv_keep_chains(conv_fit_brms, 1:2)

  # ESS between 200 and 400: passes for two chains (threshold 200), would fail
  # for four (threshold 400)
  fit <- conv_two_chains
  for (chain in 1:2) {
    fit <- .conv_repeat_draws(fit, chain, "sigma", 4)
  }
  result <- .conv_expect_rstan(fit)
  expect_true(result)
  d <- attr(result, "diagnostics")
  expect_identical(d$Threshold[5:6], c(200, 200))
  expect_true(all(d$Value[5:6] > 200 & d$Value[5:6] < 400))

  # bulk ESS below 200, tail ESS above 200
  fit <- conv_two_chains
  for (chain in 1:2) {
    fit <- .conv_repeat_draws(fit, chain, "b_wt", 6)
  }
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "ESS_bulk")
  expect_identical(
    attr(result, "diagnostics")$Value[5],
    min(apply(as.array(fit), 3, rstan::ess_bulk))
  )

  # tail ESS below 200, bulk ESS above 200
  fit <- conv_two_chains
  for (chain in 1:2) {
    fit <- .conv_repeat_draws(fit, chain, "b_Intercept", 7)
  }
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "ESS_tail")
  expect_identical(
    attr(result, "diagnostics")$Value[6],
    min(apply(as.array(fit), 3, rstan::ess_tail))
  )
})

test_that("is_converged, constant parameters", {
  # R-hat and ESS are NA for constant parameters, and the checks pass
  fit <- conv_fit_brms
  for (chain in 1:4) {
    for (parameter in names(fit@sim$samples[[chain]])) {
      fit <- .conv_set_draws(fit, chain, parameter, 1)
    }
  }
  expect_silent(is_converged(fit, verbose = FALSE))
  result <- is_converged(fit, verbose = FALSE)
  expect_true(result)
  expect_identical(.conv_rstan_failed(fit), character())
  expect_true(all(is.na(attr(result, "diagnostics")$Value[4:6])))
})

test_that("is_converged, brmsfit and stanreg use their stanfit", {
  expect_identical(is_converged(conv_brms), is_converged(conv_fit_brms))
  expect_identical(is_converged(conv_stanreg), is_converged(conv_fit_stanreg))
})

test_that("is_converged, alert for failed checks", {
  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 1:2, 1)
  fit <- .conv_set_sampler(fit, 2, "treedepth__", 1, 10)
  expect_message(
    is_converged(fit),
    "divergent transitions.*maximum treedepth"
  )
  expect_silent(is_converged(fit, verbose = FALSE))
})

test_that("is_converged, NA without MCMC draws from NUTS", {
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "Fixed_param"
    args
  })
  for (chain in seq_along(fit@sim$samples)) {
    attr(fit@sim$samples[[chain]], "sampler_params") <- NULL
  }
  expect_message(expect_identical(is_converged(fit), NA), "NUTS")
  expect_silent(expect_identical(is_converged(fit, verbose = FALSE), NA))

  # static HMC, as stored by brms with the cmdstanr backend
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "hmc"
    args$engine <- "static"
    args
  })
  expect_message(expect_identical(is_converged(fit), NA), "NUTS")

  # variational inference, as stored by rstan::vb()
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "meanfield"
    args
  })
  expect_message(expect_identical(is_converged(fit), NA), "NUTS")

  # static HMC, as stored by rstan (no engine field)
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "HMC"
    args$engine <- NULL
    args
  })
  expect_message(expect_identical(is_converged(fit), NA), "NUTS")

  model <- conv_brms
  model$algorithm <- "meanfield"
  expect_message(expect_identical(is_converged(model), NA), "NUTS")
  expect_silent(expect_identical(is_converged(model, verbose = FALSE), NA))

  model <- conv_stanreg
  model$algorithm <- "optimizing"
  expect_message(expect_identical(is_converged(model), NA), "NUTS")
  expect_silent(expect_identical(is_converged(model, verbose = FALSE), NA))
})

test_that("is_converged, NUTS as stored by brms with the cmdstanr backend", {
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "hmc"
    args$engine <- "nuts"
    args
  })
  expect_identical(is_converged(fit), is_converged(conv_fit_brms))

  fit <- .conv_set_sampler(fit, 1, "divergent__", 1, 1)
  result <- .conv_expect_rstan(fit)
  expect_false(result)
  expect_identical(.conv_failed(result), "Divergences")
})
