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


# tests ------------------------------------------------------------------------

test_that("is_converged, diagnostics attribute", {
  result <- is_converged(conv_fit_brms)
  expect_true(result)
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
  expect_true(is_converged(fit, verbose = FALSE))

  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 500, 1)
  result <- is_converged(fit, verbose = FALSE)
  expect_false(result)
  expect_identical(.conv_failed(result), "Divergences")
  expect_identical(attr(result, "diagnostics")$Value[1], 1)
})

test_that("is_converged, maximum treedepth", {
  fit <- .conv_set_sampler(conv_fit_brms, 2, "treedepth__", 1:3, 10)
  result <- is_converged(fit, verbose = FALSE)
  expect_false(result)
  expect_identical(.conv_failed(result), "Treedepth")

  # a treedepth of 10 is below the maximum of 15 in conv_fit_stanreg
  fit <- .conv_set_sampler(conv_fit_stanreg, 2, "treedepth__", 1:3, 10)
  expect_true(is_converged(fit, verbose = FALSE))
})

test_that("is_converged, maximum treedepth stored outside `control`", {
  # sets every post-warmup treedepth to 1, then the treedepth of three
  # transitions in chain 2 to `depth`, and returns the Treedepth value
  .treedepth_hits <- function(depth, control = NULL, max_depth = NULL,
                              max_treedepth = NULL) {
    fit <- conv_fit_brms
    fit@stan_args <- lapply(fit@stan_args, function(args) {
      args$control$max_treedepth <- control
      args$max_depth <- max_depth
      args$max_treedepth <- max_treedepth
      args
    })
    for (chain in seq_along(fit@sim$samples)) {
      rows <- seq_along(.conv_post_warmup(fit, chain))
      fit <- .conv_set_sampler(fit, chain, "treedepth__", rows, 1)
    }
    fit <- .conv_set_sampler(fit, 2, "treedepth__", 1:3, depth)
    attr(is_converged(fit, verbose = FALSE), "diagnostics")$Value[2]
  }

  expect_identical(.treedepth_hits(12, max_depth = 15), 0)
  expect_identical(.treedepth_hits(15, max_depth = 15), 3)
  expect_identical(.treedepth_hits(12, max_treedepth = 15), 0)
  expect_identical(.treedepth_hits(5, max_depth = 5), 3)

  # `control` comes first, then `max_depth`, then `max_treedepth`
  expect_identical(.treedepth_hits(7, control = 15, max_depth = 5), 0)
  expect_identical(.treedepth_hits(7, max_depth = 5, max_treedepth = 15), 3)

  expect_identical(.treedepth_hits(10), 3)

  # a stored limit that is not one finite number is skipped
  expect_identical(.treedepth_hits(10, max_depth = NA), 3)
  expect_identical(.treedepth_hits(10, max_depth = c(15, 15)), 3)
  expect_identical(.treedepth_hits(5, max_depth = NA, max_treedepth = 5), 3)
})

test_that("is_converged, low E-BFMI", {
  # energy as a random walk in chain 3
  withr::local_seed(123)
  fit <- .conv_set_sampler(
    conv_fit_brms,
    3,
    "energy__",
    1:1000,
    165 + cumsum(stats::rnorm(1000))
  )
  result <- is_converged(fit, verbose = FALSE)
  expect_false(result)
  expect_identical(.conv_failed(result), "E-BFMI")
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
  result <- is_converged(fit, verbose = FALSE)
  expect_false(result)
  expect_true("Rhat" %in% .conv_failed(result))
})

test_that("is_converged, ESS threshold depends on the number of chains", {
  conv_two_chains <- .conv_keep_chains(conv_fit_brms, 1:2)

  # ESS between 200 and 400: passes for two chains (threshold 200)
  fit <- conv_two_chains
  for (chain in 1:2) {
    fit <- .conv_repeat_draws(fit, chain, "sigma", 4)
  }
  result <- is_converged(fit, verbose = FALSE)
  expect_true(result)
  d <- attr(result, "diagnostics")
  expect_identical(d$Threshold[5:6], c(200, 200))
  expect_true(all(d$Value[5:6] > 200 & d$Value[5:6] < 400))

  # bulk ESS below 200, tail ESS above 200
  fit <- conv_two_chains
  for (chain in 1:2) {
    fit <- .conv_repeat_draws(fit, chain, "b_wt", 6)
  }
  expect_identical(.conv_failed(is_converged(fit, verbose = FALSE)), "ESS_bulk")
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
  expect_true(all(is.na(attr(result, "diagnostics")$Value[4:6])))
})

# all messages of a call
.conv_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msgs
}

# a copy in which all six checks fail; callers set the seed
.conv_all_failed <- function() {
  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 1, 1)
  fit <- .conv_set_sampler(fit, 2, "treedepth__", 1, 10)
  for (chain in 1:4) {
    fit <- .conv_set_sampler(
      fit,
      chain,
      "energy__",
      1:1000,
      165 + cumsum(stats::rnorm(1000))
    )
    fit <- .conv_repeat_draws(fit, chain, "sigma", 20)
  }
  draws <- fit@sim$samples[[1]][["b_wt"]]
  rows <- .conv_post_warmup(fit, 1)
  .conv_set_draws(fit, 1, "b_wt", draws[rows] + 10 * stats::sd(draws[rows]))
}

conv_checks <- c("Divergences", "Treedepth", "E-BFMI", "Rhat", "ESS_bulk", "ESS_tail")

test_that("is_converged, brmsfit and stanreg use their stanfit", {
  expect_identical(is_converged(conv_brms), is_converged(conv_fit_brms))
  expect_identical(is_converged(conv_stanreg), is_converged(conv_fit_stanreg))

  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 1, 1)
  model <- conv_brms
  model$fit <- fit
  expect_identical(
    is_converged(model, verbose = FALSE),
    is_converged(fit, verbose = FALSE)
  )
  expect_silent(is_converged(model, verbose = FALSE))
})

test_that("is_converged, alert for failed checks", {
  withr::local_seed(123)
  # all six checks fail: one message that names each check
  fit <- .conv_all_failed()
  expect_identical(.conv_failed(is_converged(fit, verbose = FALSE)), conv_checks)
  msgs <- .conv_messages(is_converged(fit))
  expect_length(msgs, 1)
  for (check in conv_checks) {
    expect_true(grepl(check, msgs, fixed = TRUE), info = check)
  }
  expect_silent(is_converged(fit, verbose = FALSE))

  # two checks fail: the passed checks are not named
  fit <- .conv_set_sampler(conv_fit_brms, 1, "divergent__", 1:2, 1)
  fit <- .conv_set_sampler(fit, 2, "treedepth__", 1, 10)
  msgs <- .conv_messages(is_converged(fit))
  expect_length(msgs, 1)
  expected <- c(
    "Divergences: divergent transitions after warmup (2, threshold 0)",
    "Treedepth: transitions at the maximum treedepth (1, threshold 0)"
  )
  for (text in expected) {
    expect_true(grepl(text, msgs, fixed = TRUE), info = text)
  }
  for (check in setdiff(conv_checks, c("Divergences", "Treedepth"))) {
    expect_false(grepl(check, msgs, fixed = TRUE), info = check)
  }

  expect_silent(is_converged(conv_fit_brms))
  expect_silent(is_converged(conv_brms))
})

test_that("is_converged, FALSE if convergence cannot be assessed", {
  .conv_expect_not_assessed <- function(x, reason) {
    expect_message(result <- is_converged(x), reason, fixed = TRUE)
    expect_identical(result, FALSE)
    expect_silent(is_converged(x, verbose = FALSE))
  }

  # static HMC, as stored by brms with the cmdstanr backend
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "hmc"
    args$engine <- "static"
    args
  })
  .conv_expect_not_assessed(fit, "NUTS")

  # no draws after warmup (rstan sets mode 2 for fits without samples)
  fit <- conv_fit_brms
  fit@mode <- 2L
  .conv_expect_not_assessed(fit, "warmup")

  model <- conv_brms
  model$algorithm <- "meanfield"
  .conv_expect_not_assessed(model, "NUTS")

  model <- conv_stanreg
  model$algorithm <- "optimizing"
  .conv_expect_not_assessed(model, "NUTS")

  model <- conv_brms
  class(model) <- c("brmsfit_multiple", class(model))
  .conv_expect_not_assessed(model, "brm_multiple")

  # a missing `$algorithm` counts as sampling
  model <- conv_brms
  model$algorithm <- NULL
  expect_identical(is_converged(model), is_converged(conv_fit_brms))
})

test_that("is_converged, NUTS as stored by brms with the cmdstanr backend", {
  fit <- conv_fit_brms
  fit@stan_args <- lapply(fit@stan_args, function(args) {
    args$algorithm <- "hmc"
    args$engine <- "nuts"
    args
  })
  expect_identical(is_converged(fit), is_converged(conv_fit_brms))
})
