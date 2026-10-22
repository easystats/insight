skip_if_not_installed("survival")

coxph <- survival::coxph
coxph.control <- survival::coxph.control

# `tmp` is 1 for a single censored case, so its coefficient diverges
cvx_lung <- survival::lung
cvx_lung$tmp <- factor(c(rep(0, 227), 1), levels = c(0, 1))
cvx_lung$start0 <- 0
cvx_lung$age2 <- cvx_lung$age * 2

cvx_mgus1 <- survival::mgus1
cvx_mgus1$fstatus <- factor(cvx_mgus1$event)

cvx_mgus2 <- survival::mgus2
cvx_mgus2$etime <- with(cvx_mgus2, ifelse(pstat == 0, futime, ptime))
cvx_mgus2$event <- with(
  cvx_mgus2,
  factor(ifelse(pstat == 0, 2 * death, 1), 0:2, c("censor", "pcm", "death"))
)

# some subjects start in state "pcm"
cvx_mgus2_istate <- cvx_mgus2
cvx_in_pcm <- withr::with_seed(1, stats::runif(nrow(cvx_mgus2)) < 0.3)
cvx_mgus2_istate$st <- factor(
  ifelse(cvx_in_pcm, "pcm", "censor"),
  c("censor", "pcm", "death")
)
cvx_mgus2_istate$etime <- with(
  cvx_mgus2_istate,
  ifelse(!cvx_in_pcm & pstat == 1, ptime, futime)
)
cvx_mgus2_istate$event <- with(
  cvx_mgus2_istate,
  factor(
    ifelse(!cvx_in_pcm & pstat == 1, 1, 2 * death),
    0:2,
    c("censor", "pcm", "death")
  )
)

# fit a model and keep the warnings of survival
.cvx_fit <- function(expr) {
  caught <- character(0)
  fit <- withCallingHandlers(
    expr,
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = caught)
}

# names of the coefficients that the survival warning names by position
.cvx_warned_terms <- function(f) {
  w <- grep("Loglik converged before variable", f$warnings, value = TRUE, fixed = TRUE)
  if (!length(w)) {
    return(character(0))
  }
  idx <- as.integer(strsplit(
    sub(".*variable\\s+([0-9,]+)\\s*;.*", "\\1", w),
    ",",
    fixed = TRUE
  )[[1]])
  names(stats::coef(f$fit))[idx]
}

.cvx_flagged <- function(result) {
  d <- attr(result, "diagnostics")
  d$Parameter[d$Diagnostic == "Infinite coefficient" & !d$Passed]
}

# check (a) fails exactly when survival warns that it ran out of iterations,
# and if check (a) passes, check (b) flags the coefficients that survival names
.cvx_expect_as_survival <- function(f) {
  result <- is_converged(f$fit, verbose = FALSE)
  d <- attr(result, "diagnostics")
  ran_out <- any(grepl("Ran out of iterations", f$warnings, fixed = TRUE))
  expect_identical(d$Diagnostic[1], "Iterations")
  expect_identical(d$Passed[1], !ran_out)
  if (ran_out) {
    expect_shape(d, nrow = 1L)
    expect_false(result)
  } else {
    warned <- .cvx_warned_terms(f)
    expect_identical(.cvx_flagged(result), warned)
    expect_identical(as.vector(result), !length(warned))
  }
  invisible(result)
}

cvx_fits <- list(
  reprex = .cvx_fit(coxph(Surv(time, status) ~ tmp, data = cvx_lung)),
  three_terms = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp + ph.ecog,
    data = cvx_lung
  )),
  clean = .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_lung)),
  cluster = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    cluster = inst
  )),
  offset = .cvx_fit(coxph(Surv(time, status) ~ tmp + offset(age / 100), data = cvx_lung)),
  na_exclude = .cvx_fit(coxph(
    Surv(time, status) ~ ph.karno + tmp,
    data = cvx_lung,
    na.action = na.exclude
  )),
  counting = .cvx_fit(coxph(Surv(start0, time, status) ~ age + tmp, data = cvx_lung)),
  counting_iter2 = .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + sex,
    data = cvx_lung,
    iter.max = 2
  )),
  ms_counting = .cvx_fit(coxph(
    Surv(start, stop, fstatus) ~ age + sex,
    data = cvx_mgus1,
    id = id
  )),
  ms_right = .cvx_fit(coxph(Surv(etime, event) ~ age + sex, data = cvx_mgus2, id = id)),
  iter_arg = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    iter.max = 2
  )),
  iter_control = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = coxph.control(iter.max = 2)
  )),
  iter_both = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    iter.max = 2,
    control = coxph.control(iter.max = 20)
  )),
  aliased = .cvx_fit(coxph(Surv(time, status) ~ age + age2, data = cvx_lung))
)


test_that("is_converged.coxph, both checks give the results of survival", {
  for (f in cvx_fits) {
    .cvx_expect_as_survival(f)
  }
})


test_that("is_converged.coxph, null model", {
  result <- is_converged(coxph(Surv(time, status) ~ 1, data = cvx_lung))
  expect_true(result)
  diagnostics <- attr(result, "diagnostics")
  expect_shape(diagnostics, nrow = 0L)
  expect_named(diagnostics, c("Diagnostic", "Parameter", "Value", "Threshold", "Passed"))
})


test_that("is_converged.coxph, diagnostics", {
  result <- is_converged(cvx_fits$three_terms$fit, verbose = FALSE)
  diagnostics <- attr(result, "diagnostics")
  expect_named(diagnostics, c("Diagnostic", "Parameter", "Value", "Threshold", "Passed"))
  expect_identical(
    diagnostics$Diagnostic,
    c("Iterations", rep("Infinite coefficient", 3))
  )
  expect_identical(diagnostics$Parameter, c(NA, "age", "tmp1", "ph.ecog"))
  expect_identical(diagnostics$Value[1], as.numeric(cvx_fits$three_terms$fit$iter))
  expect_identical(diagnostics$Threshold[1], 20)
  expect_identical(diagnostics$Passed, c(TRUE, TRUE, FALSE, TRUE))

  # the bounds of coxph.fit() and agreg.fit() differ
  toler_inf <- survival::coxph.control()$toler.inf
  expect_equal(
    diagnostics$Threshold[-1],
    pmax(1e-9, toler_inf * abs(unname(stats::coef(cvx_fits$three_terms$fit)))),
    tolerance = 1e-10
  )
  diagnostics <- attr(is_converged(cvx_fits$counting$fit, verbose = FALSE), "diagnostics")
  expect_equal(
    diagnostics$Threshold[-1],
    toler_inf * (1 + abs(unname(stats::coef(cvx_fits$counting$fit)))),
    tolerance = 1e-10
  )
})


.cvx_messages <- function(expr) {
  messages <- character(0)
  withCallingHandlers(
    expr,
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  messages
}

.cvx_expect_not_assessed <- function(fit, pattern) {
  expect_false(is_converged(fit, verbose = FALSE))
  # the alert wraps long lines
  messages <- gsub("\\s+", " ", .cvx_messages(is_converged(fit)))
  expect_length(messages, 1)
  expect_match(messages, "Convergence cannot be assessed", fixed = TRUE)
  expect_match(messages, pattern, fixed = TRUE)
}


test_that("is_converged.coxph, messages", {
  messages <- .cvx_messages(is_converged(cvx_fits$reprex$fit))
  expect_length(messages, 1)
  expect_match(messages, "Infinite coefficient", fixed = TRUE)
  expect_match(messages, "tmp1", fixed = TRUE)

  messages <- .cvx_messages(is_converged(cvx_fits$iter_arg$fit))
  expect_length(messages, 1)
  expect_match(messages, "Iterations", fixed = TRUE)

  expect_silent(is_converged(cvx_fits$clean$fit))
  expect_silent(is_converged(cvx_fits$reprex$fit, verbose = FALSE))
})


test_that("is_converged.coxph, tolerance is not used", {
  expect_identical(
    is_converged(cvx_fits$reprex$fit, tolerance = 0.001, verbose = FALSE),
    is_converged(cvx_fits$reprex$fit, tolerance = 1, verbose = FALSE)
  )
})


test_that("is_converged.coxph, convergence cannot be assessed", {
  f <- coxph(Surv(time, status) ~ ridge(age, ph.ecog, theta = 1), data = cvx_lung)
  .cvx_expect_not_assessed(f, "penalized")

  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_lung, ties = "exact"))
  .cvx_expect_not_assessed(f$fit, "Efron and Breslow")

  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_lung, iter.max = 1))
  .cvx_expect_not_assessed(f$fit, "`iter.max`")

  cvx_control <- coxph.control(iter.max = 30)
  f <- .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = cvx_control
  ))
  rm(cvx_control)
  .cvx_expect_not_assessed(f$fit, "control arguments")

  f <- .cvx_fit(coxph(
    Surv(time, status) ~ tmp + tt(age),
    data = cvx_lung,
    tt = function(x, t, ...) x * log(t + 20)
  ))
  .cvx_expect_not_assessed(f$fit, "`tt()` terms")

  no_call <- cvx_fits$clean$fit
  no_call$call <- NULL
  .cvx_expect_not_assessed(no_call, "has no call")
  no_iter <- cvx_fits$clean$fit
  no_iter$iter <- NULL
  .cvx_expect_not_assessed(no_iter, "no iterations or coefficients")
  no_flag <- cvx_fits$counting$fit
  no_flag$info <- no_flag$info[c("rank", "rescale")]
  .cvx_expect_not_assessed(no_flag, "no convergence flag")

  # right-censored and multi-state models fitted with `y = FALSE`
  f <- coxph(Surv(time, status) ~ age + sex, data = cvx_lung, y = FALSE)
  .cvx_expect_not_assessed(f, "`y = TRUE`")
  f <- coxph(Surv(start, stop, fstatus) ~ age + sex, data = cvx_mgus1, id = id, y = FALSE)
  .cvx_expect_not_assessed(f, "`y = TRUE`")

  # the score residuals of right-censored models need the data
  cvx_gone <- cvx_lung
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_gone))
  f_model <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_gone, model = TRUE))
  f_counting <- .cvx_fit(coxph(Surv(start0, time, status) ~ age + tmp, data = cvx_gone))
  rm(cvx_gone)
  .cvx_expect_not_assessed(f$fit, "model = TRUE")
  expect_identical(.cvx_flagged(is_converged(f_model$fit, verbose = FALSE)), "tmp1")
  expect_identical(
    is_converged(f_counting$fit, verbose = FALSE),
    is_converged(cvx_fits$counting$fit, verbose = FALSE)
  )
})


test_that("is_converged.coxph, counting-process models fitted with `y = FALSE`", {
  f <- .cvx_fit(coxph(Surv(start0, time, status) ~ age + tmp, data = cvx_lung, y = FALSE))
  expect_identical(
    is_converged(f$fit, verbose = FALSE),
    is_converged(cvx_fits$counting$fit, verbose = FALSE)
  )
})


test_that("is_converged.coxph, clogit models", {
  cvx_infert <- datasets::infert
  # all cases of the first 20 strata have `sep = 1`, so its coefficient diverges
  cvx_infert$sep <- as.numeric(cvx_infert$case == 1 & cvx_infert$stratum <= 20)
  f_sep <- .cvx_fit(survival::clogit(
    case ~ spontaneous + sep + survival::strata(stratum),
    data = cvx_infert,
    method = "breslow"
  ))
  .cvx_expect_as_survival(f_sep)
  expect_identical(.cvx_flagged(is_converged(f_sep$fit, verbose = FALSE)), "sep")

  # clogit() replaces the class "coxph.null" of a null model
  f_null <- survival::clogit(
    case ~ survival::strata(stratum),
    data = cvx_infert,
    method = "efron"
  )
  expect_silent(expect_true(is_converged(f_null)))

  # a penalized clogit() model has class c("clogit", "coxph")
  f_penal <- survival::clogit(
    case ~ spontaneous + survival::ridge(induced, theta = 1) + survival::strata(stratum),
    data = cvx_infert,
    method = "efron"
  )
  .cvx_expect_not_assessed(f_penal, "penalized")
})


test_that("is_converged, classes that inherit from coxph without a method", {
  cph_like <- cvx_fits$reprex$fit
  class(cph_like) <- c("cph", "rms", "coxph")
  expect_message(
    expect_null(is_converged(cph_like)),
    "does not work for models of class 'cph'",
    fixed = TRUE
  )
  expect_silent(expect_null(is_converged(cph_like, verbose = FALSE)))
})


test_that("is_converged.coxph, models without events", {
  cvx_no_events <- cvx_lung
  cvx_no_events$status <- 0
  f <- coxph(Surv(time, status) ~ age + sex, data = cvx_no_events)
  .cvx_expect_not_assessed(f, "no events")
  # without events, a null model has class "coxph", not "coxph.null"
  f_null <- coxph(Surv(time, status) ~ 1, data = cvx_no_events)
  expect_true(is_converged(f_null))
})


test_that("is_converged.coxph, multi-state predictor data changed after the fit", {
  cvx_ms_changed <- cvx_mgus2
  f_common <- coxph(Surv(etime, event) ~ age + sex, data = cvx_ms_changed, id = id)
  # `hgb` has missing values in rows of the other transition
  f_list <- coxph(
    list(Surv(etime, event) ~ sex, 1:3 ~ age + hgb),
    data = cvx_ms_changed,
    id = id
  )
  expect_true(is_converged(f_common))
  expect_true(is_converged(f_list))

  cvx_ms_changed$age <- rev(cvx_mgus2$age)
  .cvx_expect_not_assessed(f_common, "predictor data changed")

  # rows added to the data
  cvx_ms_changed <- rbind(cvx_mgus2, cvx_mgus2[1:5, ])
  cvx_ms_changed$id[nrow(cvx_mgus2) + 1:5] <- max(cvx_mgus2$id) + 1:5
  .cvx_expect_not_assessed(f_common, "predictor data changed")

  # if transitions share a baseline hazard, the data are not compared
  cvx_ms_shared <- cvx_mgus2
  f_shared <- coxph(
    list(Surv(etime, event) ~ age + sex, 1:2 + 1:3 ~ 1 / shared),
    data = cvx_ms_shared,
    id = id
  )
  expect_true(is_converged(f_shared))
  cvx_ms_shared$age <- rev(cvx_mgus2$age)
  expect_false(.coxph_data_changed(f_shared))

  # strata: `smap` has more than one row
  cvx_ms_strata <- cvx_mgus2
  f_strata <- coxph(Surv(etime, event) ~ age + strata(sex), data = cvx_ms_strata, id = id)
  expect_true(is_converged(f_strata))
  cvx_ms_strata$age <- rev(cvx_mgus2$age)
  .cvx_expect_not_assessed(f_strata, "predictor data changed")
})


test_that("is_converged.coxph, multi-state models with a transition without predictors", {
  # survival 3.8-6 and 3.8.12 number the baseline hazards of the stacked rows
  # differently if the first transition has no predictors
  cvx_free_first <- cvx_mgus2_istate
  f_first <- coxph(
    list(Surv(etime, event) ~ 1, 1:3 + 2:3 ~ age),
    data = cvx_free_first,
    id = id,
    istate = st
  )
  expect_true(is_converged(f_first))
  # the same fit with its stacked rows numbered the other way
  f_renumbered <- f_first
  baselines <- f_renumbered$smap[1, colSums(f_renumbered$cmap) > 0]
  if (all(f_renumbered$rmap[, 2] %in% seq_along(baselines))) {
    f_renumbered$rmap[, 2] <- baselines[f_renumbered$rmap[, 2]]
  } else {
    f_renumbered$rmap[, 2] <- match(f_renumbered$rmap[, 2], baselines)
  }
  expect_false(identical(f_renumbered$rmap, f_first$rmap))
  expect_true(is_converged(f_renumbered))
  cvx_free_first$age <- rev(cvx_free_first$age)
  .cvx_expect_not_assessed(f_first, "predictor data changed")

  # transition 1:3 has no stacked rows, so the row numbers fit both numberings
  cvx_free_empty <- cvx_mgus2_istate
  cvx_free_empty$z <- ifelse(
    cvx_in_pcm,
    withr::with_seed(3, stats::rnorm(nrow(cvx_mgus2_istate))),
    NA
  )
  f_empty <- coxph(
    list(Surv(etime, event) ~ 1, 1:3 ~ z, 2:3 ~ age),
    data = cvx_free_empty,
    id = id,
    istate = st
  )
  .cvx_expect_not_assessed(f_empty, "score vector could not be computed")
  cvx_free_empty$age <- rev(cvx_free_empty$age)
  .cvx_expect_not_assessed(f_empty, "predictor data changed")
})


test_that("is_converged.coxph, the data hint only if the data cannot be read", {
  local_mocked_bindings(
    .coxph_infinite = function(...) stop("object 'rr' not found", call. = FALSE)
  )
  messages <- gsub("\\s+", " ", .cvx_messages(is_converged(cvx_fits$clean$fit)))
  expect_length(messages, 1)
  expect_match(messages, "object 'rr' not found", fixed = TRUE)
  expect_no_match(messages, "no longer available", fixed = TRUE)
})


test_that("is_converged.coxph, predictor data changed after the fit", {
  cvx_changed <- cvx_lung
  f <- coxph(Surv(time, status) ~ age + sex, data = cvx_changed)
  f_model <- coxph(Surv(time, status) ~ age + sex, data = cvx_changed, model = TRUE)
  expect_true(is_converged(f))

  cvx_changed$age <- withr::with_seed(123, sample(cvx_changed$age))
  .cvx_expect_not_assessed(f, "predictor data changed")
  expect_true(is_converged(f_model))

  cvx_changed <- cvx_lung[-1, ]
  .cvx_expect_not_assessed(f, "predictor data changed")

  # a model with a factor stores `xlevels`, which `fit$x` would match
  cvx_changed_factor <- cvx_lung
  f_factor <- coxph(Surv(time, status) ~ age + factor(sex), data = cvx_changed_factor)
  expect_true(is_converged(f_factor))
  cvx_changed_factor$age <- withr::with_seed(123, sample(cvx_changed_factor$age))
  .cvx_expect_not_assessed(f_factor, "predictor data changed")
})


test_that("is_converged.coxph, variable control arguments with a formula object", {
  cvx_env_fo <- Surv(time, status) ~ ph.ecog + age
  n <- 50
  fit_arg <- function(n) coxph(cvx_env_fo, data = cvx_lung, iter.max = n)
  f_arg <- .cvx_fit(fit_arg(2))
  .cvx_expect_not_assessed(f_arg$fit, "control arguments of the model call (`iter.max`)")

  # a literal formula, or a formula object with constant control arguments
  fit_literal <- function(n) {
    coxph(Surv(time, status) ~ ph.ecog + age, data = cvx_lung, iter.max = n)
  }
  fit_constant <- function() coxph(cvx_env_fo, data = cvx_lung, iter.max = 2)
  for (f in list(.cvx_fit(fit_literal(2)), .cvx_fit(fit_constant()))) {
    .cvx_expect_as_survival(f)
    expect_identical(attr(is_converged(f$fit, verbose = FALSE), "diagnostics")$Threshold, 2)
  }

  # clogit() stores a coxph() call with the formula as an object
  cvx_env_infert <- datasets::infert
  fit_clogit <- function(n) {
    survival::clogit(
      case ~ spontaneous + induced + survival::strata(stratum),
      data = cvx_env_infert,
      method = "efron",
      iter.max = n
    )
  }
  f_clogit <- .cvx_fit(fit_clogit(2))
  .cvx_expect_as_survival(f_clogit)
  expect_identical(
    attr(is_converged(f_clogit$fit, verbose = FALSE), "diagnostics")$Threshold,
    2
  )
})


test_that("is_converged.coxph, strata changed after the fit", {
  cvx_strata <- cvx_lung
  f <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = cvx_strata)
  f_x <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = cvx_strata, x = TRUE)
  expect_true(is_converged(f))

  cvx_strata$sex <- withr::with_seed(123, sample(cvx_strata$sex))
  .cvx_expect_not_assessed(f, "predictor data changed")
  expect_true(is_converged(f_x))
})


test_that("is_converged.coxph, the strata check reads only the data arguments of the call", {
  cvx_args <- cvx_lung
  cvx_args_fo <- Surv(time, status) ~ age + ph.ecog + strata(sex)
  tie <- "efron"
  fit_ties <- function(tie) coxph(cvx_args_fo, data = cvx_args, ties = tie)
  expect_true(is_converged(fit_ties("breslow")))

  cvx_args$w <- rep(1:2, 114)
  cvx_args$off <- cvx_args$age / 100
  f_weights <- coxph(Surv(time, status) ~ age + strata(sex), data = cvx_args, weights = w)
  f_offset <- coxph(Surv(time, status) ~ ph.ecog + offset(off) + strata(sex), data = cvx_args)
  f_interaction <- coxph(Surv(time, status) ~ age + age:strata(sex), data = cvx_args)
  expect_true(is_converged(f_weights))
  expect_true(is_converged(f_offset))
  expect_true(is_converged(f_interaction))

  # the response and the weights are taken from the fit
  cvx_args$time <- withr::with_seed(123, sample(cvx_lung$time))
  cvx_args$w <- rev(cvx_args$w)
  expect_true(is_converged(f_weights))

  broken <- f_weights
  broken$weights <- c(1, 2)
  .cvx_expect_not_assessed(broken, "strata of the model could not be compared")
})


test_that("is_converged.coxph, edge cases of the strata check", {
  cvx_edge <- cvx_lung
  f <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = cvx_edge)
  twin <- coxph(
    Surv(time, status) ~ age + ph.ecog + strata(sex),
    data = cvx_edge,
    model = TRUE
  )
  # a model object without the stored residuals or log-likelihood is compared
  # on what it has
  for (removed in list("residuals", "loglik", c("residuals", "loglik"))) {
    partial <- f
    partial[removed] <- NULL
    partial_twin <- twin
    partial_twin[removed] <- NULL
    expect_identical(
      is_converged(partial, verbose = FALSE),
      is_converged(partial_twin, verbose = FALSE)
    )
  }
  # the strata from the model frame, if the model matrix has none
  mf <- stats::model.frame(f)
  mm <- stats::model.matrix(f)
  attr(mm, "strata") <- NULL
  expect_false(.coxph_strata_changed(f, mf, mm, stats::coef(f), NULL))
  cvx_edge$sex[5] <- NA
  .cvx_expect_not_assessed(f, "predictor data changed")
})


test_that("is_converged.coxph, a strata change that the log-likelihood hides", {
  cvx_pbc <- survival::pbc
  f <- coxph(Surv(time, status == 2) ~ age + log(bili) + strata(edema), data = cvx_pbc)
  expect_true(is_converged(f))
  # rows 13 and 238 swap their strata (edema 0 and 0.5)
  cvx_pbc$edema[c(13, 238)] <- cvx_pbc$edema[c(238, 13)]
  .cvx_expect_not_assessed(f, "predictor data changed")
})


test_that("is_converged.coxph, model objects with broken elements", {
  fit <- cvx_fits$clean$fit
  na_iter <- fit
  na_iter$iter <- NA
  .cvx_expect_not_assessed(na_iter, "no valid number of iterations")
  zero_iter <- fit
  zero_iter$iter <- 0L
  .cvx_expect_not_assessed(zero_iter, "no valid number of iterations")
  no_coef <- fit
  no_coef$coefficients <- numeric(0)
  .cvx_expect_not_assessed(no_coef, "no iterations or coefficients")
  na_var <- fit
  na_var$var[] <- NA
  .cvx_expect_not_assessed(na_var, "missing values")
  no_smap <- cvx_fits$ms_right$fit
  no_smap$smap <- NULL
  .cvx_expect_not_assessed(no_smap, "score vector could not be computed")
})
