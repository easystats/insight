skip_if_not_installed("survival")

# survival is not attached. coxph() adds Surv() and the specials that the
# formula uses to the formula environment, so only the functions that the
# tests call by name are needed here.
coxph <- survival::coxph
coxph.control <- survival::coxph.control

# `tmp` is 1 for a single censored case, so its coefficient diverges
cvx_lung <- survival::lung
cvx_lung$tmp <- factor(c(rep(0, 227), 1), levels = c(0, 1))
cvx_lung$start0 <- 0
cvx_lung$age2 <- cvx_lung$age * 2

# multi-state data with start-stop times (counting process)
cvx_mgus1 <- survival::mgus1
cvx_mgus1$fstatus <- factor(cvx_mgus1$event)

# multi-state data with right-censored times, as in the survival vignette
cvx_mgus2 <- survival::mgus2
cvx_mgus2$etime <- with(cvx_mgus2, ifelse(pstat == 0, futime, ptime))
cvx_mgus2$event <- with(
  cvx_mgus2,
  factor(ifelse(pstat == 0, 2 * death, 1), 0:2, c("censor", "pcm", "death"))
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

# names of the coefficients that the survival warning names by position. The
# warning reads, for example, "Loglik converged before variable  2,3 ;
# coefficient may be infinite. ", and the regex keeps "2,3".
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
  weights_strata = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp + strata(sex),
    data = cvx_lung,
    weights = rep(1:2, 114)
  )),
  weights_fractional = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    weights = rep(c(0.5, 1.5), 114)
  )),
  cluster = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    cluster = inst
  )),
  offset = .cvx_fit(coxph(Surv(time, status) ~ tmp + offset(age / 100), data = cvx_lung)),
  na_omit = .cvx_fit(coxph(Surv(time, status) ~ ph.karno + tmp, data = cvx_lung)),
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
  counting_iter3 = .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + sex,
    data = cvx_lung,
    iter.max = 3
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
  iter_partial = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    iter = 2
  )),
  iter_control = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = coxph.control(iter.max = 2)
  )),
  iter_list = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = list(iter.max = 2)
  )),
  iter_both = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    iter.max = 2,
    control = coxph.control(iter.max = 20)
  )),
  toler_inf = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = coxph.control(toler.inf = 2)
  )),
  eps = .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_lung,
    control = coxph.control(eps = 1e-4)
  )),
  aliased = .cvx_fit(coxph(Surv(time, status) ~ age + age2, data = cvx_lung)),
  null = .cvx_fit(coxph(Surv(time, status) ~ 1, data = cvx_lung))
)


test_that("is_converged.coxph, both checks give the results of survival", {
  for (f in cvx_fits[names(cvx_fits) != "null"]) {
    .cvx_expect_as_survival(f)
  }
})


test_that("is_converged.coxph, the probes cover the cases they name", {
  # stated independently of the survival warnings
  expect_identical(
    .cvx_flagged(is_converged(cvx_fits$reprex$fit, verbose = FALSE)),
    "tmp1"
  )
  expect_identical(
    .cvx_flagged(is_converged(cvx_fits$three_terms$fit, verbose = FALSE)),
    "tmp1"
  )
  expect_true(is_converged(cvx_fits$clean$fit))
  expect_length(cvx_fits$clean$warnings, 0)

  # fractional weights and clusters give a robust variance
  expect_false(is.null(cvx_fits$weights_fractional$fit$naive.var))
  expect_false(is.null(cvx_fits$cluster$fit$naive.var))

  # with the robust variance, the factor would not be flagged
  fit <- cvx_fits$cluster$fit
  u <- colSums(as.matrix(stats::residuals(fit, type = "score", weighted = TRUE)))
  robust_infs <- stats::setNames(abs(drop(u %*% fit$var)), names(stats::coef(fit)))
  diagnostics <- attr(is_converged(fit, verbose = FALSE), "diagnostics")
  tmp_row <- which(diagnostics$Parameter == "tmp1")
  expect_lt(robust_infs[["tmp1"]], diagnostics$Threshold[tmp_row])
  expect_false(diagnostics$Passed[tmp_row])

  # the residuals of a model with na.exclude contain missing values
  expect_true(anyNA(stats::residuals(cvx_fits$na_exclude$fit, type = "score")))

  # both fitters and both multi-state response types
  expect_identical(attr(cvx_fits$counting$fit$y, "type"), "counting")
  expect_identical(attr(cvx_fits$ms_counting$fit$y, "type"), "mcounting")
  expect_identical(attr(cvx_fits$ms_right$fit$y, "type"), "mright")
  expect_s3_class(cvx_fits$ms_right$fit, "coxphms")
  expect_identical(
    .cvx_flagged(is_converged(cvx_fits$counting$fit, verbose = FALSE)),
    "tmp1"
  )
  expect_true(any(grepl(
    "beta may be infinite",
    cvx_fits$counting$warnings,
    fixed = TRUE
  )))

  # the control values of the call
  for (i in c("iter_arg", "iter_partial", "iter_control", "iter_list")) {
    expect_false(is_converged(cvx_fits[[i]]$fit, verbose = FALSE))
    expect_identical(
      attr(is_converged(cvx_fits[[i]]$fit, verbose = FALSE), "diagnostics")$Threshold[1],
      2
    )
  }
  diagnostics <- attr(
    is_converged(cvx_fits$iter_both$fit, verbose = FALSE),
    "diagnostics"
  )
  expect_identical(diagnostics$Threshold[1], 20)
  expect_true(diagnostics$Passed[1])
  expect_length(cvx_fits$toler_inf$warnings, 0)
  expect_true(is_converged(cvx_fits$toler_inf$fit, verbose = FALSE))
  # the default `toler.inf` depends on `eps`
  diagnostics <- attr(is_converged(cvx_fits$eps$fit, verbose = FALSE), "diagnostics")
  coefs <- stats::coef(cvx_fits$eps$fit)
  expect_equal(
    diagnostics$Threshold[-1],
    pmax(1e-4, sqrt(1e-4) * abs(unname(coefs))),
    tolerance = 1e-10
  )
})


test_that("is_converged.coxph, null model", {
  result <- is_converged(cvx_fits$null$fit)
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

  # the rows that fail are the flagged coefficients
  for (i in c("reprex", "counting", "ms_counting", "ms_right")) {
    diagnostics <- attr(is_converged(cvx_fits[[i]]$fit, verbose = FALSE), "diagnostics")
    expect_identical(
      diagnostics$Parameter[!diagnostics$Passed],
      .cvx_warned_terms(cvx_fits[[i]])
    )
    expect_identical(
      diagnostics$Parameter[-1],
      names(stats::coef(cvx_fits[[i]]$fit))
    )
  }

  # the bounds of coxph.fit() and agreg.fit() differ
  toler_inf <- survival::coxph.control()$toler.inf
  for (i in c("counting", "ms_counting")) {
    diagnostics <- attr(is_converged(cvx_fits[[i]]$fit, verbose = FALSE), "diagnostics")
    expect_equal(
      diagnostics$Threshold[-1],
      toler_inf * (1 + abs(unname(stats::coef(cvx_fits[[i]]$fit)))),
      tolerance = 1e-10
    )
  }
  diagnostics <- attr(
    is_converged(cvx_fits$three_terms$fit, verbose = FALSE),
    "diagnostics"
  )
  expect_equal(
    diagnostics$Threshold[-1],
    pmax(1e-9, toler_inf * abs(unname(stats::coef(cvx_fits$three_terms$fit)))),
    tolerance = 1e-10
  )

  # if check (a) fails, check (b) does not run
  for (i in c("iter_arg", "counting_iter2")) {
    diagnostics <- attr(is_converged(cvx_fits[[i]]$fit, verbose = FALSE), "diagnostics")
    expect_identical(diagnostics$Diagnostic, "Iterations")
    expect_false(diagnostics$Passed)
  }

  # agreg.fit() can converge on the last iteration
  diagnostics <- attr(
    is_converged(cvx_fits$counting_iter3$fit, verbose = FALSE),
    "diagnostics"
  )
  expect_identical(diagnostics$Value[1], diagnostics$Threshold[1])
  expect_true(diagnostics$Passed[1])

  # an aliased coefficient is never flagged
  diagnostics <- attr(is_converged(cvx_fits$aliased$fit, verbose = FALSE), "diagnostics")
  aliased_row <- which(diagnostics$Parameter == "age2")
  expect_true(is.na(stats::coef(cvx_fits$aliased$fit)[["age2"]]))
  expect_true(is.na(diagnostics$Threshold[aliased_row]))
  expect_true(diagnostics$Passed[aliased_row])
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
  result <- is_converged(fit, verbose = FALSE)
  expect_false(result)
  expect_null(attr(result, "diagnostics"))
  expect_silent(is_converged(fit, verbose = FALSE))
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
  expect_message(is_converged(cvx_fits$reprex$fit), "tmp1")

  messages <- .cvx_messages(is_converged(cvx_fits$iter_arg$fit))
  expect_length(messages, 1)
  expect_match(messages, "Iterations", fixed = TRUE)

  expect_silent(is_converged(cvx_fits$clean$fit))
  for (f in cvx_fits) {
    expect_silent(is_converged(f$fit, verbose = FALSE))
  }
})


test_that("is_converged.coxph, tolerance is not used", {
  for (i in c("reprex", "clean")) {
    expect_identical(
      is_converged(cvx_fits[[i]]$fit, tolerance = 0.001, verbose = FALSE),
      is_converged(cvx_fits[[i]]$fit, tolerance = 1, verbose = FALSE)
    )
  }
})


test_that("is_converged.coxph, convergence cannot be assessed", {
  penalized <- list(
    .cvx_fit(coxph(Surv(time, status) ~ age + frailty(inst), data = cvx_lung)),
    .cvx_fit(coxph(Surv(time, status) ~ ridge(age, ph.ecog, theta = 1), data = cvx_lung)),
    .cvx_fit(coxph(Surv(time, status) ~ pspline(age), data = cvx_lung))
  )
  for (f in penalized) {
    expect_s3_class(f$fit, "coxph.penal")
    .cvx_expect_not_assessed(f$fit, "penalized")
  }

  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_lung, ties = "exact"))
  expect_identical(f$fit$method, "exact")
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

  # model objects without a call, iterations or convergence flag
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_lung))
  no_call <- f$fit
  no_call$call <- NULL
  .cvx_expect_not_assessed(no_call, "has no call")
  no_iter <- f$fit
  no_iter$iter <- NULL
  .cvx_expect_not_assessed(no_iter, "no iterations or coefficients")
  no_flag <- cvx_fits$counting$fit
  no_flag$info <- no_flag$info[c("rank", "rescale")]
  .cvx_expect_not_assessed(no_flag, "no convergence flag")

  # right-censored and multi-state models fitted with `y = FALSE` store no
  # response
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_lung, y = FALSE))
  expect_null(f$fit$y)
  .cvx_expect_not_assessed(f$fit, "`y = TRUE`")
  f <- .cvx_fit(coxph(
    Surv(etime, event) ~ age + sex,
    data = cvx_mgus2,
    id = id,
    y = FALSE
  ))
  expect_null(f$fit$y)
  .cvx_expect_not_assessed(f$fit, "`y = TRUE`")
  # a multi-state counting-process model stores `first` and `info`, but is
  # not assessed without its response either
  f <- .cvx_fit(coxph(
    Surv(start, stop, fstatus) ~ age + sex,
    data = cvx_mgus1,
    id = id,
    y = FALSE
  ))
  expect_false(is.null(f$fit$first))
  .cvx_expect_not_assessed(f$fit, "`y = TRUE`")

  # the score residuals of right-censored models need the data
  cvx_gone <- cvx_lung
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_gone))
  f_model <- .cvx_fit(coxph(
    Surv(time, status) ~ age + tmp,
    data = cvx_gone,
    model = TRUE
  ))
  f_counting <- .cvx_fit(coxph(Surv(start0, time, status) ~ age + tmp, data = cvx_gone))
  rm(cvx_gone)
  .cvx_expect_not_assessed(f$fit, "model = TRUE")
  f_present <- .cvx_fit(coxph(Surv(time, status) ~ age + tmp, data = cvx_lung))
  .cvx_expect_as_survival(f_model)
  expect_identical(
    is_converged(f_model$fit, verbose = FALSE),
    is_converged(f_present$fit, verbose = FALSE)
  )
  expect_identical(.cvx_flagged(is_converged(f_model$fit, verbose = FALSE)), "tmp1")
  # counting-process models store the score vector
  .cvx_expect_as_survival(f_counting)
  expect_identical(
    is_converged(f_counting$fit, verbose = FALSE),
    is_converged(cvx_fits$counting$fit, verbose = FALSE)
  )
})


test_that("is_converged.coxph, counting-process models fitted with `y = FALSE`", {
  # the checks of agreg.fit() use only the stored score vector and the
  # convergence flag
  f_pass <- .cvx_fit(coxph(Surv(start0, time, status) ~ age + sex, data = cvx_lung))
  f_pass_no_y <- .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + sex,
    data = cvx_lung,
    y = FALSE
  ))
  f_iter_no_y <- .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + sex,
    data = cvx_lung,
    iter.max = 2,
    y = FALSE
  ))
  f_infinite_no_y <- .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + tmp,
    data = cvx_lung,
    y = FALSE
  ))
  pairs <- list(
    list(f_pass, f_pass_no_y),
    list(cvx_fits$counting_iter2, f_iter_no_y),
    list(cvx_fits$counting, f_infinite_no_y)
  )
  for (p in pairs) {
    expect_null(p[[2]]$fit$y)
    expect_identical(
      is_converged(p[[2]]$fit, verbose = FALSE),
      is_converged(p[[1]]$fit, verbose = FALSE)
    )
    .cvx_expect_as_survival(p[[2]])
  }
  # stated independently of the fits with `y = TRUE`
  expect_true(is_converged(f_pass_no_y$fit))
  diagnostics <- attr(is_converged(f_iter_no_y$fit, verbose = FALSE), "diagnostics")
  expect_identical(diagnostics$Diagnostic, "Iterations")
  expect_false(diagnostics$Passed)
  expect_identical(.cvx_flagged(is_converged(f_infinite_no_y$fit, verbose = FALSE)), "tmp1")
})


test_that("is_converged.coxph, clogit models", {
  cvx_infert <- datasets::infert
  # all cases of the first 20 strata have `sep = 1`, so its coefficient diverges
  cvx_infert$sep <- as.numeric(cvx_infert$case == 1 & cvx_infert$stratum <= 20)
  f_iter <- .cvx_fit(survival::clogit(
    case ~ spontaneous + induced + survival::strata(stratum),
    data = cvx_infert,
    method = "efron",
    iter.max = 2
  ))
  f_sep <- .cvx_fit(survival::clogit(
    case ~ spontaneous + sep + survival::strata(stratum),
    data = cvx_infert,
    method = "breslow"
  ))
  f_clean <- .cvx_fit(survival::clogit(
    case ~ spontaneous + induced + survival::strata(stratum),
    data = cvx_infert,
    method = "efron"
  ))
  expect_match(f_iter$warnings, "Ran out of iterations", fixed = TRUE)
  expect_identical(.cvx_warned_terms(f_sep), "sep")
  expect_length(f_clean$warnings, 0)
  for (f in list(f_iter, f_sep, f_clean)) {
    expect_s3_class(f$fit, "clogit")
    .cvx_expect_as_survival(f)
  }
  expect_false(is_converged(f_iter$fit, verbose = FALSE))
  expect_identical(.cvx_flagged(is_converged(f_sep$fit, verbose = FALSE)), "sep")
  expect_true(is_converged(f_clean$fit))

  # the default method of clogit() is "exact"
  f_exact <- .cvx_fit(survival::clogit(
    case ~ spontaneous + induced + survival::strata(stratum),
    data = cvx_infert
  ))
  expect_identical(f_exact$fit$method, "exact")
  .cvx_expect_not_assessed(f_exact$fit, "Efron and Breslow")
})


test_that("is_converged.coxph, other classes that inherit from coxph", {
  # a class vector like that of rms::cph(); rms is not needed for the class
  cph_like <- cvx_fits$reprex$fit
  class(cph_like) <- c("cph", "rms", "coxph")
  expect_message(
    expect_null(is_converged(cph_like)),
    "does not work for models of class 'cph'",
    fixed = TRUE
  )

  skip_if_not_installed("survey")
  cvx_design <- survey::svydesign(
    id = ~1,
    probs = rep(1, nrow(cvx_lung)),
    data = cvx_lung
  )
  cvx_svy <- survey::svycoxph(Surv(time, status) ~ age + sex, design = cvx_design)
  expect_s3_class(cvx_svy, "coxph")
  expect_message(
    expect_null(is_converged(cvx_svy)),
    "does not work for models of class 'svycoxph'",
    fixed = TRUE
  )
})


test_that("is_converged.coxph, models without events", {
  cvx_no_events <- cvx_lung
  cvx_no_events$status <- 0
  # right-censored: no iterations and missing coefficients, and no warning
  f_right <- .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_no_events))
  expect_length(f_right$warnings, 0)
  expect_identical(f_right$fit$iter, 0)
  expect_true(all(is.na(stats::coef(f_right$fit))))
  .cvx_expect_not_assessed(f_right$fit, "no events")
  # counting process
  f_counting <- .cvx_fit(coxph(
    Surv(time - 1, time, status) ~ age + sex,
    data = cvx_no_events
  ))
  expect_identical(attr(f_counting$fit$y, "type"), "counting")
  .cvx_expect_not_assessed(f_counting$fit, "no events")
  # a null model has no coefficients that could diverge. Without events, it
  # has class "coxph", not "coxph.null".
  f_null <- .cvx_fit(coxph(Surv(time, status) ~ 1, data = cvx_no_events))
  expect_length(stats::coef(f_null$fit), 0)
  expect_true(is_converged(f_null$fit))
})


test_that("is_converged.coxph, multi-state predictor data changed after the fit", {
  cvx_ms_changed <- cvx_mgus2
  f_common <- .cvx_fit(coxph(
    Surv(etime, event) ~ age + sex,
    data = cvx_ms_changed,
    id = id
  ))
  # `age` and `hgb` enter only the transition to death. `hgb` has missing
  # values in rows of the other transition.
  f_list <- .cvx_fit(coxph(
    list(Surv(etime, event) ~ sex, 1:3 ~ age + hgb),
    data = cvx_ms_changed,
    id = id
  ))
  expect_true(anyNA(cvx_mgus2$hgb))
  for (f in list(f_common, f_list)) {
    expect_s3_class(f$fit, "coxphms")
    expect_true(is_converged(f$fit))
  }

  # a constant shift changes the linear predictor of a transition only by a
  # constant, and the score residuals not at all
  cvx_ms_changed$age <- cvx_mgus2$age + 10
  for (f in list(f_common, f_list)) {
    expect_true(is_converged(f$fit))
  }

  # main flagged all coefficients of `f_common` as infinite after this change
  cvx_ms_changed$age <- rev(cvx_mgus2$age)
  for (f in list(f_common, f_list)) {
    .cvx_expect_not_assessed(f$fit, "predictor data changed")
  }
})


test_that("is_converged.coxph, the data hint only if the data cannot be read", {
  # the score residuals fail although the data of the model can be read
  local_mocked_bindings(
    .coxph_infinite = function(...) stop("object 'rr' not found", call. = FALSE)
  )
  messages <- gsub("\\s+", " ", .cvx_messages(is_converged(cvx_fits$clean$fit)))
  expect_length(messages, 1)
  expect_match(messages, "object 'rr' not found", fixed = TRUE)
  expect_no_match(messages, "no longer available", fixed = TRUE)
  expect_false(is_converged(cvx_fits$clean$fit, verbose = FALSE))

  # the hint is given if the data cannot be read
  cvx_hint_gone <- cvx_lung
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_hint_gone))
  rm(cvx_hint_gone)
  .cvx_expect_not_assessed(f$fit, "no longer available")
})


test_that("is_converged.coxph, predictor data changed after the fit", {
  cvx_changed <- cvx_lung
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_changed))
  f_model <- .cvx_fit(coxph(
    Surv(time, status) ~ age + sex,
    data = cvx_changed,
    model = TRUE
  ))
  expect_true(is_converged(f$fit))

  # main flagged `age` and `sex` as infinite after this shuffle
  cvx_changed$age <- withr::with_seed(123, sample(cvx_changed$age))
  .cvx_expect_not_assessed(f$fit, "predictor data changed")
  # the model frame is stored with `model = TRUE`
  expect_true(is_converged(f_model$fit))

  # one row less
  cvx_changed <- cvx_lung[-1, ]
  .cvx_expect_not_assessed(f$fit, "predictor data changed")

  # a model with a factor stores `xlevels`, which `fit$x` would match
  cvx_changed_factor <- cvx_lung
  f_factor <- .cvx_fit(coxph(
    Surv(time, status) ~ age + factor(sex),
    data = cvx_changed_factor
  ))
  expect_false(is.null(f_factor$fit$xlevels))
  expect_true(is_converged(f_factor$fit))
  cvx_changed_factor$age <- withr::with_seed(123, sample(cvx_changed_factor$age))
  .cvx_expect_not_assessed(f_factor$fit, "predictor data changed")

  # with `x = TRUE`, the score residuals use the stored model matrix and
  # offset, so a changed offset variable does not matter
  cvx_changed_offset <- cvx_lung
  cvx_changed_offset$off <- cvx_changed_offset$age / 100
  f_x <- .cvx_fit(coxph(
    Surv(time, status) ~ sex + offset(off),
    data = cvx_changed_offset,
    x = TRUE
  ))
  score_before <- sum(stats::residuals(f_x$fit, type = "score"))
  cvx_changed_offset$off <- cvx_changed_offset$off * 3
  expect_identical(sum(stats::residuals(f_x$fit, type = "score")), score_before)
  expect_true(is_converged(f_x$fit))
})


test_that("is_converged.coxph, model objects with broken elements", {
  fit <- cvx_fits$clean$fit
  no_iter <- fit
  no_iter$iter <- integer(0)
  .cvx_expect_not_assessed(no_iter, "no valid number of iterations")
  na_iter <- fit
  na_iter$iter <- NA
  .cvx_expect_not_assessed(na_iter, "no valid number of iterations")
  # a model with predictors but no coefficients is not a null model
  no_coef <- fit
  no_coef$coefficients <- numeric(0)
  .cvx_expect_not_assessed(no_coef, "no iterations or coefficients")
  # check (b) gives missing values for coefficients that are not missing
  na_var <- fit
  na_var$var[] <- NA
  .cvx_expect_not_assessed(na_var, "missing values")
  # a fit with events has at least one iteration
  zero_iter <- fit
  zero_iter$iter <- 0L
  .cvx_expect_not_assessed(zero_iter, "no valid number of iterations")
  # a fit without events whose number of events was removed
  cvx_broken_no_events <- cvx_lung
  cvx_broken_no_events$status <- 0
  no_nevent <- .cvx_fit(coxph(
    Surv(time, status) ~ age + sex,
    data = cvx_broken_no_events
  ))$fit
  no_nevent$nevent <- NULL
  .cvx_expect_not_assessed(no_nevent, "iterations")
})


test_that("is_converged, verbose = FALSE for classes without a method", {
  cph_like <- cvx_fits$reprex$fit
  class(cph_like) <- c("cph", "rms", "coxph")
  expect_silent(expect_null(is_converged(cph_like, verbose = FALSE)))
  expect_message(
    expect_null(is_converged(cph_like)),
    "does not work for models of class 'cph'",
    fixed = TRUE
  )
})
