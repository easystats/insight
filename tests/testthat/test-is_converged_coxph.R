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
  # weights, strata and an offset
  f_more <- .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + tmp + strata(sex) + offset(ph.karno / 100),
    data = cvx_lung,
    weights = rep(1:2, 114)
  ))
  f_more_no_y <- .cvx_fit(coxph(
    Surv(start0, time, status) ~ age + tmp + strata(sex) + offset(ph.karno / 100),
    data = cvx_lung,
    weights = rep(1:2, 114),
    y = FALSE
  ))
  pairs <- list(
    list(f_pass, f_pass_no_y),
    list(cvx_fits$counting_iter2, f_iter_no_y),
    list(cvx_fits$counting, f_infinite_no_y),
    list(f_more, f_more_no_y)
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
  f_cluster <- .cvx_fit(survival::clogit(
    case ~ spontaneous + sep + survival::strata(stratum),
    data = cvx_infert,
    method = "efron",
    cluster = pooled.stratum
  ))
  for (f in list(f_iter, f_sep, f_clean, f_cluster)) {
    expect_s3_class(f$fit, "clogit")
    .cvx_expect_as_survival(f)
  }
  expect_false(is_converged(f_iter$fit, verbose = FALSE))
  expect_identical(.cvx_flagged(is_converged(f_sep$fit, verbose = FALSE)), "sep")
  expect_identical(.cvx_flagged(is_converged(f_cluster$fit, verbose = FALSE)), "sep")
  # no message, in particular not the one for classes without a method
  expect_silent(expect_true(is_converged(f_clean$fit)))

  # a null model, as for coxph models. clogit() replaces the class
  # "coxph.null".
  f_null <- .cvx_fit(survival::clogit(
    case ~ survival::strata(stratum),
    data = cvx_infert,
    method = "efron"
  ))
  expect_false(inherits(f_null$fit, "coxph.null"))
  expect_null(f_null$fit$coefficients)
  expect_silent(expect_true(is_converged(f_null$fit)))

  # changed predictor data, as for coxph models
  cvx_infert_changed <- cvx_infert
  f_changed <- .cvx_fit(survival::clogit(
    case ~ spontaneous + induced + survival::strata(stratum),
    data = cvx_infert_changed,
    method = "efron"
  ))
  expect_true(is_converged(f_changed$fit))
  cvx_infert_changed$induced <- rev(cvx_infert_changed$induced)
  .cvx_expect_not_assessed(f_changed$fit, "predictor data changed")

  # a penalized clogit() model has class c("clogit", "coxph")
  f_penal <- .cvx_fit(survival::clogit(
    case ~ spontaneous + survival::ridge(induced, theta = 1) + survival::strata(stratum),
    data = cvx_infert,
    method = "efron"
  ))
  expect_false(inherits(f_penal$fit, "coxph.penal"))
  .cvx_expect_not_assessed(f_penal$fit, "penalized")

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

  # rows added to the data
  cvx_ms_changed <- rbind(cvx_mgus2, cvx_mgus2[1:5, ])
  cvx_ms_changed$id[nrow(cvx_mgus2) + 1:5] <- max(cvx_mgus2$id) + 1:5
  .cvx_expect_not_assessed(f_common$fit, "predictor data changed")

  # if transitions share a baseline hazard, the transition of a stacked row is
  # not stored, and the data are not compared
  cvx_ms_shared <- cvx_mgus2
  f_shared <- .cvx_fit(coxph(
    list(Surv(etime, event) ~ age + sex, 1:2 + 1:3 ~ 1 / shared),
    data = cvx_ms_shared,
    id = id
  ))
  expect_gt(anyDuplicated(f_shared$fit$smap[1, ]), 0)
  expect_true(is_converged(f_shared$fit))
  cvx_ms_shared$age <- rev(cvx_mgus2$age)
  expect_false(.coxph_data_changed(f_shared$fit))

  # strata: `smap` has more than one row
  cvx_ms_strata <- cvx_mgus2
  f_strata <- .cvx_fit(coxph(
    Surv(etime, event) ~ age + strata(sex),
    data = cvx_ms_strata,
    id = id
  ))
  expect_gt(nrow(f_strata$fit$smap), 1)
  expect_true(is_converged(f_strata$fit))
  cvx_ms_strata$age <- rev(cvx_mgus2$age)
  .cvx_expect_not_assessed(f_strata$fit, "predictor data changed")

  # some subjects start in state "pcm", so the model has a transition out of
  # a state other than the first one
  cvx_ms_istate <- cvx_mgus2
  in_pcm <- withr::with_seed(1, stats::runif(nrow(cvx_mgus2)) < 0.3)
  cvx_ms_istate$st <- factor(
    ifelse(in_pcm, "pcm", "censor"),
    c("censor", "pcm", "death")
  )
  cvx_ms_istate$etime <- with(
    cvx_ms_istate,
    ifelse(!in_pcm & pstat == 1, ptime, futime)
  )
  cvx_ms_istate$event <- with(
    cvx_ms_istate,
    factor(
      ifelse(!in_pcm & pstat == 1, 1, 2 * death),
      0:2,
      c("censor", "pcm", "death")
    )
  )
  f_istate <- .cvx_fit(coxph(
    Surv(etime, event) ~ age + sex,
    data = cvx_ms_istate,
    id = id,
    istate = st
  ))
  expect_identical(ncol(f_istate$fit$cmap), 3L)
  expect_true(is_converged(f_istate$fit))
  cvx_ms_istate$age <- rev(cvx_ms_istate$age)
  .cvx_expect_not_assessed(f_istate$fit, "predictor data changed")
})


test_that("is_converged.coxph, the data hint only if the data cannot be read", {
  # survival gives this error for a multi-state model with a transition
  # without predictors, whose data are present
  cvx_hint_ms <- cvx_mgus2
  f_ms <- .cvx_fit(coxph(
    list(Surv(etime, event) ~ 1, 1:3 ~ age),
    data = cvx_hint_ms,
    id = id
  ))
  .cvx_expect_not_assessed(f_ms$fit, "object 'rr' not found")
  messages <- gsub("\\s+", " ", .cvx_messages(is_converged(f_ms$fit)))
  expect_no_match(messages, "no longer available", fixed = TRUE)

  # the hint is given if the data cannot be read (not mocked: the data object
  # is not found)
  cvx_hint_gone <- cvx_lung
  f <- .cvx_fit(coxph(Surv(time, status) ~ age + sex, data = cvx_hint_gone))
  rm(cvx_hint_gone)
  .cvx_expect_not_assessed(f$fit, "no longer available")

  # the score residuals of a single-state model fail although its data can be
  # read
  local_mocked_bindings(
    .coxph_infinite = function(...) stop("object 'rr' not found", call. = FALSE)
  )
  messages <- gsub("\\s+", " ", .cvx_messages(is_converged(cvx_fits$clean$fit)))
  expect_length(messages, 1)
  expect_match(messages, "object 'rr' not found", fixed = TRUE)
  expect_no_match(messages, "no longer available", fixed = TRUE)
  expect_false(is_converged(cvx_fits$clean$fit, verbose = FALSE))
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


test_that("is_converged.coxph, variable control arguments with a formula object", {
  # oracle: reference implementation, the warnings survival gives at the fit
  # (.cvx_expect_as_survival()), and the stated `iter.max` of each fit
  # insight reads the control arguments in the formula environment, which is
  # this test. coxph() read them in the frame of its call, here the function
  # with `n = 2`, so the fits ran out of iterations, while `n` is 50 here.
  cvx_env_fo <- Surv(time, status) ~ ph.ecog + age
  n <- 50
  fit_arg <- function(n) coxph(cvx_env_fo, data = cvx_lung, iter.max = n)
  fit_control <- function(n) {
    coxph(cvx_env_fo, data = cvx_lung, control = coxph.control(iter.max = n))
  }
  fit_update <- function(n) {
    coxph(update(cvx_env_fo, . ~ . + sex), data = cvx_lung, iter.max = n, eps = n / 1e10)
  }
  f_arg <- .cvx_fit(fit_arg(2))
  f_control <- .cvx_fit(fit_control(2))
  f_update <- .cvx_fit(fit_update(2))
  for (f in list(f_arg, f_control, f_update)) {
    expect_match(f$warnings, "Ran out of iterations", fixed = TRUE)
  }
  .cvx_expect_not_assessed(f_arg$fit, "control arguments of the model call (`iter.max`)")
  .cvx_expect_not_assessed(
    f_control$fit,
    "control arguments of the model call (`control`)"
  )
  .cvx_expect_not_assessed(
    f_update$fit,
    "control arguments of the model call (`iter.max`, `eps`)"
  )

  # correct fits of this kind cannot be assessed either, for example at the
  # level of this test, where `n` is the value that the fit used
  f_top <- .cvx_fit(coxph(cvx_env_fo, data = cvx_lung, iter.max = n))
  expect_length(f_top$warnings, 0)
  .cvx_expect_not_assessed(f_top$fit, "control arguments of the model call (`iter.max`)")

  # a literal formula: the formula environment is the frame of the call
  fit_literal <- function(n) {
    coxph(Surv(time, status) ~ ph.ecog + age, data = cvx_lung, iter.max = n)
  }
  f_literal_fail <- .cvx_fit(fit_literal(2))
  f_literal_pass <- .cvx_fit(fit_literal(30))
  # a formula object with control arguments that hold only constants
  fit_constant <- function() coxph(cvx_env_fo, data = cvx_lung, iter.max = 2)
  f_constant_fail <- .cvx_fit(fit_constant())
  f_constant_pass <- .cvx_fit(coxph(
    update(cvx_env_fo, . ~ . + sex),
    data = cvx_lung,
    control = coxph.control(iter.max = 30)
  ))
  for (f in list(f_literal_fail, f_constant_fail)) {
    .cvx_expect_as_survival(f)
    result <- is_converged(f$fit, verbose = FALSE)
    expect_false(result)
    expect_identical(attr(result, "diagnostics")$Threshold, 2)
  }
  for (f in list(f_literal_pass, f_constant_pass)) {
    expect_length(f$warnings, 0)
    .cvx_expect_as_survival(f)
    result <- is_converged(f$fit, verbose = FALSE)
    expect_true(result)
    expect_identical(attr(result, "diagnostics")$Threshold[1], 30)
  }

  # clogit() stores a coxph() call with the formula as an object, and the
  # call of the user as `userCall`
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
  expect_match(f_clogit$warnings, "Ran out of iterations", fixed = TRUE)
  .cvx_expect_as_survival(f_clogit)
  expect_identical(
    attr(is_converged(f_clogit$fit, verbose = FALSE), "diagnostics")$Threshold,
    2
  )
  cvx_env_clogit_fo <- case ~ spontaneous + induced + survival::strata(stratum)
  fit_clogit_fo <- function(n) {
    survival::clogit(
      cvx_env_clogit_fo,
      data = cvx_env_infert,
      method = "efron",
      iter.max = n
    )
  }
  f_clogit_fo <- .cvx_fit(fit_clogit_fo(2))
  .cvx_expect_not_assessed(
    f_clogit_fo$fit,
    "control arguments of the model call (`iter.max`)"
  )
})


test_that("is_converged.coxph, a data object hidden by one of the same name", {
  # oracle: construction, the outer object is made equal to or different from
  # the data of the fit
  # the formula environment is this test, so the data object of the call is
  # read here, and not in the function that fitted the model
  cvx_hidden_fo <- Surv(time, status) ~ age + sex
  fit_local <- function() {
    cvx_hidden <- cvx_lung
    coxph(cvx_hidden_fo, data = cvx_hidden)
  }
  f <- .cvx_fit(fit_local())
  expect_null(f$fit[["model"]])
  expect_null(f$fit[["x"]])
  expect_identical(attr(f$fit$y, "type"), "right")

  # the same values here: the data are read from this object
  cvx_hidden <- cvx_lung
  expect_true(is_converged(f$fit))
  # other predictor values here
  cvx_hidden$age <- withr::with_seed(123, sample(cvx_hidden$age))
  .cvx_expect_not_assessed(f$fit, "predictor data changed")
})


test_that("is_converged.coxph, strata changed after the fit", {
  # oracle: reference implementation, the warnings survival gives at the fit,
  # and the same fits with `model = TRUE`, which read no data
  cvx_strata <- cvx_lung
  f_efron <- .cvx_fit(coxph(
    Surv(time, status) ~ age + ph.ecog + strata(sex),
    data = cvx_strata
  ))
  f_breslow <- .cvx_fit(coxph(
    Surv(time, status) ~ age + ph.ecog + strata(sex),
    data = cvx_strata,
    ties = "breslow"
  ))
  f_aliased <- .cvx_fit(coxph(
    Surv(time, status) ~ age + age2 + strata(sex),
    data = cvx_strata
  ))
  expect_true(is.na(stats::coef(f_aliased$fit)[["age2"]]))
  fits <- list(f_efron, f_breslow, f_aliased)

  # the model frame is stored with `model = TRUE`, so these fits read no data
  # and get the result of main
  fits_model <- lapply(fits, function(f) {
    model_call <- f$fit$call
    model_call$model <- TRUE
    .cvx_fit(eval(model_call))
  })
  # the model matrix and strata are stored with `x = TRUE`
  f_x <- .cvx_fit(coxph(
    Surv(time, status) ~ age + ph.ecog + strata(sex),
    data = cvx_strata,
    x = TRUE
  ))
  expect_identical(
    is_converged(f_x$fit, verbose = FALSE),
    is_converged(fits_model[[1]]$fit, verbose = FALSE)
  )

  # data unchanged
  for (i in seq_along(fits)) {
    expect_null(fits[[i]]$fit[["model"]])
    expect_null(fits[[i]]$fit[["x"]])
    .cvx_expect_as_survival(fits[[i]])
    expect_true(is_converged(fits[[i]]$fit))
    expect_identical(
      is_converged(fits[[i]]$fit, verbose = FALSE),
      is_converged(fits_model[[i]]$fit, verbose = FALSE)
    )
  }

  # strata relabeled without a change of grouping are not detected, and do
  # not change the score residuals
  cvx_strata$sex <- 3 - cvx_lung$sex
  for (i in seq_along(fits)) {
    expect_equal(
      is_converged(fits[[i]]$fit, verbose = FALSE),
      is_converged(fits_model[[i]]$fit, verbose = FALSE),
      tolerance = 1e-10
    )
  }

  # the strata variable permuted: the grouping changes, while the linear
  # predictor stays the same. Main flagged `age` and `ph.ecog` of the Efron
  # fit as infinite.
  cvx_strata <- cvx_lung
  cvx_strata$sex <- withr::with_seed(123, sample(cvx_strata$sex))
  expect_false(identical(cvx_strata$sex, cvx_lung$sex))
  for (i in seq_along(fits)) {
    .cvx_expect_not_assessed(fits[[i]]$fit, "predictor data changed")
    expect_true(is_converged(fits_model[[i]]$fit))
  }
  expect_true(is_converged(f_x$fit))

  # the refit calls survival::coxph(): the stored call names `coxph` without
  # the namespace, as after library(survival), and always for clogit(). In the
  # formula environment of these fits, `coxph` is a function that stops, so a
  # refit through it fails whether or not survival is attached.
  bare <- new.env(parent = baseenv())
  bare$cvx_bare <- cvx_lung
  bare$cvx_bare_infert <- datasets::infert
  f_bare <- .cvx_fit(eval(
    quote(survival::coxph(
      Surv(time, status) ~ age + ph.ecog + strata(sex),
      data = cvx_bare
    )),
    bare
  ))
  f_bare$fit$call[[1]] <- as.name("coxph")
  # clogit() calls `coxph()` from the frame of its call
  bare$coxph <- survival::coxph
  f_bare_clogit <- .cvx_fit(eval(
    quote(survival::clogit(
      case ~ spontaneous + induced + survival::strata(stratum),
      data = cvx_bare_infert,
      method = "efron"
    )),
    bare
  ))
  bare$coxph <- function(...) stop("not survival::coxph()", call. = FALSE)
  expect_identical(f_bare_clogit$fit$call[[1]], as.name("coxph"))
  for (f in list(f_bare, f_bare_clogit)) {
    expect_error(
      eval(f$fit$call, environment(f$fit$terms)),
      "not survival::coxph()",
      fixed = TRUE
    )
    .cvx_expect_as_survival(f)
    expect_true(is_converged(f$fit))
  }
  bare$cvx_bare$sex <- withr::with_seed(123, sample(cvx_lung$sex))
  .cvx_expect_not_assessed(f_bare$fit, "predictor data changed")

  # the refit runs only for fits with a `strata()` term
  cvx_strata <- cvx_lung
  refits <- 0L
  local_mocked_bindings(.coxph_strata_changed = function(...) {
    refits <<- refits + 1L
    FALSE
  })
  expect_true(is_converged(cvx_fits$clean$fit))
  expect_identical(refits, 0L)
  expect_true(is_converged(f_efron$fit))
  expect_identical(refits, 1L)
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
  .cvx_expect_not_assessed(no_nevent, "no valid number of iterations")
  # a multi-state model without the maps of the stacked data: no error
  no_smap <- cvx_fits$ms_right$fit
  no_smap$smap <- NULL
  expect_false(.coxph_data_changed(no_smap))
  .cvx_expect_not_assessed(no_smap, "score vector could not be computed")
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
