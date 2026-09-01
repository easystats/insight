skip_if_not_installed("glmmTMB", minimum_version = "1.1.15")
skip_if_not_installed("ordinal")
skip_if_not_installed("lme4")

data(wine, package = "ordinal")

# fixed effects only, compared against ordinal::clm()
m_tmb <- glmmTMB::glmmTMB(
  rating ~ temp * contact,
  data = wine,
  family = glmmTMB::ordinal()
)
m_clm <- ordinal::clm(rating ~ temp * contact, data = wine)

# mixed, compared against ordinal::clmm()
m_tmb_mixed <- glmmTMB::glmmTMB(
  rating ~ temp + contact + (1 | judge),
  data = wine,
  family = glmmTMB::ordinal()
)
m_clmm <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)

test_that("model_info", {
  expect_true(model_info(m_tmb)$is_ordinal)
  expect_true(model_info(m_tmb)$is_logit)
  expect_false(model_info(m_tmb)$is_linear)
  expect_true(model_info(m_tmb_mixed)$is_mixed)
})

test_that("find_parameters: thresholds, no fixed intercept", {
  expect_identical(find_parameters(m_tmb), find_parameters(m_clm))
  expect_identical(
    find_parameters(m_tmb_mixed, effects = "fixed"),
    find_parameters(m_clmm)
  )
  expect_identical(
    find_parameters(m_tmb_mixed),
    list(
      conditional = c("1|2", "2|3", "3|4", "4|5", "tempwarm", "contactyes"),
      random = list(judge = "(Intercept)")
    )
  )
})

test_that("get_parameters: thresholds match ordinal", {
  out <- get_parameters(m_tmb)
  expect_identical(out$Parameter, get_parameters(m_clm)$Parameter)
  expect_equal(out$Estimate, get_parameters(m_clm)$Estimate, tolerance = 1e-3)
  expect_identical(unique(out$Component), "conditional")

  out <- get_parameters(m_tmb_mixed)
  expect_identical(out$Parameter, get_parameters(m_clmm)$Parameter)
  expect_equal(out$Estimate, get_parameters(m_clmm)$Estimate, tolerance = 1e-3)

  out <- get_parameters(m_tmb_mixed, effects = "random")
  expect_named(out$random, "judge")
})

test_that("get_varcov: delta-method threshold SEs match ordinal", {
  vc <- get_varcov(m_tmb)
  expect_identical(dimnames(vc)[[1]], get_parameters(m_clm)$Parameter)
  expect_equal(
    sqrt(diag(vc)),
    sqrt(diag(vcov(m_clm))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  vc <- get_varcov(m_tmb_mixed)
  ref <- vcov(m_clmm)[1:6, 1:6]
  expect_identical(dimnames(vc)[[1]], dimnames(ref)[[1]])
  expect_equal(vc, ref, tolerance = 1e-2, ignore_attr = TRUE)

  # "full" stays on glmmTMB's internal (softmax) scale for the thresholds
  vc_full <- suppressWarnings(get_varcov(m_tmb, component = "full"))
  expect_false(isTRUE(all.equal(
    unname(sqrt(diag(vc_full))[c("1|2", "2|3")]),
    unname(sqrt(diag(vc))[c("1|2", "2|3")])
  )))
})

test_that("get_statistic matches ordinal", {
  out <- get_statistic(m_tmb)
  expect_identical(out$Parameter, get_statistic(m_clm)$Parameter)
  expect_equal(out$Statistic, get_statistic(m_clm)$Statistic, tolerance = 1e-3)
  expect_identical(attributes(out)$statistic, "z-statistic")

  out <- get_statistic(m_tmb_mixed)
  expect_equal(out$Statistic, get_statistic(m_clmm)$Statistic, tolerance = 1e-2)
})

test_that("get_predicted: per-category probabilities", {
  out <- get_predicted(m_tmb, ci = 0.95, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Row", "Response", "Predicted") %in% colnames(out)))
  expect_identical(nrow(out), nrow(wine) * nlevels(wine$rating))

  # matches glmmTMB's own predict(type = "probs")
  pr <- predict(m_tmb, type = "probs", se.fit = TRUE)
  expect_equal(out$Predicted, as.vector(pr$fit), tolerance = 1e-8)
  ci_data <- attributes(out)$ci_data
  expect_equal(ci_data$SE, as.vector(pr$se.fit), tolerance = 1e-8)

  # intervals match ordinal::clm() and stay within [0, 1]
  out_clm <- get_predicted(m_clm, ci = 0.95, verbose = FALSE)
  ci_clm <- attributes(out_clm)$ci_data
  expect_identical(colnames(out), setdiff(colnames(out_clm), "rating"))
  expect_equal(out$Predicted, out_clm$Predicted, tolerance = 1e-3)
  expect_equal(ci_data$SE, ci_clm$SE, tolerance = 1e-3)
  expect_equal(ci_data$CI_low, ci_clm$CI_low, tolerance = 1e-3)
  expect_equal(ci_data$CI_high, ci_clm$CI_high, tolerance = 1e-3)
  expect_true(all(ci_data$CI_low >= 0 & ci_data$CI_high <= 1))
  expect_false("rating" %in% colnames(out))

  # data grid
  dg <- get_datagrid(m_tmb, "temp", verbose = FALSE)
  out <- get_predicted(m_tmb, data = dg, verbose = FALSE)
  expect_identical(nrow(out), nrow(dg) * nlevels(wine$rating))
  expect_true("temp" %in% colnames(out))
  expect_equal(
    out$Predicted,
    as.vector(predict(m_tmb, newdata = dg, type = "probs")),
    tolerance = 1e-8
  )

  # link scale is untouched
  expect_equal(
    as.vector(get_predicted(m_tmb, predict = "link", verbose = FALSE)),
    unname(predict(m_tmb, type = "link")),
    tolerance = 1e-8
  )

  # random effects
  out <- get_predicted(m_tmb_mixed, verbose = FALSE)
  expect_equal(
    out$Predicted,
    as.vector(predict(m_tmb_mixed, type = "probs")),
    tolerance = 1e-8
  )
})

test_that("get_predicted: type and include_random arguments", {
  # explicit `type = "response"` means probabilities, as for clm
  out <- get_predicted(m_tmb, predict = NULL, type = "response", verbose = FALSE)
  expect_equal(
    out$Predicted,
    as.vector(predict(m_tmb, type = "probs")),
    tolerance = 1e-8
  )
  out <- get_predicted(m_tmb, predict = NULL, type = "probs", verbose = FALSE)
  expect_true("Response" %in% colnames(out))
  out <- get_predicted(m_tmb, predict = "probs", verbose = FALSE)
  expect_true("Response" %in% colnames(out))

  # `type` takes precedence over the default `predict`
  out <- suppressMessages(get_predicted(m_tmb, type = "link"))
  expect_false("Response" %in% colnames(out))
  expect_equal(
    as.vector(out),
    unname(predict(m_tmb, type = "link")),
    tolerance = 1e-8
  )

  # glmmTMB's own other types pass through
  expect_equal(
    as.vector(get_predicted(
      m_tmb,
      predict = NULL,
      type = "conditional",
      verbose = FALSE
    )),
    unname(predict(m_tmb, type = "conditional")),
    tolerance = 1e-8
  )

  # bootstrapping and prediction intervals are not implemented
  expect_warning(get_predicted(m_tmb, iterations = 5), "Bootstrapped")
  expect_warning(get_predicted(m_tmb, predict = "prediction"), "not supported")

  # population-level predictions
  out <- get_predicted(m_tmb_mixed, include_random = FALSE, verbose = FALSE)
  expect_equal(
    out$Predicted,
    as.vector(predict(m_tmb_mixed, type = "probs", re.form = NA)),
    tolerance = 1e-8
  )
  dg <- get_datagrid(m_tmb_mixed, "temp", verbose = FALSE)
  out <- get_predicted(m_tmb_mixed, data = dg, verbose = FALSE)
  expect_equal(
    out$Predicted,
    as.vector(predict(m_tmb_mixed, newdata = dg, type = "probs", re.form = NA)),
    tolerance = 1e-8
  )
})

test_that("probit link", {
  m_probit <- glmmTMB::glmmTMB(
    rating ~ temp + contact,
    data = wine,
    family = glmmTMB::ordinal(link = "probit")
  )
  m_clm_probit <- ordinal::clm(rating ~ temp + contact, data = wine, link = "probit")
  expect_true(model_info(m_probit)$is_probit)
  expect_equal(
    get_parameters(m_probit)$Estimate,
    get_parameters(m_clm_probit)$Estimate,
    tolerance = 1e-3
  )
  expect_equal(
    sqrt(diag(get_varcov(m_probit))),
    sqrt(diag(vcov(m_clm_probit))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("get_variance matches clmm", {
  out <- get_variance(m_tmb_mixed)
  ref <- get_variance(m_clmm)
  expect_equal(out$var.residual, pi^2 / 3, tolerance = 1e-6)
  expect_equal(unlist(out), unlist(ref), tolerance = 1e-3)
  # observation-level approximation falls back, as for clmm
  expect_equal(
    get_variance(m_tmb_mixed, approximation = "observation_level")$var.residual,
    pi^2 / 3,
    tolerance = 1e-6
  )
})

test_that("non-estimated parameters", {
  # rank deficient model: the dropped coefficient is kept (as NA) in
  # get_parameters(), like for other glmmTMB models, but has no variance
  wine2 <- wine
  wine2$temp2 <- wine2$temp
  m_rd <- suppressMessages(glmmTMB::glmmTMB(
    rating ~ temp + temp2 + contact,
    data = wine2,
    family = glmmTMB::ordinal()
  ))
  params <- get_parameters(m_rd)
  expect_identical(
    params$Parameter,
    c("1|2", "2|3", "3|4", "4|5", "tempwarm", "temp2warm", "contactyes")
  )
  expect_true(is.na(params$Estimate[params$Parameter == "temp2warm"]))
  vc <- suppressWarnings(get_varcov(m_rd))
  expect_false("temp2warm" %in% dimnames(vc)[[1]])
  out <- suppressWarnings(get_statistic(m_rd))
  expect_identical(out$Parameter, params$Parameter)
  expect_true(is.na(out$Statistic[out$Parameter == "temp2warm"]))
  ref <- coef(summary(m_rd))$cond[, "z value"]
  ref <- ref[!is.na(ref)]
  expect_equal(
    out$Statistic[match(names(ref), out$Parameter)],
    unname(ref),
    tolerance = 1e-6
  )

  # errors from glmmTMB are wrapped in insight's message
  m_disp <- glmmTMB::glmmTMB(
    rating ~ temp + contact,
    dispformula = ~contact,
    data = wine,
    family = glmmTMB::ordinal()
  )
  expect_error(get_varcov(m_disp), "Can't extract")
})

test_that("get_predicted: classification", {
  out <- get_predicted(m_tmb, predict = "classification", verbose = FALSE)
  expect_s3_class(out, "factor")
  expect_identical(levels(out), levels(wine$rating))
  pr <- predict(m_tmb, type = "probs")
  expect_identical(as.character(out), colnames(pr)[max.col(pr)])
  expect_warning(
    get_predicted(m_tmb, predict = "classification", ci = 0.95),
    "not available"
  )
})
