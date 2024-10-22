skip_if_offline()
skip_if_not_installed("GLMMadaptive")
skip_if_not_installed("lme4")
skip_if_not_installed("httr2")

m <- download_model("GLMMadaptive_zi_2")
m2 <- download_model("GLMMadaptive_zi_1")

skip_if(is.null(m))
skip_if(is.null(m2))

data(cbpp, package = "lme4")
tmp <<- cbpp
m3 <- GLMMadaptive::mixed_model(
  cbind(incidence, size - incidence) ~ period,
  random = ~ 1 | herd,
  data = tmp,
  family = binomial
)

test_that("model_info", {
  expect_true(model_info(m)$is_zero_inflated)
  expect_true(model_info(m)$is_count)
  expect_true(model_info(m)$is_pois)
  expect_false(model_info(m)$is_negbin)
  expect_false(model_info(m)$is_linear)
})

test_that("get_deviance + logLik", {
  expect_equal(get_deviance(m3), 183.96674, tolerance = 1e-3)
  expect_equal(get_loglikelihood(m3), logLik(m3), tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(get_df(m3, type = "model"), 5L)
})

test_that("get_df", {
  expect_equal(
    get_df(m3, type = "residual"),
    51,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m3, type = "normal"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m3, type = "wald"),
    Inf,
    ignore_attr = TRUE
  )
})

test_that("n_parameters", {
  expect_identical(n_parameters(m), 6L)
  expect_identical(n_parameters(m2), 6L)
  expect_identical(n_parameters(m, effects = "random"), 2L)
  expect_identical(n_parameters(m2, effects = "random"), 1L)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m, effects = "fixed")$conditional,
    c("child", "camper")
  )
  expect_identical(
    find_predictors(m, effects = "fixed")$zero_inflated,
    c("child", "livebait")
  )
  expect_identical(
    find_predictors(m, effects = "all", flatten = TRUE),
    c("child", "camper", "persons", "livebait")
  )
  expect_identical(
    find_predictors(m, effects = "all")$zero_inflated_random,
    "persons"
  )
  expect_identical(find_predictors(m, effects = "random")$random, "persons")
  expect_identical(
    find_predictors(
      m,
      effects = "fixed",
      component = "cond",
      flatten = TRUE
    ),
    c("child", "camper")
  )
  expect_identical(
    find_predictors(
      m,
      effects = "all",
      component = "cond",
      flatten = TRUE
    ),
    c("child", "camper", "persons")
  )
  expect_identical(
    find_predictors(m, effects = "all", component = "cond")$conditional,
    c("child", "camper")
  )

  expect_identical(
    find_predictors(
      m,
      effects = "random",
      component = "cond",
      flatten = TRUE
    ),
    "persons"
  )
  expect_identical(
    find_predictors(
      m,
      effects = "fixed",
      component = "zi",
      flatten = TRUE
    ),
    c("child", "livebait")
  )
  expect_identical(
    find_predictors(
      m,
      effects = "all",
      component = "zi",
      flatten = TRUE
    ),
    c("child", "livebait", "persons")
  )
  expect_identical(
    find_predictors(
      m,
      effects = "random",
      component = "zi",
      flatten = TRUE
    ),
    "persons"
  )
  expect_null(find_predictors(
    m,
    effects = "fixed",
    component = "dispersion",
    flatten = TRUE
  ))
  expect_null(find_predictors(
    m,
    effects = "all",
    component = "dispersion",
    flatten = TRUE
  ))
  expect_null(find_predictors(
    m,
    effects = "random",
    component = "dispersion",
    flatten = TRUE
  ))
})

test_that("find_response", {
  expect_identical(find_response(m), "count")
})

test_that("link_inverse", {
  expect_identical(link_inverse(m)(0.2), exp(0.2))
})

test_that("clean_names", {
  expect_identical(
    clean_names(m),
    c("count", "child", "camper", "persons", "livebait")
  )
})

test_that("find_formula", {
  expect_length(find_formula(m), 4)
  expect_named(
    find_formula(m),
    c(
      "conditional",
      "random",
      "zero_inflated",
      "zero_inflated_random"
    )
  )
})

test_that("find_random", {
  expect_identical(
    find_random(m),
    list(random = "persons", zero_inflated_random = "persons")
  )
  expect_identical(find_random(m, flatten = TRUE), "persons")
})

test_that("find_respone", {
  expect_identical(find_response(m), "count")
})

test_that("find_terms", {
  expect_identical(
    find_terms(m),
    list(
      response = "count",
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "persons"
    )
  )
  expect_identical(
    find_terms(m, flatten = TRUE),
    c("count", "child", "camper", "persons", "livebait")
  )
})

test_that("get_response", {
  expect_identical(get_response(m3), cbpp[, c("incidence", "size")])
})

test_that("get_predictors", {
  expect_identical(
    colnames(get_predictors(m)),
    c("child", "camper", "livebait")
  )
})

test_that("get_random", {
  expect_identical(colnames(get_random(m)), "persons")
})


# data stems from model frame, since we downloaded models, so it's not
# in the environment. Thus, "get_data()" throws warning, and we therefore
# set verbose = FALSE

test_that("get_data", {
  expect_identical(
    sort(colnames(get_data(m, verbose = FALSE))),
    sort(c("count", "child", "camper", "livebait", "persons"))
  )
  expect_identical(
    colnames(get_data(m, effects = "fixed", verbose = FALSE)),
    c("count", "child", "camper", "livebait")
  )
  expect_identical(colnames(get_data(m, effects = "random", verbose = FALSE)), "persons")
  expect_identical(
    sort(colnames(get_data(m, component = "zi", verbose = FALSE))),
    sort(c("count", "child", "livebait", "persons"))
  )
  expect_identical(
    sort(colnames(get_data(m, component = "zi", effects = "fixed", verbose = FALSE))),
    sort(c("count", "child", "livebait"))
  )
  expect_identical(colnames(get_data(
    m,
    component = "zi", effects = "random", verbose = FALSE
  )), "persons")
  expect_identical(
    colnames(get_data(m, component = "cond", verbose = FALSE)),
    c("count", "child", "camper", "persons")
  )
  expect_identical(colnames(get_data(
    m,
    component = "cond", effects = "fixed", verbose = FALSE
  )), c("count", "child", "camper"))
  expect_identical(colnames(get_data(
    m,
    component = "cond", effects = "random", verbose = FALSE
  )), "persons")
  expect_identical(colnames(suppressWarnings(get_data(m, component = "dispersion"))), "count")
  expect_null(suppressWarnings(get_data(m, component = "dispersion", effects = "random", verbose = FALSE)))
  expect_identical(
    colnames(get_data(m3)),
    c("incidence", "size", "period", "herd")
  )
})

test_that("find_parameter", {
  expect_identical(
    find_parameters(m),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      random = "(Intercept)",
      zero_inflated = c("(Intercept)", "child", "livebait1"),
      zero_inflated_random = "zi_(Intercept)"
    )
  )
  expect_identical(
    find_parameters(m2),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      random = "(Intercept)",
      zero_inflated = c("(Intercept)", "child", "livebait1")
    )
  )
  expect_identical(
    find_parameters(m3),
    list(
      conditional = c("(Intercept)", "period2", "period3", "period4"),
      random = "(Intercept)"
    )
  )

  expect_identical(nrow(get_parameters(m)), 6L)
  expect_equal(
    get_parameters(m, effects = "random"),
    list(
      random = c(-1.0715496, 1.4083630, 1.9129880, 0.2007521),
      zero_inflated_random = c(-0.1676294, 0.5502481, 1.2592406, 0.9336591)
    ),
    tolerance = 1e-5
  )
  expect_identical(nrow(get_parameters(m2)), 6L)
  expect_equal(get_parameters(m2, effects = "random"),
    list(random = c(
      -1.3262364, -0.2048055, 1.3852572, 0.5282277
    )),
    tolerance = 1e-5
  )
  expect_identical(
    get_parameters(m3)$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "conditional"
    )
  )
  expect_error(get_parameters(m3, "zi"))
})

test_that("linkfun", {
  expect_false(is.null(link_function(m)))
  expect_false(is.null(link_function(m2)))
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m))
  expect_false(is_multivariate(m2))
})

test_that("find_algorithm", {
  expect_identical(
    find_algorithm(m),
    list(algorithm = "quasi-Newton", optimizer = "optim")
  )
})

test_that("detect custom families", {
  skip_on_cran()
  set.seed(1234)
  n <- 100 # number of subjects
  K <- 8 # number of measurements per subject
  t_max <- 5 # maximum follow-up time

  # we construct a data frame with the design:
  # everyone has a baseline measurement, and then measurements at random follow-up times
  DF <- data.frame(
    id = rep(seq_len(n), each = K),
    time = as.vector(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
    sex = rep(gl(2, n / 2, labels = c("male", "female")), each = K)
  )

  # design matrices for the fixed and random effects non-zero part
  X <- model.matrix(~ sex * time, data = DF)
  Z <- model.matrix(~time, data = DF)
  # design matrices for the fixed and random effects zero part
  X_zi <- model.matrix(~sex, data = DF)
  Z_zi <- model.matrix(~1, data = DF)

  betas <- c(-2.13, -0.25, 0.24, -0.05) # fixed effects coefficients non-zero part
  sigma <- 0.5 # standard deviation error terms non-zero part
  gammas <- c(-1.5, 0.5) # fixed effects coefficients zero part
  D11 <- 0.5 # variance of random intercepts non-zero part
  D22 <- 0.1 # variance of random slopes non-zero part
  D33 <- 0.4 # variance of random intercepts zero part

  # we simulate random effects
  b <- cbind(rnorm(n, sd = sqrt(D11)), rnorm(n, sd = sqrt(D22)), rnorm(n, sd = sqrt(D33)))
  # linear predictor non-zero part
  eta_y <- as.vector(X %*% betas + rowSums(Z * b[DF$id, 1:2, drop = FALSE]))
  # linear predictor zero part
  eta_zi <- as.vector(X_zi %*% gammas + rowSums(Z_zi * b[DF$id, 3, drop = FALSE]))
  # we simulate log-normal longitudinal data
  DF$y <- exp(rnorm(n * K, mean = eta_y, sd = sigma))
  # we set the zeros from the logistic regression
  DF$y[as.logical(rbinom(n * K, size = 1, prob = plogis(eta_zi)))] <- 0

  hurdle.lognormal <- function() {
    stats <- make.link("identity")
    log_dens <- function(y, eta, mu_fun, phis, eta_zi) {
      scaleParameter <- exp(phis)
      # binary indicator for y > 0
      ind <- y > 0
      # non-zero part
      eta <- as.matrix(eta)
      eta_zi <- as.matrix(eta_zi)
      out <- eta
      out[ind, ] <- plogis(eta_zi[ind, ], lower.tail = FALSE, log.p = TRUE) +
        dnorm(x = log(y[ind]), mean = eta[ind, ], sd = scaleParameter, log = TRUE)
      # zero part
      out[!ind, ] <- plogis(eta_zi[!ind, ], log.p = TRUE)
      attr(out, "mu_y") <- eta
      out
    }
    score_eta_fun <- function(y, mu, phis, eta_zi) {
      scaleParameter <- exp(phis)
      # binary indicator for y > 0
      ind <- y > 0
      # non-zero part
      eta <- as.matrix(mu)
      out <- eta
      out[!ind, ] <- 0
      out[ind, ] <- (log(y[ind]) - eta[ind, ]) / scaleParameter^2
      out
    }
    score_eta_zi_fun <- function(y, mu, phis, eta_zi) {
      ind <- y > 0
      probs <- plogis(as.matrix(eta_zi))
      out <- 1 - probs
      out[ind, ] <- -probs[ind, ]
      out
    }
    score_phis_fun <- function(y, mu, phis, eta_zi) {
      scaleParameter <- exp(phis)
      # binary indicator for y > 0
      ind <- y > 0
      # non-zero part
      eta <- as.matrix(mu)
      out <- eta
      out[!ind, ] <- 0
      out[ind, ] <- -1 + (log(y[ind]) - eta[ind, ])^2 / scaleParameter^2
      out
    }
    simulateResponses <- function(n, mu, phis, eta_zi) {
      y <- rlnorm(n = n, meanlog = mu, sdlog = exp(phis))
      y[as.logical(rbinom(n, 1, plogis(eta_zi)))] <- 0
      y
    }
    structure(
      list(
        family = "two-part log-normal", link = stats$name,
        linkfun = stats$linkfun, linkinv = stats$linkinv, log_dens = log_dens,
        score_eta_fun = score_eta_fun, score_eta_zi_fun = score_eta_zi_fun,
        score_phis_fun = score_phis_fun, simulate = simulateResponses
      ),
      class = "family"
    )
  }
  km1 <- GLMMadaptive::mixed_model(y ~ sex * time,
    random = ~ 1 | id, data = DF,
    family = hurdle.lognormal(), n_phis = 1,
    zi_fixed = ~sex
  )
  out <- model_info(km1)
  expect_true(out$is_zero_inflated)
})
