.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {
  if (suppressWarnings(require("testthat") &&
    require("insight") && require("lme4") &&
    require("BayesFactor") &&
    require("rstanarm"))) {

    # skip_on_cran()

    # defining models ---------------------

    m1 <- insight::download_model("stanreg_merMod_5")
    m2 <- insight::download_model("stanreg_glm_6")
    m3 <- insight::download_model("stanreg_glm_1")

    data("puzzles")
    m4 <-
      stan_glm(
        RT ~ color * shape,
        data = puzzles,
        prior = rstanarm::cauchy(0, c(3, 1, 2)),
        iter = 500,
        chains = 2,
        refresh = 0
      )
    m5 <-
      stan_glm(
        RT ~ color * shape,
        data = puzzles,
        prior = rstanarm::cauchy(0, c(1, 2, 3)),
        iter = 500,
        chains = 2,
        refresh = 0
      )
    m6 <- insight::download_model("stanreg_gamm4_1")

    m7 <- stan_lm(mpg ~ wt + qsec + am,
      data = mtcars, prior = R2(0.75),
      chains = 1, iter = 300, refresh = 0
    )

    m8 <- stan_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy, refresh = 0)

    m9 <- stan_aov(yield ~ block + N * P * K, data = npk, prior = R2(0.5), refresh = 0)

    N <- 200
    x <- rnorm(N, 2, 1)
    z <- rnorm(N, 2, 1)
    mu <- binomial(link = "logit")$linkinv(1 + 0.2 * x)
    phi <- exp(1.5 + 0.4 * z)
    y <- rbeta(N, mu * phi, (1 - mu) * phi)
    hist(y, col = "dark grey", border = FALSE, xlim = c(0, 1))
    fake_dat <- data.frame(y, x, z)
    m10 <- stan_betareg(
      y ~ x | z,
      data = fake_dat,
      link = "logit",
      link.phi = "log",
      algorithm = "optimizing" # just for speed of example
    )

    ols <- lm(mpg ~ wt + qsec + am,
      data = mtcars, # all row are complete so ...
      na.action = na.exclude
    ) # not necessary in this case
    b <- coef(ols)[-1]
    R <- qr.R(ols$qr)[-1, -1]
    SSR <- crossprod(ols$residuals)[1]
    not_NA <- !is.na(fitted(ols))
    N <- sum(not_NA)
    xbar <- colMeans(mtcars[not_NA, c("wt", "qsec", "am")])
    y <- mtcars$mpg[not_NA]
    ybar <- mean(y)
    s_y <- sd(y)
    m11 <- stan_biglm.fit(b, R, SSR, N, xbar, ybar, s_y,
      prior = R2(.75),
      # the next line is only to make the example go fast
      chains = 1, iter = 500, seed = 12345
    )

    dat <- infert[order(infert$stratum), ] # order by strata
    m12 <- stan_clogit(case ~ spontaneous + induced + (1 | education),
      strata = stratum,
      data = dat,
      subset = parity <= 2,
      QR = TRUE,
      chains = 2, iter = 500
    ) # for speed only

    if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
      m13 <- stan_jm(
        formulaLong = logBili ~ year + (1 | id),
        dataLong = pbcLong,
        formulaEvent = Surv(futimeYears, death) ~ sex + trt,
        dataEvent = pbcSurv,
        time_var = "year",
        # this next line is only to keep the example small in size!
        chains = 1, cores = 1, seed = 12345, iter = 1000
      )
      expect_snapshot(model_info(m13))
    }

    data("Orange", package = "datasets")
    Orange$circumference <- Orange$circumference / 100
    Orange$age <- Orange$age / 100
    m14 <- stan_nlmer(
      circumference ~ stats::SSlogis(age, Asym, xmid, scal) ~ Asym | Tree,
      data = Orange,
      # for speed only
      chains = 1,
      iter = 1000
    )

    m15 <- stan_mvmer(
      formula = list(
        logBili ~ year + (1 | id),
        albumin ~ sex + year + (year | id)
      ),
      data = pbcLong,
      # this next line is only to keep the example small in size!
      chains = 1, cores = 1, seed = 12345, iter = 1000
    )

    test_that("model_info-stanreg-glm", {
      expect_snapshot(model_info(m1))
      expect_snapshot(model_info(m2))
      expect_snapshot(model_info(m3))
      expect_snapshot(model_info(m4))
      expect_snapshot(model_info(m5))
      expect_snapshot(model_info(m6))
      expect_snapshot(model_info(m7))
      expect_snapshot(model_info(m8))
      expect_snapshot(model_info(m9))
      expect_snapshot(model_info(m10))
      expect_snapshot(model_info(m11))
      expect_snapshot(model_info(m12))
      # expect_snapshot(model_info(m14))
      expect_snapshot(model_info(m15))
    })

    test_that("n_parameters", {
      expect_equal(n_parameters(m1), 21)
      expect_equal(n_parameters(m1, effects = "fixed"), 5)
    })

    test_that("get_priors", {
      expect_equal(
        colnames(get_priors(m1)),
        c("Parameter", "Distribution", "Location", "Scale")
      )
      expect_equal(
        colnames(get_priors(m2)),
        c(
          "Parameter",
          "Distribution",
          "Location",
          "Scale",
          "Adjusted_Scale"
        )
      )
      expect_equal(get_priors(m1)$Scale, c(2.5, 2.5, 2.5, 2.5, 2.5), tolerance = 1e-3)
      expect_equal(get_priors(m2)$Adjusted_Scale, c(1.08967, 2.30381, 2.30381, 0.61727, 0.53603, 0.41197), tolerance = 1e-3)
      expect_equal(get_priors(m3)$Adjusted_Scale, c(NA, 2.555042), tolerance = 1e-3)
      expect_equal(get_priors(m4)$Adjusted_Scale, c(6.399801, NA, NA, NA), tolerance = 1e-3)
      expect_equal(get_priors(m5)$Adjusted_Scale, c(6.399801, NA, NA, NA), tolerance = 1e-3)
      expect_equal(
        get_priors(m6),
        data.frame(
          Parameter = "(Intercept)",
          Distribution = "normal",
          Location = 3.057333,
          Scale = 2.5,
          Adjusted_Scale = 1.089666,
          stringsAsFactors = FALSE,
          row.names = NULL
        ),
        tolerance = 1e-3
      )
    })


    test_that("clean_names", {
      expect_identical(
        clean_names(m1),
        c("incidence", "size", "period", "herd")
      )
    })


    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("size", "period")))
      expect_identical(find_predictors(m1, flatten = TRUE), c("size", "period"))
      expect_identical(
        find_predictors(m1, effects = "all", component = "all"),
        list(
          conditional = c("size", "period"),
          random = "herd"
        )
      )
      expect_identical(
        find_predictors(
          m1,
          effects = "all",
          component = "all",
          flatten = TRUE
        ),
        c("size", "period", "herd")
      )
    })

    test_that("find_response", {
      expect_equal(
        find_response(m1, combine = TRUE),
        "cbind(incidence, size - incidence)"
      )
      expect_equal(
        find_response(m1, combine = FALSE),
        c("incidence", "size")
      )
    })

    test_that("get_response", {
      expect_equal(nrow(get_response(m1)), 56)
      expect_equal(colnames(get_response(m1)), c("incidence", "size"))
    })

    test_that("find_random", {
      expect_equal(find_random(m1), list(random = "herd"))
    })

    test_that("get_random", {
      expect_equal(get_random(m1), lme4::cbpp[, "herd", drop = FALSE])
    })

    test_that("find_terms", {
      expect_identical(
        find_terms(m1),
        list(
          response = "cbind(incidence, size - incidence)",
          conditional = c("size", "period"),
          random = "herd"
        )
      )
    })

    test_that("find_variables", {
      expect_identical(
        find_variables(m1),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period"),
          random = "herd"
        )
      )
      expect_identical(
        find_variables(m1, effects = "fixed"),
        list(
          response = c("incidence", "size"),
          conditional = c("size", "period")
        )
      )
      expect_null(find_variables(m1, component = "zi"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 842)
    })

    test_that("find_paramaters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = c("(Intercept)", "size", "period2", "period3", "period4"),
          random = c(sprintf("b[(Intercept) herd:%i]", 1:15), "Sigma[herd:(Intercept),(Intercept)]")
        )
      )
      expect_equal(
        find_parameters(m1, flatten = TRUE),
        c(
          "(Intercept)",
          "size",
          "period2",
          "period3",
          "period4",
          sprintf("b[(Intercept) herd:%i]", 1:15),
          "Sigma[herd:(Intercept),(Intercept)]"
        )
      )
    })

    test_that("find_paramaters", {
      expect_equal(
        colnames(get_parameters(m1)),
        c("(Intercept)", "size", "period2", "period3", "period4")
      )
      expect_equal(
        colnames(get_parameters(m1, effects = "all")),
        c(
          "(Intercept)",
          "size",
          "period2",
          "period3",
          "period4",
          sprintf("b[(Intercept) herd:%i]", 1:15),
          "Sigma[herd:(Intercept),(Intercept)]"
        )
      )
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), plogis(.2), tolerance = 1e-4)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 56)
      expect_equal(
        colnames(get_data(m1)),
        c("incidence", "size", "period", "herd")
      )
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equal(
        find_formula(m1),
        list(
          conditional = as.formula("cbind(incidence, size - incidence) ~ size + period"),
          random = as.formula("~1 | herd")
        ),
        ignore_attr = TRUE
      )
    })

    test_that("get_variance", {
      expect_equal(
        get_variance(m1),
        list(
          var.fixed = 0.36274,
          var.random = 0.03983,
          var.residual = 3.28987,
          var.distribution = 3.28987,
          var.dispersion = 0,
          var.intercept = c(herd = 0.59889)
        ),
        tolerance = 1e-3
      )

      expect_equal(get_variance_fixed(m1),
        c(var.fixed = 0.3627389),
        tolerance = 1e-4
      )
      expect_equal(get_variance_random(m1),
        c(var.random = 0.03983106),
        tolerance = 1e-4
      )
      expect_equal(get_variance_residual(m1),
        c(var.residual = 3.289868),
        tolerance = 1e-4
      )
      expect_equal(get_variance_distribution(m1),
        c(var.distribution = 3.289868),
        tolerance = 1e-4
      )
      expect_equal(get_variance_dispersion(m1),
        c(var.dispersion = 0),
        tolerance = 1e-4
      )
    })

    test_that("find_algorithm", {
      expect_equal(
        find_algorithm(m1),
        list(
          algorithm = "sampling",
          chains = 2,
          iterations = 500,
          warmup = 250
        )
      )
    })

    test_that("clean_parameters", {
      expect_equal(
        clean_parameters(m2),
        structure(
          list(
            Parameter = c(
              "(Intercept)",
              "Speciesversicolor",
              "Speciesvirginica",
              "Petal.Length",
              "Speciesversicolor:Petal.Length",
              "Speciesvirginica:Petal.Length",
              "sigma"
            ),
            Effects = c(
              "fixed", "fixed", "fixed",
              "fixed", "fixed", "fixed", "fixed"
            ),
            Component = c(
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "conditional",
              "sigma"
            ),
            Cleaned_Parameter = c(
              "(Intercept)",
              "Speciesversicolor",
              "Speciesvirginica",
              "Petal.Length",
              "Speciesversicolor:Petal.Length",
              "Speciesvirginica:Petal.Length",
              "sigma"
            )
          ),
          class = c("clean_parameters", "data.frame"),
          row.names = c(NA, -7L)
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_statistic", {
      expect_null(find_statistic(m1))
      expect_null(find_statistic(m2))
      expect_null(find_statistic(m3))
      expect_null(find_statistic(m4))
      expect_null(find_statistic(m5))
      expect_null(find_statistic(m6))
    })

    model <- stan_glm(
      disp ~ carb,
      data = mtcars,
      priors = NULL,
      prior_intercept = NULL,
      refresh = 0
    )

    test_that("flat_priors", {
      p <- get_priors(model)
      expect_equal(p$Distribution, c("uniform", "normal"))
      expect_equal(p$Location, c(NA, 0), tolerance = 1e-3)
    })
  }
}
