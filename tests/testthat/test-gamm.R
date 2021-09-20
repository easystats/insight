.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {
  unloadNamespace("gam")
  if (requiet("testthat") && requiet("insight") && requiet("mgcv")) {
    set.seed(0)
    dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
    m1 <-
      gamm(
        y ~ s(x0) + s(x1) + s(x2),
        family = poisson,
        data = dat,
        random = list(fac = ~1),
        verbosePQL = FALSE
      )

    test_that("model_info", {
      expect_true(model_info(m1)$is_poisson)
      expect_false(model_info(m1)$is_linear)
    })

    test_that("clean_names", {
      expect_equal(clean_names(m1), c("y", "x0", "x1", "x2", "fac"))
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2")))
      expect_identical(
        find_predictors(m1, effects = "all"),
        list(
          conditional = c("x0", "x1", "x2"),
          random = "fac"
        )
      )
      expect_identical(find_predictors(m1, flatten = TRUE), c("x0", "x1", "x2"))
      expect_identical(find_predictors(m1, effects = "random"), list(random = "fac"))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "y")
    })

    test_that("get_response", {
      expect_equal(get_response(m1), dat$y)
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 200)
      expect_equal(colnames(get_data(m1)), c("y", "x0", "x1", "x2", "fac", "g", "g.0", "g.1", "y.0", "Xr.V1", "Xr.V2", "Xr.V3", "Xr.V4", "Xr.V5", "Xr.V6", "Xr.V7", "Xr.V8", "Xr.0.V1", "Xr.0.V2", "Xr.0.V3", "Xr.0.V4", "Xr.0.V5", "Xr.0.V6", "Xr.0.V7", "Xr.0.V8", "Xr.1.V1", "Xr.1.V2", "Xr.1.V3", "Xr.1.V4", "Xr.1.V5", "Xr.1.V6", "Xr.1.V7", "Xr.1.V8", "X.(Intercept)", "X.s(x0)Fx1", "X.s(x1)Fx1", "X.s(x2)Fx1"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 2)
      expect_equal(
        find_formula(m1),
        list(
          conditional = as.formula("y ~ s(x0) + s(x1) + s(x2)"),
          random = as.formula("~1 | fac")
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_terms", {
      expect_equal(find_terms(m1), list(response = "y", conditional = c("s(x0)", "s(x1)", "s(x2)"), random = "fac"))
      expect_equal(find_terms(m1, flatten = TRUE), c("y", "s(x0)", "s(x1)", "s(x2)", "fac"))
    })

    test_that("find_variables", {
      expect_equal(find_variables(m1), list(response = "y", conditional = c("x0", "x1", "x2"), random = "fac"))
      expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2", "fac"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 200)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("find_parameters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = "(Intercept)",
          smooth_terms = c("s(x0)", "s(x1)", "s(x2)")
        )
      )
      expect_equal(nrow(get_parameters(m1)), 4)
      expect_equal(get_parameters(m1)$Parameter, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)"))
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
    })



    # test formula random effects -----------------------

    n <- 200
    sig <- 2
    set.seed(0)
    n.g <- 10
    n <- n.g * 10 * 4
    dat <- gamSim(1, n = n, scale = 2)
    f <- dat$f
    ## simulate nested random effects....
    fa <- as.factor(rep(1:10, rep(4 * n.g, 10)))
    ra <- rep(rnorm(10), rep(4 * n.g, 10))
    fb <- as.factor(rep(rep(1:4, rep(n.g, 4)), 10))
    rb <- rep(rnorm(4), rep(n.g, 4))
    for (i in 1:9) {
      rb <- c(rb, rep(rnorm(4), rep(n.g, 4)))
    }
    ## simulate auto-correlated errors within groups
    e <- array(0, 0)
    for (i in 1:40) {
      eg <- rnorm(n.g, 0, sig)
      for (j in 2:n.g) {
        eg[j] <- eg[j - 1] * 0.6 + eg[j]
      }
      e <- c(e, eg)
    }
    dat$y <- f + ra + rb + e
    dat$fa <- fa
    dat$fb <- fb

    ## fit model ....
    m1 <- gamm(
      y ~ s(x0, bs = "cr") + s(x1, bs = "cr"),
      data = dat,
      random = list(fa = ~1, fb = ~1),
      correlation = corAR1()
    )

    set.seed(0)

    void <- capture.output(
      dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
    )

    m2 <- gamm(
      y ~ s(x0) + s(x1) + s(x2),
      family = poisson,
      data = dat,
      verbosePQL = FALSE
    )

    dat$g <- dat$fac
    m3 <- gamm(
      y ~ s(x0) + s(x1) + s(x2),
      family = poisson,
      data = dat,
      random = list(g = ~1),
      verbosePQL = FALSE
    )

    test_that("find_formula-gamm-1", {
      expect_equal(
        find_formula(m1),
        list(
          conditional = as.formula("y ~ s(x0, bs = \"cr\") + s(x1, bs = \"cr\")"),
          random = list(as.formula("~1 | fa"), as.formula("~1 | fb"))
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_formula-gamm-2", {
      expect_equal(
        find_formula(m2),
        list(conditional = as.formula("y ~ s(x0) + s(x1) + s(x2)")),
        ignore_attr = TRUE
      )
    })

    test_that("find_formula-gamm-3", {
      expect_equal(
        find_formula(m3),
        list(
          conditional = as.formula("y ~ s(x0) + s(x1) + s(x2)"),
          random = as.formula("~1 | g")
        ),
        ignore_attr = TRUE
      )
    })
  }
}
