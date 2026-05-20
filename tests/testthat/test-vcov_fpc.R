skip_on_cran()
skip_if_not_installed("Matrix")
skip_if_not_installed("lme4")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("pbkrtest")

test_that("get_varcov, fpc, mixed", {
  data("sleepstudy", package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  model2 <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = sleepstudy
  )

  vcovFPC_Lai <- function(object, popsize2 = NULL, popsize1 = NULL, KR = FALSE) {
    # Obtained finite-population-adjusted standard errors for fixed effect
    # estimates for a fitted multilevel model
    #
    # Args:
    # object: an R object of class merMod as resulting from lmer()
    # popsize2: population size at level-2; if NULL, an infinite level-2
    # population is assumed
    # popsize1: population size at level-1; if NULL, an infinite level-1
    # population is assumed
    # KR: Whether Kenward-Roger approximation of standard errors should be used,
    # which is recommended for small number of clusters and average cluster size.
    # Default to FALSE.
    #
    # Returns:
    # The variance-covariance matrix of the fixed effect estimates, as
    # returned by vcov()
    if (!inherits(object, "merMod")) {
      stop("Wrong input: Not a fitted model from lmer() with class merMod")
    }
    if (length(object@flist) != 1) {
      stop("Wrong input: Only models with two levels are supported")
    }
    if (is.null(popsize1) & is.null(popsize2)) {
      message("No FPC specified; return results from lme4::vcov.merMod()")
      return(vcov(object))
    }
    PR <- object@pp
    N <- unname(object@devcomp$dims["n"])
    nclus <- unname(lme4::ngrps(object))
    if (isTRUE(popsize2 > nclus)) {
      fpc2 <- 1 - nclus / popsize2
    } else {
      fpc2 <- 1
      message("No FPC needed at level-2")
    }
    if (isTRUE(popsize1 > N)) {
      fpc1 <- 1 - N / popsize1
    } else {
      fpc1 <- 1
      message("No FPC needed at level-1")
    }
    if (fpc1 == 1 & fpc2 == 1) {
      message("Return results from lme4::vcov.merMod()")
      return(vcov(object))
    }
    A <- as.matrix(PR$Lambdat %*% PR$Zt)
    Astar <- A * sqrt(fpc2)
    X <- PR$X
    Astar_X <- Astar %*% X
    D <- as.matrix(Matrix::Diagonal(nrow(Astar), fpc1) + tcrossprod(Astar))
    Fisher_I <- (crossprod(X) - crossprod(solve(t(chol(D)), Astar_X))) / fpc1
    Phi <- solve(Fisher_I) * sigma(object)^2
    Phi <- as(Phi, "dpoMatrix")
    nmsX <- colnames(X)
    dimnames(Phi) <- list(nmsX, nmsX)
    if (!KR) {
      return(Phi)
    } else {
      if (!require("pbkrtest")) {
        stop(
          "Please install the `pbkrtest` package for the use of Kenward-Roger correction!"
        )
      } else {
        SigmaG <- pbkrtest::get_SigmaG(object, details = 0)
        vcov_kr <- pbkrtest:::vcovAdj_internal(Phi, SigmaG, X, details = 0)
        return(as.matrix(vcov_kr))
      }
    }
  }

  out1 <- as.matrix(vcovFPC_Lai(model, popsize2 = 20))
  out2 <- get_varcov(model, vcov = "fpc", vcov_args = list(cluster_size = 20))
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- as.matrix(vcovFPC_Lai(model, popsize2 = 20, popsize1 = 400))
  out2 <- get_varcov(
    model,
    vcov = "fpc",
    vcov_args = list(cluster_size = 20, population_size = 400)
  )
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- as.matrix(vcovFPC_Lai(model, popsize2 = 20, popsize1 = 400, KR = TRUE))
  out2 <- get_varcov(
    model,
    vcov = "fpc",
    vcov_args = list(cluster_size = 20, population_size = 400, kr = TRUE)
  )
  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)

  # errors

  expect_error(
    get_varcov(model, vcov = "fpc"),
    regex = "You must provide either",
    fixed = TRUE
  )

  expect_error(
    get_varcov(
      model,
      vcov = "fpc",
      vcov_args = list(cluster_size = 10)
    ),
    regex = "`cluster_size` must be larger",
    fixed = TRUE
  )

  expect_error(
    get_varcov(
      model,
      vcov = "fpc",
      vcov_args = list(population_size = 10)
    ),
    regex = "`population_size` must be larger",
    fixed = TRUE
  )
})


test_that("get_varcov, fpc, lm", {
  data(iris)
  model <- lm(Sepal.Width ~ Species, data = iris)
  out <- get_varcov(model, vcov = "fpc", vcov_args = list(population_size = 200))

  expect_equal(
    out,
    matrix(
      c(
        0.00057983796533689,
        -0.00057983796533689,
        -0.00057983796533689,
        -0.00057983796533689,
        0.00115967593067378,
        0.000579837965336889,
        -0.00057983796533689,
        0.000579837965336889,
        0.00115967593067378
      ),
      nrow = 3
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  # errors
  expect_error(
    get_varcov(
      model,
      vcov = "fpc",
      vcov_args = list(population_size = 10)
    ),
    regex = "`population_size` must be larger",
    fixed = TRUE
  )

  expect_error(
    get_varcov(model, vcov = "fpc"),
    regex = "You must provide",
    fixed = TRUE
  )

  # works with glmmTMB non-mixed
  model <- glmmTMB::glmmTMB(Sepal.Width ~ Species, data = iris)
  out <- get_varcov(model, vcov = "fpc", vcov_args = list(population_size = 200))
  expect_equal(
    out,
    matrix(
      c(
        0.000568241212496003,
        -0.000568241212496003,
        -0.00056824121249594,
        -0.000568241212496003,
        0.00113648242499201,
        0.00056824121249594,
        -0.00056824121249594,
        0.000568241212495939,
        0.00113648242499175
      ),
      nrow = 3
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  data(Salamanders, package = "glmmTMB")
  model <- glmmTMB::glmmTMB(
    count ~ mined,
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )

  out <- get_varcov(model, vcov = "fpc", vcov_args = list(population_size = 1000))
  expect_equal(
    out,
    matrix(
      c(
        0.00723393879734834,
        -0.00723393879734834,
        -0.00723393879734834,
        0.00775696030558524
      ),
      nrow = 2
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  out <- get_varcov(
    model,
    component = "all",
    vcov = "fpc",
    vcov_args = list(population_size = 1000)
  )
  expect_equal(
    out,
    matrix(
      c(
        0.00723393879734834,
        -0.00723393879734834,
        0.00420667067224415,
        -0.00420667067224437,
        -0.00723393879734834,
        0.00775696030558524,
        -0.00420667067224417,
        0.00435760802439436,
        0.00420667067224415,
        -0.00420667067224417,
        0.012276831894378,
        -0.0122768318943786,
        -0.00420667067224436,
        0.00435760802439436,
        -0.0122768318943786,
        0.0172811732991375
      ),
      nrow = 4
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
