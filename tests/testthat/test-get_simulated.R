test_that("get_simulated - lm", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)

  out <- get_simulated(model, iterations = 2, seed = 123)
  ref <- stats::simulate(model, nsim = 2, seed = 123)
  names(ref) <- c("iter_1", "iter_2")

  expect_s3_class(out, "data.frame")
  expect_named(out, c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_equal(out, ref, tolerance = 1e-12, ignore_attr = TRUE)
  expect_false(is.null(attributes(out)$seed))

  out <- get_simulated(model, iterations = 2, include_data = TRUE, seed = 123)
  expect_named(out, c("mpg", "cyl", "hp", "iter_1", "iter_2"))
  expect_identical(dim(out), c(32L, 5L))

  out <- get_simulated(
    model,
    iterations = 2,
    data = get_datagrid(model, "cyl"),
    seed = 123
  )
  expect_named(out, c("iter_1", "iter_2"))
  expect_identical(dim(out), c(2L, 3L))

  out <- get_simulated(
    model,
    iterations = 2,
    data = get_datagrid(model, "cyl"),
    include_data = TRUE,
    seed = 123
  )
  expect_named(out, c("cyl", "hp", "iter_1", "iter_2"))
  expect_identical(dim(out), c(4L, 3L))
})


test_that("get_simulated - glm, binomial", {
  data(mtcars)
  model <- glm(vs ~ am + wt, data = mtcars, family = "binomial")

  out <- get_simulated(model, iterations = 2, seed = 123)
  ref <- stats::simulate(model, nsim = 2, seed = 123)
  names(ref) <- c("iter_1", "iter_2")

  expect_s3_class(out, "data.frame")
  expect_named(out, c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_equal(out, ref, tolerance = 1e-12, ignore_attr = TRUE)
  expect_false(is.null(attributes(out)$seed))

  out <- get_simulated(model, iterations = 2, seed = 123, include_data = TRUE)
  expect_named(out, c("vs", "am", "wt", "iter_1", "iter_2"))
  expect_identical(dim(out), c(32L, 5L))

  model <- glm(vs ~ am + wt, data = mtcars, family = "binomial")
  out <- get_simulated(
    model,
    iterations = 5,
    seed = 123,
    data = insight::get_datagrid(model, "am")
  )
  expect_identical(dim(out), c(2L, 5L))
  expect_named(out, c("iter_1", "iter_2", "iter_3", "iter_4", "iter_5"))

  model <- glm(vs ~ am + wt, data = mtcars, family = "binomial")
  out <- get_simulated(
    model,
    iterations = 5,
    include_data = TRUE,
    seed = 123,
    data = insight::get_datagrid(model, "am")
  )
  expect_identical(dim(out), c(2L, 7L))
  expect_named(out, c("am", "wt", "iter_1", "iter_2", "iter_3", "iter_4", "iter_5"))

  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")

  m <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  out <- get_simulated(m)
  expect_named(out, c("iter_1_incidence", "iter_1_size"))
  expect_identical(dim(out), c(56L, 2L))

  out <- get_simulated(m, iterations = 3)
  expect_named(
    out,
    c(
      "iter_1_incidence",
      "iter_1_size",
      "iter_2_incidence",
      "iter_2_size",
      "iter_3_incidence",
      "iter_3_size"
    )
  )
  expect_identical(dim(out), c(56L, 6L))

  expect_error(
    get_simulated(m, data = get_datagrid(m, "period")),
    regex = "Cannot simulate with `prior.weights`",
    fixed = TRUE
  )
})


test_that("get_simulated - data.frame dispatch", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  dg <- get_datagrid(model, wt = c(2, 3), cyl = c(4, 6))

  out <- get_simulated(dg, model, iterations = 3, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("iter_1", "iter_2", "iter_3"))
  expect_identical(nrow(out), nrow(dg))

  out <- get_simulated(dg, model, iterations = 3, seed = 123, include_data = TRUE)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("wt", "cyl", "iter_1", "iter_2", "iter_3"))
  expect_identical(dim(out), c(30L, 5L))
})


test_that("get_simulated - default method errors", {
  pca <- stats::prcomp(USArrests, scale. = TRUE)
  expect_error(get_simulated(pca), "not yet been implemented")
})


test_that("get_simulated - betareg", {
  skip_if_not_installed("betareg")

  data("GasolineYield", package = "betareg")
  model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)

  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(GasolineYield))
  expect_false(is.null(attributes(out)$seed))
})


test_that("get_simulated - glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_on_cran()

  data(mtcars)

  # binomial ---------------------------------
  model <- suppressWarnings(glmmTMB::glmmTMB(
    vs ~ am + (1 | cyl),
    data = mtcars,
    family = "binomial"
  ))

  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("iter_1", "iter_2"))
  expect_false(is.null(attributes(out)$seed))

  # errors --------------------------
  expect_error(
    get_simulated(model, iterations = 2, seed = 123, re.form = NA),
    "Only `re.form = NULL` is",
    fixed = TRUE
  )

  # linear -----------------------------------
  skip_if_not_installed("modelbased")
  skip_if_not_installed("datawizard")
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("e42dep", "c172code"))

  m <- glmmTMB::glmmTMB(neg_c_7 ~ e42dep + c172code + c12hour + (1 | grp), data = efc)

  set.seed(1234)
  out <- get_simulated(m, iterations = 5)
  expect_named(
    out,
    c("iter_1", "iter_2", "iter_3", "iter_4", "iter_5")
  )
  expect_identical(dim(out), c(834L, 5L))

  set.seed(1234)
  out <- get_simulated(m, iterations = 5, include_data = TRUE)
  expect_named(
    out,
    c(
      "neg_c_7",
      "e42dep",
      "c172code",
      "c12hour",
      "grp",
      "iter_1",
      "iter_2",
      "iter_3",
      "iter_4",
      "iter_5"
    )
  )
  expect_identical(dim(out), c(834L, 10L))

  set.seed(1234)
  out <- get_simulated(m, iterations = 5, data = get_datagrid(m, c("e42dep", "c172code")))
  expect_named(
    out,
    c("iter_1", "iter_2", "iter_3", "iter_4", "iter_5")
  )
  expect_identical(dim(out), c(12L, 5L))

  set.seed(1234)
  out <- get_simulated(
    m,
    iterations = 5,
    data = get_datagrid(m, c("e42dep", "c172code")),
    include_data = TRUE
  )
  expect_named(
    out,
    c("e42dep", "c172code", "iter_1", "iter_2", "iter_3", "iter_4", "iter_5")
  )
  expect_identical(dim(out), c(12L, 7L))
  expect_identical(
    as.character(out$e42dep),
    c(
      "independent",
      "slightly dependent",
      "moderately dependent",
      "severely dependent",
      "independent",
      "slightly dependent",
      "moderately dependent",
      "severely dependent",
      "independent",
      "slightly dependent",
      "moderately dependent",
      "severely dependent"
    )
  )

  set.seed(1234)
  out <- get_simulated(
    m,
    iterations = 5,
    data = get_datagrid(m, "c12hour"),
    include_data = TRUE
  )
  expect_named(
    out,
    c("c12hour", "iter_1", "iter_2", "iter_3", "iter_4", "iter_5")
  )
  expect_identical(dim(out), c(2L, 6L))
  expect_equal(out$c12hour, c(4, 168))

  set.seed(1234)
  out <- get_simulated(
    m,
    iterations = 5,
    data = get_datagrid(m, "c12hour")
  )
  expect_named(
    out,
    c("iter_1", "iter_2", "iter_3", "iter_4", "iter_5")
  )
  expect_identical(dim(out), c(2L, 5L))

  # poisson -------------------------------#
  m <- suppressWarnings(glmmTMB::glmmTMB(
    tot_sc_e ~ e42dep + c172code + c12hour + (1 | grp),
    data = efc,
    family = poisson()
  ))
  out <- get_simulated(m, iterations = 5, seed = 1234)
  ref <- simulate(m, nsims = 5, seed = 1234)
  expect_equal(head(out), head(ref), ignore_attr = TRUE)

  out <- get_simulated(m, data = get_datagrid(m, "c172code"), iterations = 5, seed = 1234)
  expect_named(out, c("iter_1", "iter_2", "iter_3", "iter_4", "iter_5"))
  expect_identical(dim(out), c(3L, 5L))
  expect_identical(out$iter_1, c(0, 0, 0))
  expect_identical(out$iter_4, c(0, 1, 1))

  # centrality argument works
  out <- get_simulated(
    m,
    data = get_datagrid(m, "c172code"),
    iterations = 5,
    seed = 1234,
    centrality = mean
  )
  expect_equal(out$iter_1, c(0.7, 0.95059, 1.17308), tolerance = 1e-3)

  out <- get_simulated(
    m,
    data = get_datagrid(m, "c172code"),
    iterations = 5,
    seed = 1234,
    centrality = median
  )
  expect_equal(out$iter_1, c(1, 1, 1), tolerance = 1e-3)

  out <- get_simulated(
    m,
    data = get_datagrid(m, "c172code"),
    iterations = 5,
    seed = 1234,
    include_data = TRUE
  )
  expect_named(out, c("c172code", "iter_1", "iter_2", "iter_3", "iter_4", "iter_5"))
  expect_identical(dim(out), c(3L, 6L))
  expect_identical(out$iter_1, c(0, 0, 0))
  expect_identical(out$iter_4, c(0, 1, 1))
  expect_identical(
    as.character(out$c172code),
    c(
      "low level of education",
      "intermediate level of education",
      "high level of education"
    )
  )

  # zero-inflation --------------------------------
  data(Salamanders, package = "glmmTMB")
  m2 <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    zi = ~ spp + mined,
    family = glmmTMB::nbinom2,
    data = Salamanders
  )

  out <- get_simulated(m2)
  expect_named(out, "iter_1")
  expect_identical(dim(out), c(644L, 1L))

  out <- get_simulated(m2, iterations = 3)
  expect_named(out, c("iter_1", "iter_2", "iter_3"))
  expect_identical(dim(out), c(644L, 3L))

  out <- get_simulated(m2, iterations = 3, include_data = TRUE)
  expect_named(out, c("count", "spp", "mined", "site", "iter_1", "iter_2", "iter_3"))
  expect_identical(dim(out), c(644L, 7L))

  out <- get_simulated(
    m2,
    iterations = 3,
    data = get_datagrid(m2, "spp"),
    include_data = TRUE,
    seed = 1234
  )
  expect_named(out, c("spp", "iter_1", "iter_2", "iter_3"))
  expect_identical(dim(out), c(7L, 4L))
  expect_equal(out$iter_1, c(0, 0, 0, 0, 0, 0, 0))

  out <- get_simulated(
    m2,
    iterations = 3,
    data = get_datagrid(m2, "spp"),
    include_data = TRUE,
    centrality = median,
    seed = 1234
  )
  expect_equal(out$iter_1, c(0, 0, 0, 0, 0, 1, 1))

  out <- get_simulated(
    m2,
    iterations = 3,
    data = get_datagrid(m2, "spp"),
    include_data = TRUE,
    centrality = mean,
    seed = 1234
  )
  expect_equal(
    out$iter_1,
    c(0.95652, 0.25, 1.02174, 0.48913, 1.91304, 2.66304, 1.47826),
    tolerance = 1e-3
  )
})


test_that("get_simulated - merMod", {
  skip_if_not_installed("lme4")

  model <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)
  out <- get_simulated(model, iterations = 2, seed = 123)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("iter_1", "iter_2"))
  expect_identical(nrow(out), nrow(mtcars))
  expect_false(is.null(attributes(out)$seed))

  dg <- get_datagrid(model, am = c(0, 1), cyl = c(4, 6))
  out2 <- get_simulated(
    model,
    data = dg,
    iterations = 2,
    seed = 123,
    allow.new.levels = TRUE
  )
  expect_identical(nrow(out2), nrow(dg))
  expect_identical(names(out2), c("iter_1", "iter_2"))
})
