skip_on_os("mac") # error: FreeADFunObject
skip_if_not_installed("TMB")
skip_if_not_installed("glmmTMB")

# fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
# fish$nofish <- as.factor(fish$nofish)
# fish$livebait <- as.factor(fish$livebait)
# fish$camper <- as.factor(fish$camper)

data("fish", package = "insight")
m1 <- glmmTMB::glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + camper + (1 | persons),
  data = fish,
  family = glmmTMB::truncated_poisson()
)

m2 <- glmmTMB::glmmTMB(
  count ~ child + camper + (1 | persons),
  data = fish,
  family = poisson()
)

m3 <- glmmTMB::glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | persons),
  data = fish,
  family = poisson()
)

m4 <- glmmTMB::glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | ID),
  dispformula = ~xb,
  data = fish,
  family = glmmTMB::truncated_poisson()
)

m7 <- suppressWarnings(glmmTMB::glmmTMB(
  count ~ child + camper + (1 + xb | persons),
  ziformula = ~ child + livebait + (1 + zg + nofish | ID),
  dispformula = ~xb,
  data = fish,
  family = glmmTMB::truncated_poisson()
))

data(Salamanders, package = "glmmTMB")
m5 <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  ziformula = ~mined,
  family = poisson,
  data = Salamanders
)

m6 <-
  glmmTMB::glmmTMB(count ~ 1,
    ziformula = ~1,
    family = poisson(),
    data = Salamanders
  )

test_that("find_weights", {
  expect_null(find_weights(m2))
})

test_that("get_weights", {
  expect_null(get_weights(m2))
})

test_that("get_deviance + logLik", {
  skip_on_cran() ## FIXME: check with win-devel
  expect_equal(get_deviance(m2), 1697.449311, tolerance = 1e-3)
  expect_equal(get_loglikelihood(m2), logLik(m2), tolerance = 1e-3, ignore_attr = TRUE)
  expect_identical(get_df(m2, type = "model"), 4L)
})

test_that("get_df", {
  expect_equal(
    get_df(m2, type = "residual"),
    df.residual(m2),
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m2, type = "normal"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m2, type = "wald"),
    Inf,
    ignore_attr = TRUE
  )
  expect_equal(
    get_df(m2, type = "ml1"),
    c(`(Intercept)` = 247, child = 247, camper1 = 247),
    ignore_attr = TRUE
  )
})

test_that("model_info", {
  expect_true(model_info(m1)$is_zero_inflated)
  expect_false(model_info(m2)$is_zero_inflated)
  expect_true(model_info(m3)$is_count)
  expect_true(model_info(m3)$is_pois)
  expect_false(model_info(m3)$is_negbin)
  expect_true(model_info(m6)$is_count)
  expect_false(model_info(m1)$is_linear)
})

test_that("clean_names", {
  expect_identical(clean_names(m1), c("count", "child", "camper", "persons"))
  expect_identical(clean_names(m2), c("count", "child", "camper", "persons"))
  expect_identical(
    clean_names(m3),
    c("count", "child", "camper", "persons", "livebait")
  )
  expect_identical(
    clean_names(m4),
    c(
      "count",
      "child",
      "camper",
      "persons",
      "livebait",
      "ID",
      "xb"
    )
  )
  expect_identical(clean_names(m6), "count")
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m1, effects = "all"),
    list(
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "camper"),
      zero_inflated_random = "persons"
    )
  )
  expect_identical(
    find_predictors(m1, effects = "all", flatten = TRUE),
    c("child", "camper", "persons")
  )
  expect_identical(
    find_predictors(m1, effects = "random"),
    list(random = "persons", zero_inflated_random = "persons")
  )
  expect_identical(
    find_predictors(m1, effects = "random", flatten = TRUE),
    "persons"
  )
  expect_identical(
    find_predictors(m1, effects = "random", component = "conditional"),
    list(random = "persons")
  )
  expect_identical(
    find_predictors(
      m1,
      effects = "random",
      component = "conditional",
      flatten = TRUE
    ),
    "persons"
  )
  expect_identical(
    find_predictors(m1),
    list(
      conditional = c("child", "camper"),
      zero_inflated = c("child", "camper")
    )
  )
  expect_identical(find_predictors(m1, flatten = TRUE), c("child", "camper"))

  expect_identical(
    find_predictors(m2, effects = "all"),
    list(
      conditional = c("child", "camper"),
      random = "persons"
    )
  )
  expect_identical(
    find_predictors(m2, effects = "all", flatten = TRUE),
    c("child", "camper", "persons")
  )
  expect_identical(
    find_predictors(m2, effects = "random"),
    list(random = "persons")
  )
  expect_identical(
    find_predictors(m2, effects = "random", flatten = TRUE),
    "persons"
  )
  expect_identical(find_predictors(m2), list(conditional = c("child", "camper")))

  expect_null(find_predictors(m6))
})

test_that("find_response", {
  expect_identical(find_response(m1), "count")
  expect_identical(find_response(m2), "count")
  expect_identical(find_response(m6), "count")
})

test_that("link_inverse", {
  expect_identical(link_inverse(m1)(0.2), exp(0.2))
  expect_identical(link_inverse(m2)(0.2), exp(0.2))
})

test_that("get_data", {
  expect_identical(
    colnames(get_data(m1)),
    c("count", "child", "camper", "persons")
  )
  expect_identical(
    colnames(get_data(m1, effects = "all")),
    c("count", "child", "camper", "persons")
  )
  expect_identical(colnames(get_data(m1, effects = "random")), "persons")
  expect_identical(
    colnames(get_data(m2)),
    c("count", "child", "camper", "persons")
  )
  expect_identical(
    colnames(get_data(m2, effects = "all")),
    c("count", "child", "camper", "persons")
  )
  expect_identical(colnames(get_data(m2, effects = "random", verbose = FALSE)), "persons")
  get_data(m3)
  expect_identical(colnames(get_data(m6, verbose = FALSE)), "count")
  expect_null(get_data(m6, effects = "random", verbose = FALSE))
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m3, effects = "fixed", component = "conditional"),
    list(conditional = c("child", "camper"))
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    c("child", "camper")
  )
  expect_identical(
    find_predictors(m3, effects = "fixed", component = "zero_inflated"),
    list(zero_inflated = c("child", "livebait"))
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "fixed",
      component = "zero_inflated",
      flatten = TRUE
    ),
    c("child", "livebait")
  )
  expect_identical(
    find_predictors(m3, effects = "all", component = "conditional"),
    list(
      conditional = c("child", "camper"),
      random = "persons"
    )
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "all",
      component = "conditional",
      flatten = TRUE
    ),
    c("child", "camper", "persons")
  )
  expect_identical(
    find_predictors(m3, effects = "all", component = "zero_inflated"),
    list(
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "persons"
    )
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "all",
      component = "zero_inflated",
      flatten = TRUE
    ),
    c("child", "livebait", "persons")
  )
  expect_identical(
    find_predictors(m3, effects = "random", component = "conditional"),
    list(random = "persons")
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "random",
      component = "conditional",
      flatten = TRUE
    ),
    "persons"
  )
  expect_identical(
    find_predictors(m3, effects = "random", component = "zero_inflated"),
    list(zero_inflated_random = "persons")
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "random",
      component = "zero_inflated",
      flatten = TRUE
    ),
    "persons"
  )

  expect_identical(
    find_predictors(m3, effects = "fixed", component = "all"),
    list(
      conditional = c("child", "camper"),
      zero_inflated = c("child", "livebait")
    )
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "fixed",
      component = "all",
      flatten = TRUE
    ),
    c("child", "camper", "livebait")
  )
  expect_identical(
    find_predictors(m3, effects = "all", component = "all"),
    list(
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "persons"
    )
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "all",
      component = "all",
      flatten = TRUE
    ),
    c("child", "camper", "persons", "livebait")
  )
  expect_identical(
    find_predictors(m3, effects = "random", component = "all"),
    list(random = "persons", zero_inflated_random = "persons")
  )
  expect_identical(
    find_predictors(
      m3,
      effects = "random",
      component = "all",
      flatten = TRUE
    ),
    "persons"
  )

  expect_null(find_predictors(
    m6,
    effects = "random",
    component = "all",
    flatten = TRUE
  ))
})

test_that("find_formula", {
  expect_length(find_formula(m4), 5)
  expect_equal(
    find_formula(m4),
    list(
      conditional = as.formula("count ~ child + camper"),
      random = as.formula("~1 | persons"),
      zero_inflated = as.formula("~child + livebait"),
      zero_inflated_random = as.formula("~1 | ID"),
      dispersion = as.formula("~xb")
    ),
    ignore_attr = TRUE
  )
  expect_equal(find_formula(m6), list(conditional = as.formula("count ~ 1")), ignore_attr = TRUE)
})

test_that("find_predictors", {
  expect_identical(
    find_predictors(m4),
    list(
      conditional = c("child", "camper"),
      zero_inflated = c("child", "livebait"),
      dispersion = "xb"
    )
  )
  expect_identical(
    find_predictors(m4, flatten = TRUE),
    c("child", "camper", "livebait", "xb")
  )
  expect_identical(
    find_predictors(m4, effects = "random"),
    list(random = "persons", zero_inflated_random = "ID")
  )
  expect_identical(
    find_predictors(m4, effects = "all", flatten = TRUE),
    c("child", "camper", "persons", "livebait", "ID", "xb")
  )
  expect_identical(
    find_predictors(m4, effects = "all"),
    list(
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "ID",
      dispersion = "xb"
    )
  )
  expect_identical(
    find_predictors(m4, component = "conditional", flatten = TRUE),
    c("child", "camper")
  )
  expect_identical(
    find_predictors(m4, component = "conditional", flatten = FALSE),
    list(conditional = c("child", "camper"))
  )
  expect_identical(
    find_predictors(m4, effects = "random", component = "conditional"),
    list(random = "persons")
  )
  expect_identical(
    find_predictors(m4, effects = "all", component = "conditional"),
    list(
      conditional = c("child", "camper"),
      random = "persons"
    )
  )
  expect_identical(
    find_predictors(m4, component = "zero_inflated"),
    list(zero_inflated = c("child", "livebait"))
  )
  expect_identical(
    find_predictors(m4, effects = "random", component = "zero_inflated"),
    list(zero_inflated_random = "ID")
  )
  expect_identical(
    find_predictors(
      m4,
      effects = "all",
      component = "zero_inflated",
      flatten = TRUE
    ),
    c("child", "livebait", "ID")
  )
  expect_identical(
    find_predictors(m4, component = "dispersion"),
    list(dispersion = "xb")
  )
  expect_identical(
    find_predictors(m4, component = "dispersion", flatten = TRUE),
    "xb"
  )
  expect_null(find_predictors(m4, effects = "random", component = "dispersion"))
  expect_identical(
    find_predictors(m4, effects = "all", component = "dispersion"),
    list(dispersion = "xb")
  )
  expect_identical(
    find_predictors(
      m4,
      effects = "all",
      component = "dispersion",
      flatten = TRUE
    ),
    "xb"
  )
})

test_that("find_random", {
  expect_identical(
    find_random(m4),
    list(random = "persons", zero_inflated_random = "ID")
  )
  expect_identical(find_random(m4, flatten = TRUE), c("persons", "ID"))
  expect_null(find_random(m6, flatten = TRUE))
})

test_that("find_respone", {
  expect_identical(find_response(m4), "count")
  expect_identical(find_response(m6), "count")
})

test_that("find_terms", {
  expect_identical(
    find_terms(m4),
    list(
      response = "count",
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "ID",
      dispersion = "xb"
    )
  )
  expect_identical(
    find_terms(m4, flatten = TRUE),
    c(
      "count",
      "child",
      "camper",
      "persons",
      "livebait",
      "ID",
      "xb"
    )
  )
  expect_identical(find_terms(m6), list(response = "count", conditional = "1"))
  expect_identical(find_terms(m6, flatten = TRUE), c("count", "1"))
})

test_that("find_variables", {
  expect_identical(
    find_variables(m4),
    list(
      response = "count",
      conditional = c("child", "camper"),
      random = "persons",
      zero_inflated = c("child", "livebait"),
      zero_inflated_random = "ID",
      dispersion = "xb"
    )
  )
  expect_identical(
    find_variables(m4, flatten = TRUE),
    c(
      "count",
      "child",
      "camper",
      "persons",
      "livebait",
      "ID",
      "xb"
    )
  )
  expect_identical(find_variables(m6), list(response = "count"))
  expect_identical(find_variables(m6, flatten = TRUE), "count")
})

test_that("get_response", {
  expect_identical(get_response(m4), fish$count)
  expect_identical(get_response(m6), Salamanders$count)
})

test_that("get_predictors", {
  expect_identical(
    colnames(get_predictors(m4)),
    c("child", "camper", "livebait", "xb")
  )
  expect_null(get_predictors(m6, verbose = FALSE))
})

test_that("get_random", {
  expect_identical(colnames(get_random(m4)), c("persons", "ID"))
  expect_warning(expect_null(get_random(m6)))
})

test_that("get_data", {
  expect_identical(
    colnames(get_data(m4)),
    c(
      "count",
      "child",
      "camper",
      "persons",
      "livebait",
      "ID",
      "xb"
    )
  )
  expect_named(
    get_data(m4, effects = "fixed"),
    c("count", "child", "camper", "livebait", "xb")
  )
  expect_identical(colnames(get_data(m4, effects = "random")), c("persons", "ID"))
  expect_identical(colnames(get_data(m4, component = "zi")), c("child", "livebait", "ID", "count"))
  expect_identical(colnames(get_data(
    m4,
    component = "zi", effects = "fixed"
  )), c("child", "livebait", "count"))
  expect_named(
    get_data(m4, component = "zi", effects = "random", verbose = FALSE),
    "ID"
  )
  expect_named(
    get_data(m4, component = "cond", verbose = FALSE),
    c("count", "child", "camper", "persons")
  )
  expect_named(
    get_data(m4, component = "cond", effects = "fixed", verbose = FALSE),
    c("count", "child", "camper")
  )
  expect_named(
    get_data(m4, component = "cond", effects = "random", verbose = FALSE),
    "persons"
  )
  expect_identical(colnames(get_data(m4, component = "disp")), c("xb", "count"))
  expect_identical(colnames(get_data(
    m4,
    component = "disp", effects = "fixed"
  )), c("xb", "count"))
  expect_null(get_data(m4, component = "disp", effects = "random", verbose = FALSE))
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m4),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      random = list(persons = "(Intercept)"),
      zero_inflated = c("(Intercept)", "child", "livebait1"),
      zero_inflated_random = list(ID = "(Intercept)")
    )
  )

  expect_identical(
    find_parameters(m4, flatten = TRUE),
    c("(Intercept)", "child", "camper1", "livebait1")
  )
  expect_identical(
    find_parameters(m6),
    list(
      conditional = "(Intercept)",
      zero_inflated = "(Intercept)"
    )
  )

  expect_identical(
    find_parameters(m3),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      random = list(persons = "(Intercept)"),
      zero_inflated = c("(Intercept)", "child", "livebait1"),
      zero_inflated_random = list(persons = "(Intercept)")
    )
  )

  expect_identical(
    find_parameters(m3),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      random = list(persons = "(Intercept)"),
      zero_inflated = c("(Intercept)", "child", "livebait1"),
      zero_inflated_random = list(persons = "(Intercept)")
    )
  )

  expect_identical(
    find_parameters(m3, effects = "fixed"),
    list(
      conditional = c("(Intercept)", "child", "camper1"),
      zero_inflated = c("(Intercept)", "child", "livebait1")
    )
  )

  expect_identical(
    find_parameters(m3, effects = "random", component = "zi"),
    list(zero_inflated_random = list(persons = "(Intercept)"))
  )

  expect_identical(
    find_parameters(
      m3,
      effects = "fixed",
      component = "zi",
      flatten = TRUE
    ),
    c("(Intercept)", "child", "livebait1")
  )
})


test_that("get_parameters", {
  expect_identical(nrow(get_parameters(m4)), 6L)
  expect_identical(
    colnames(get_parameters(m4)),
    c("Parameter", "Estimate", "Component")
  )
  expect_identical(
    get_parameters(m4)$Parameter,
    c(
      "(Intercept)",
      "child",
      "camper1",
      "(Intercept)",
      "child",
      "livebait1"
    )
  )
  expect_identical(
    get_parameters(m4)$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "zero_inflated",
      "zero_inflated",
      "zero_inflated"
    )
  )
  expect_identical(
    get_parameters(m6)$Parameter,
    c("(Intercept)", "(Intercept)")
  )

  expect_identical(
    get_parameters(m2)$Parameter,
    c("(Intercept)", "child", "camper1")
  )

  expect_identical(
    get_parameters(m2, component = "all")$Parameter,
    c("(Intercept)", "child", "camper1")
  )

  expect_null(get_parameters(m2, component = "zi"))
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
  expect_false(is.null(link_function(m2)))
  expect_false(is.null(link_function(m3)))
  expect_false(is.null(link_function(m4)))
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
  expect_false(is_multivariate(m2))
  expect_false(is_multivariate(m3))
  expect_false(is_multivariate(m4))
})

data(Salamanders, package = "glmmTMB")
mpred <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  zi = ~mined,
  family = poisson, data = Salamanders
)

test_that("get_predicted with new levels", {
  skip_on_cran() ## FIXME: check with win-devel
  pr <- get_predicted(mpred, data = head(Salamanders), allow.new.levels = TRUE)
  expect_equal(as.vector(pr), c(0.252, 0.39207, 0.21119, 2.20128, 2.39424, 2.28901), tolerance = 1e-3)
})

# test_that("get_variance", {
#
#   expect_warning(expect_equal(get_variance(m5), list(
#     var.fixed = 0.32588694431268194762,
#     var.random = 0.07842738279575413307,
#     var.residual = 0.41218000030914692111,
#     var.distribution = 0.41218000030914692111,
#     var.dispersion = 0,
#     var.intercept = c(site = 0.07842738279575474369)
#   ),
#   tolerance = 1e-3))
#
#   expect_warning(expect_equal(get_variance_fixed(m1), c(var.fixed = 1.09712435712435052437), tolerance = 1e-3))
#   expect_warning(expect_equal(get_variance_random(m1), c(var.random = 0.86712737445492238386), tolerance = 1e-3))
#   expect_warning(expect_equal(get_variance_residual(m1), c(var.residual = 0.02634500773355940087 ), tolerance = 1e-3))
#   expect_warning(expect_equal(get_variance_distribution(m1), c(var.distribution = 0.02634500773355940087 ), tolerance = 1e-3))
#   expect_warning(expect_equal(get_variance_dispersion(m1), c(var.dispersion = 0), tolerance = 1e-3))
# })

test_that("find_algorithm", {
  expect_identical(
    find_algorithm(m1),
    list(algorithm = "ML", optimizer = "nlminb")
  )
})

test_that("find_random_slopes", {
  skip_on_cran()


  expect_null(find_random_slopes(m6))

  expect_identical(
    find_random_slopes(m7),
    list(
      random = "xb",
      zero_inflated_random = c("zg", "nofish")
    )
  )
})

test_that("clean_parameters", {
  expect_identical(
    clean_parameters(m1),
    structure(
      list(
        Parameter = c(
          "(Intercept)",
          "child",
          "camper1",
          "(Intercept)",
          "(Intercept)",
          "child",
          "camper1",
          "(Intercept)"
        ),
        Effects = c(
          "fixed",
          "fixed",
          "fixed",
          "random",
          "fixed",
          "fixed",
          "fixed",
          "random"
        ),
        Component = c(
          "conditional",
          "conditional",
          "conditional",
          "conditional",
          "zero_inflated",
          "zero_inflated",
          "zero_inflated",
          "zero_inflated"
        ),
        Group = c("", "", "", "persons", "", "", "", "persons"),
        Cleaned_Parameter = c(
          "(Intercept)",
          "child",
          "camper1",
          "(Intercept)",
          "(Intercept)",
          "child",
          "camper1",
          "(Intercept)"
        )
      ),
      class = c("clean_parameters", "data.frame"),
      row.names = c(NA, -8L)
    )
  )
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "z-statistic")
  expect_identical(find_statistic(m3), "z-statistic")
  expect_identical(find_statistic(m4), "z-statistic")
  expect_identical(find_statistic(m5), "z-statistic")
  expect_identical(find_statistic(m6), "z-statistic")
  expect_identical(find_statistic(m7), "z-statistic")
})


# dispersion model, example from ?glmmTMB
sim1 <- function(nfac = 40, nt = 100, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
  dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd = tsd)[dat$t]
  dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
  dat
}
set.seed(101)
d1 <- sim1(mu = 100, residsd = 10)
d2 <- sim1(mu = 200, residsd = 5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)
m0 <- glmmTMB::glmmTMB(x ~ sd + (1 | t), dispformula = ~sd, data = dat)


test_that("get_parameters", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.9")
  expect_identical(nrow(get_parameters(m0)), 4L)
  expect_identical(
    colnames(get_parameters(m0)),
    c("Parameter", "Estimate", "Component")
  )
  expect_identical(
    get_parameters(m0)$Parameter,
    c(
      "(Intercept)",
      "sdten",
      "(Intercept)",
      "sdten"
    )
  )
  expect_equal(
    get_parameters(m0)$Estimate,
    c(200.03431, -99.71491, 1.6014, 0.69323),
    tolerance = 1e-3
  )
  expect_identical(
    get_parameters(m0)$Component,
    c("conditional", "conditional", "dispersion", "dispersion")
  )
})

skip_if_not(packageVersion("glmmTMB") > "1.1.4")

test_that("get_predicted", {
  # response
  x <- get_predicted(m1, predict = "expectation", verbose = FALSE, include_random = TRUE)
  y <- get_predicted(m1, predict = "response", include_random = TRUE)
  z <- predict(m1, type = "response")
  expect_equal(x, y, ignore_attr = TRUE)
  expect_equal(x, z, ignore_attr = TRUE)
  expect_equal(y, z, ignore_attr = TRUE)
  get_predicted(m1, predict = NULL, type = "response")

  # should be the same, when include_random = "default"
  x <- get_predicted(m1, predict = "expectation", verbose = FALSE)
  y <- get_predicted(m1, predict = "response")
  z <- predict(m1, type = "response")
  expect_equal(x, y, ignore_attr = TRUE)
  expect_equal(x, z, ignore_attr = TRUE)
  expect_equal(y, z, ignore_attr = TRUE)


  # link
  x <- get_predicted(m1, predict = "link", include_random = TRUE)
  y <- get_predicted(m1, predict = NULL, type = "link", include_random = TRUE)
  z <- predict(m1, type = "link")
  expect_equal(x, y, ignore_attr = TRUE)
  expect_equal(y, z, ignore_attr = TRUE)
  expect_equal(x, z, ignore_attr = TRUE)

  # unsupported: zprob
  x <- suppressWarnings(get_predicted(m1, predict = "zprob", include_random = TRUE))
  y <- get_predicted(m1, predict = NULL, type = "zprob", include_random = TRUE)
  z <- predict(m1, type = "zprob")
  expect_identical(x, y)
  expect_equal(x, z, ignore_attr = TRUE)

  ## TODO

  # not official supported raise warning
  # expect_warning(get_predicted(m1, predict = "zprob"))
  # expect_warning(get_predicted(m1, predict = "zprob", verbose = FALSE), NA)
  # the second warning is raised for zero-inflation models only. remove when
  # the zprob correction is implemented
  expect_warning(get_predicted(m1, predict = "prediction"))
  expect_warning(get_predicted(m1, predict = "classification"))
})


test_that("model_info, ordered beta", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.8")
  skip_if_not_installed("datawizard")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
  m <- glmmTMB::glmmTMB(
    y ~ Days + (Days | Subject),
    data = sleepstudy,
    family = glmmTMB::ordbeta()
  )
  out <- model_info(m)
  expect_true(out$is_orderedbeta)
  expect_identical(out$family, "ordbeta")
})


test_that("model_info, recognize ZI even without ziformula", {
  data("fish", package = "insight")
  fish$count <- fish$count + 1
  m1 <- glmmTMB::glmmTMB(
    count ~ child + camper + (1 | persons),
    data = fish,
    family = glmmTMB::truncated_nbinom1()
  )
  out <- model_info(m1)
  expect_true(out$is_zero_inflated)
  expect_true(out$is_hurdle)
})

## FIXME: test doesn't work on GitHuba

# skip_if_not_installed("withr")

# withr::with_environment(
#   new.env(),
#   test_that("get_variance, ordered beta", {
#     skip_if_not_installed("glmmTMB", minimum_version = "1.1.8")
#     skip_if_not_installed("datawizard")
#     skip_if_not_installed("lme4")
#     skip_on_cran()
#     data(sleepstudy, package = "lme4")
#     sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
#     m <- glmmTMB::glmmTMB(
#       y ~ Days + (Days | Subject),
#       data = sleepstudy,
#       family = glmmTMB::ordbeta()
#     )
#     out <- get_variance(m)
#     expect_equal(out$var.distribution, 1.44250604187634, tolerance = 1e-4)
#   })
# )

test_that("get/find_parameters with dispersion-random", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.10")
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + cover + mined + (1 | site),
    ziformula = ~ spp + mined,
    dispformula = ~ DOY + (1 | site),
    data = Salamanders,
    family = glmmTMB::nbinom2
  )
  out <- get_parameters(m)
  expect_identical(nrow(out), 19L)
  out <- get_parameters(m, effects = "random")
  expect_length(out, 2)
  expect_named(out, c("random", "dispersion_random"))

  expect_equal(
    find_parameters(m),
    list(
      conditional = c(
        "(Intercept)", "sppPR", "sppDM", "sppEC-A",
        "sppEC-L", "sppDES-L", "sppDF", "cover", "minedno"
      ),
      random = list(site = "(Intercept)"),
      zero_inflated = c(
        "(Intercept)", "sppPR",
        "sppDM", "sppEC-A", "sppEC-L", "sppDES-L", "sppDF", "minedno"
      ),
      dispersion = c("(Intercept)", "DOY"),
      dispersion_random = "site"
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_parameters(m, effects = "fixed"),
    list(
      conditional = c(
        "(Intercept)", "sppPR", "sppDM", "sppEC-A",
        "sppEC-L", "sppDES-L", "sppDF", "cover", "minedno"
      ),
      zero_inflated = c(
        "(Intercept)", "sppPR",
        "sppDM", "sppEC-A", "sppEC-L", "sppDES-L", "sppDF", "minedno"
      ),
      dispersion = c("(Intercept)", "DOY")
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_parameters(m, effects = "random"),
    list(
      random = list(site = "(Intercept)"),
      dispersion_random = "site"
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    find_parameters(m, component = "conditional"),
    list(
      conditional = c(
        "(Intercept)", "sppPR", "sppDM", "sppEC-A",
        "sppEC-L", "sppDES-L", "sppDF", "cover", "minedno"
      ),
      random = list(site = "(Intercept)")
    ),
    ignore_attr = TRUE
  )

  # formula and find random works
  out <- find_formula(m)
  expect_equal(
    out,
    list(
      conditional = count ~ spp + cover + mined,
      random = ~ 1 | site,
      zero_inflated = ~ spp + mined,
      dispersion = ~DOY,
      dispersion_random = ~ 1 | site
    ),
    ignore_attr = TRUE
  )
  out <- find_random(m)
  expect_identical(out, list(random = "site", dispersion_random = "site"))

  skip_on_cran()

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + cover + mined + (1 | site),
    ziformula = ~ spp + mined,
    dispformula = ~ DOY + (1 + Wtemp | site),
    data = Salamanders,
    family = poisson()
  )
  out <- find_random_slopes(m)
  expect_identical(out, list(dispersion_random = "Wtemp"))
})
