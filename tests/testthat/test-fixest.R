# Currently doesn't work on devel - potential fixest issue?
skip_if(TRUE)

skip_on_os("mac")
skip_if_not_installed("fixest", minimum_version = "0.11.2")
skip_if_not_installed("carData")
skip_if_not_installed("withr")

# avoid warnings
fixest::setFixest_nthreads(1)

data(trade, package = "fixest")
data(Greene, package = "carData")

m1 <- fixest::femlm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade)
m2 <- fixest::femlm(log1p(Euros) ~ log(dist_km) | Origin + Destination + Product, data = trade, family = "gaussian")
m3 <- fixest::feglm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade, family = "poisson")
m4 <- fixest::feols(
  Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width,
  data = iris
)

test_that("robust variance-covariance", {
  mod <- fixest::feols(mpg ~ hp + drat | cyl, data = mtcars)
  # default is clustered
  expect_equal(
    sqrt(diag(vcov(mod))),
    sqrt(diag(get_varcov(mod, vcov = ~cyl))),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )

  # HC1
  expect_equal(
    sqrt(diag(vcov(mod, vcov = "HC1"))),
    sqrt(diag(get_varcov(mod, vcov = "HC1"))),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )

  expect_true(all(
    sqrt(diag(vcov(mod))) !=
      sqrt(diag(get_varcov(mod, vcov = "HC1")))
  ))
})


test_that("offset", {
  tmp <- fixest::feols(mpg ~ hp, offset = ~ log(qsec), data = mtcars)
  expect_identical(find_offset(tmp), "qsec")
  tmp <- fixest::feols(mpg ~ hp, offset = ~qsec, data = mtcars)
  expect_identical(find_offset(tmp), "qsec")
})


test_that("model_info", {
  expect_true(model_info(m1)$is_count)
  expect_true(model_info(m2)$is_linear)
  expect_true(model_info(m3)$is_count)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product")))
  expect_identical(find_predictors(m2), list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product")))
  expect_identical(find_predictors(m3), list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product")))
  expect_identical(find_predictors(m4), list(
    conditional = c("Petal.Length", "Sepal.Length"), cluster = "Species",
    instruments = "Petal.Width", endogenous = "Sepal.Length"
  ))
  expect_identical(
    find_predictors(m1, component = "all"),
    list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_predictors(m2, component = "all"),
    list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_predictors(m3, component = "all"),
    list(conditional = "dist_km", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_predictors(m4, component = "all"),
    list(
      conditional = c("Petal.Length", "Sepal.Length"),
      cluster = "Species",
      instruments = "Petal.Width",
      endogenous = "Sepal.Length"
    )
  )
})

test_that("find_random", {
  expect_null(find_random(m1))
  expect_null(find_random(m2))
  expect_null(find_random(m3))
})

test_that("get_varcov", {
  expect_equal(vcov(m1), get_varcov(m1), tolerance = 1e-3)
  expect_equal(vcov(m4), get_varcov(m4), tolerance = 1e-3)
})

test_that("get_random", {
  expect_warning(expect_null(get_random(m1)))
})

test_that("find_response", {
  expect_identical(find_response(m1), "Euros")
  expect_identical(find_response(m2), "Euros")
  expect_identical(find_response(m3), "Euros")
})

test_that("get_response", {
  expect_equal(get_response(m1), trade$Euros, ignore_attr = TRUE)
  expect_equal(get_response(m2), trade$Euros, ignore_attr = TRUE)
  expect_equal(get_response(m3), trade$Euros, ignore_attr = TRUE)
})

test_that("get_predictors", {
  expect_identical(colnames(get_predictors(m1)), c("dist_km", "Origin", "Destination", "Product"))
  expect_identical(colnames(get_predictors(m2)), c("dist_km", "Origin", "Destination", "Product"))
  expect_identical(colnames(get_predictors(m3)), c("dist_km", "Origin", "Destination", "Product"))
})

test_that("link_inverse", {
  expect_equal(link_inverse(m1)(0.2), exp(0.2), tolerance = 1e-4)
  expect_equal(link_inverse(m2)(0.2), 0.2, tolerance = 1e-4)
  expect_equal(link_inverse(m3)(0.2), exp(0.2), tolerance = 1e-4)
})

test_that("link_function", {
  expect_equal(link_function(m1)(0.2), log(0.2), tolerance = 1e-4)
  expect_equal(link_function(m2)(0.2), 0.2, tolerance = 1e-4)
  expect_equal(link_function(m3)(0.2), log(0.2), tolerance = 1e-4)
})

test_that("get_data", {
  expect_identical(nrow(get_data(m1, verbose = FALSE)), 38325L)
  expect_identical(colnames(get_data(m1, verbose = FALSE)), c("Euros", "dist_km", "Origin", "Destination", "Product"))
  expect_identical(nrow(get_data(m2, verbose = FALSE)), 38325L)
  expect_identical(colnames(get_data(m2, verbose = FALSE)), c("Euros", "dist_km", "Origin", "Destination", "Product"))

  # old bug: m4 uses a complex formula and we need to extract all relevant
  # variables in order to compute predictions.
  nd <- get_data(m4, verbose = FALSE)
  tmp <- predict(m4, newdata = nd)
  expect_type(tmp, "double")
  expect_length(tmp, nrow(iris))
})

skip_if_not_installed("parameters")
test_that("get_df", {
  expect_equal(get_df(m1, type = "residual"), fixest::degrees_freedom(m1, type = "resid"), ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "normal"), Inf, ignore_attr = TRUE)
  ## statistic is t for this model
  expect_equal(get_df(m1, type = "wald"), fixest::degrees_freedom(m1, type = "t"), ignore_attr = TRUE)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(
      conditional = as.formula("Euros ~ log(dist_km)"),
      cluster = as.formula("~Origin + Destination + Product")
    ),
    ignore_attr = TRUE
  )
  expect_length(find_formula(m2), 2)
  expect_equal(
    find_formula(m2),
    list(
      conditional = as.formula("log1p(Euros) ~ log(dist_km)"),
      cluster = as.formula("~Origin + Destination + Product")
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(response = "Euros", conditional = "log(dist_km)", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_terms(m1, flatten = TRUE),
    c("Euros", "log(dist_km)", "Origin", "Destination", "Product")
  )
  expect_identical(
    find_terms(m2),
    list(response = "log1p(Euros)", conditional = "log(dist_km)", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_terms(m2, flatten = TRUE),
    c("log1p(Euros)", "log(dist_km)", "Origin", "Destination", "Product")
  )
})


test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(response = "Euros", conditional = "dist_km", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("Euros", "dist_km", "Origin", "Destination", "Product")
  )
  expect_identical(
    find_variables(m2),
    list(response = "Euros", conditional = "dist_km", cluster = c("Origin", "Destination", "Product"))
  )
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("Euros", "dist_km", "Origin", "Destination", "Product")
  )
})


test_that("n_obs", {
  expect_identical(n_obs(m1), 38325L)
  expect_identical(n_obs(m2), 38325L)
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(conditional = "log(dist_km)")
  )
  expect_equal(
    get_parameters(m1),
    data.frame(
      Parameter = "log(dist_km)",
      Estimate = -1.52774702640008,
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4
  )
  expect_identical(
    find_parameters(m2),
    list(conditional = "log(dist_km)")
  )
  expect_equal(
    get_parameters(m2),
    data.frame(
      Parameter = "log(dist_km)",
      Estimate = -2.16843021944503,
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  # see https://github.com/easystats/parameters/issues/892#issuecomment-1712645841
  # and https://github.com/lrberge/fixest/blob/c14c55917897478d996f80bd3392d2e7355b1f29/R/ESTIMATION_FUNS.R#L2903
  d <- Greene
  d$dv <- as.numeric(Greene$decision == "yes")
  m5 <- fixest::feglm(dv ~ language | judge,
    data = d,
    cluster = "judge", family = "logit"
  )
  expect_identical(find_statistic(m1), "z-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
  expect_identical(find_statistic(m3), "z-statistic")
  expect_identical(find_statistic(m4), "t-statistic")
  expect_identical(find_statistic(m5), "z-statistic")
})

test_that("get_statistic", {
  stat <- get_statistic(m1)
  out <- as.data.frame(summary(m1)$coeftable)
  expect_equal(stat$Statistic, out[, "z value"], tolerance = 1e-3, ignore_attr = TRUE)
  stat <- get_statistic(m2)
  out <- as.data.frame(summary(m2)$coeftable)
  expect_equal(stat$Statistic, out[, "z value"], tolerance = 1e-3, ignore_attr = TRUE)
  stat <- get_statistic(m3)
  out <- as.data.frame(summary(m3)$coeftable)
  expect_equal(stat$Statistic, out[, "z value"], tolerance = 1e-3, ignore_attr = TRUE)

  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  mod <- fixest::fenegbin(count ~ mined, data = Salamanders)
  out <- get_statistic(mod)
  expect_equal(
    out$Statistic,
    summary(mod)$coeftable[1:2, 3],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("get_predicted", {
  pred <- get_predicted(m1)
  expect_s3_class(pred, "get_predicted")
  expect_length(pred, nrow(trade))
  a <- get_predicted(m1)
  b <- get_predicted(m1, type = "response", predict = NULL)
  expect_equal(a, b, tolerance = 1e-5)
  a <- get_predicted(m1, predict = "link")
  b <- get_predicted(m1, type = "link", predict = NULL)
  expect_equal(a, b, tolerance = 1e-5)
  # these used to raise warnings
  expect_warning(get_predicted(m1, ci = 0.4), NA)
  expect_warning(get_predicted(m1, predict = NULL, type = "link"), NA)
})


withr::with_environment(
  new.env(),
  test_that("get_data works when model data has name of reserved words", {
    set.seed(1234)
    rep <- data.frame(Y = runif(100) > 0.5, X = rnorm(100))
    m <- fixest::feglm(Y ~ X, data = rep, family = binomial)
    out <- get_data(m)
    expect_s3_class(out, "data.frame")
    expect_equal(
      head(out),
      data.frame(
        Y = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
        X = c(
          -1.80603125680195, -0.582075924689333, -1.10888962442678,
          -1.01496200949201, -0.162309523556819, 0.563055818994517
        )
      ),
      ignore_attr = TRUE,
      tolerance = 1e-3
    )
  })
)


test_that("find_variables with interaction", {
  data(mtcars)
  mod <- suppressMessages(fixest::feols(mpg ~ 0 | carb | vs:cyl ~ am:cyl, data = mtcars))
  expect_equal(
    find_variables(mod),
    list(
      response = "mpg", conditional = "vs", cluster = "carb",
      instruments = c("am", "cyl"), endogenous = c("vs", "cyl")
    ),
    ignore_attr = TRUE
  )

  # used to produce a warning
  mod <- fixest::feols(mpg ~ 0 | carb | vs:cyl ~ am:cyl, data = mtcars)
  expect_warning(find_variables(mod), NA)
})


test_that("find_predictors with i(f1, i.f2) interaction", {
  data(airquality)
  aq <- airquality
  aq$week <- aq$Day %/% 7 + 1

  mod <- fixest::feols(Ozone ~ i(Month, i.week), aq, notes = FALSE)
  expect_equal(
    find_predictors(mod),
    list(conditional = c("Month", "week")),
    ignore_attr = TRUE
  )
})
