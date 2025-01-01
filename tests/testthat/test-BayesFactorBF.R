skip_on_os("linux")
skip_if_not_installed("BayesFactor")

x <- BayesFactor::correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})
test_that("find_formula", {
  expect_null(find_formula(x))
})
test_that("get_parameters", {
  expect_identical(nrow(get_parameters(x)), 4000L)
})
mi <- insight::model_info(x)
test_that("model_info-BF", {
  expect_false(mi$is_binomial)
  expect_true(mi$is_linear)
  expect_true(mi$is_correlation)
  expect_false(mi$is_ttest)
})


set.seed(123)
x <- rnorm(1000, 0, 1)
y <- rnorm(1000, 0, 1)

t1 <- suppressMessages(BayesFactor::ttestBF(x = x, mu = 60))
t2 <- BayesFactor::ttestBF(x = x, y = y)
t2d <- suppressMessages(BayesFactor::ttestBF(x = x, y = y, paired = TRUE, mu = 60))

test_that("get_data", {
  expect_s3_class(get_data(t1), "data.frame")
  expect_s3_class(get_data(t2), "data.frame")
  expect_s3_class(get_data(t2d), "data.frame")
})
test_that("find_formula", {
  expect_equal(find_formula(t1), list(conditional = y ~ 1), ignore_attr = TRUE)
  expect_equal(find_formula(t2), list(conditional = y ~ group), ignore_attr = TRUE)
  expect_equal(find_formula(t2d), list(conditional = y ~ 1), ignore_attr = TRUE)
})
test_that("get_parameters", {
  expect_identical(nrow(get_parameters(t1)), 4000L)
  expect_identical(nrow(get_parameters(t2)), 4000L)
  expect_identical(nrow(get_parameters(t2d)), 4000L)

  expect_equal(median(get_parameters(t1)[["Difference"]]), -60, tolerance = 0.05)
  expect_equal(median(get_parameters(t2)[["Difference"]]), 0, tolerance = 0.05)
  expect_equal(median(get_parameters(t2d)[["Difference"]]), -60, tolerance = 0.05)
})
test_that("model_info", {
  expect_true(model_info(t1)$is_ttest)
  expect_true(model_info(t2)$is_ttest)
  expect_true(model_info(t2d)$is_ttest)
})
test_that("get_priors", {
  expect_identical(nrow(get_priors(t1)), 1L)
  expect_identical(nrow(get_priors(t2)), 1L)
  expect_identical(nrow(get_priors(t2d)), 1L)
})
test_that("find_parameters", {
  expect_identical(nrow(get_parameters(t1)), 4000L)
  expect_identical(nrow(get_parameters(t2)), 4000L)
  expect_identical(nrow(get_parameters(t2d)), 4000L)

  expect_identical(find_parameters(t1)[[1]], "Difference")
  expect_identical(find_parameters(t2)[[1]], "Difference")
  expect_identical(find_parameters(t2d)[[1]], "Difference")
})

t <- c(-0.15, 2.39, 2.42, 2.43)
N <- c(100, 150, 97, 99)
x <- BayesFactor::meta.ttestBF(t = t, n1 = N, rscale = 1)
test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})
test_that("find_formula", {
  expect_null(find_formula(x))
})
test_that("get_parameters", {
  expect_identical(nrow(get_parameters(x)), 4000L)
})


data(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose)
levels(ToothGrowth$dose) <- c("Low", "Medium", "High")
x <- BayesFactor::anovaBF(len ~ supp * dose, data = ToothGrowth, progress = FALSE)

test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})

test_that("find_formula", {
  expect_equal(find_formula(x),
    list(conditional = as.formula("len ~ supp + dose + supp:dose")),
    ignore_attr = TRUE
  )
})

test_that("get_parameters", {
  expect_named(
    get_parameters(x, verbose = FALSE),
    c("mu", "supp-OJ", "supp-VC", "sig2", "g_supp")
  )
})

test_that("clean_parameters", {
  cp <- clean_parameters(x)
  expect_identical(
    cp$Cleaned_Parameter,
    c(
      "supp [OJ]", "supp [VC]", "dose [Low]", "dose [Medium]",
      "dose [High]", "mu", "sig2", "g_supp"
    )
  )
  expect_identical(
    cp$Component,
    c(
      "conditional", "conditional", "conditional", "conditional",
      "conditional", "extra", "extra", "extra"
    )
  )
})


data(puzzles, package = "BayesFactor")
x <- BayesFactor::anovaBF(RT ~ shape * color + ID, data = puzzles, whichRandom = "ID", progress = FALSE)

test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})

test_that("find_formula", {
  expect_equal(
    find_formula(x),
    list(
      conditional = as.formula("RT ~ shape + color + shape:color"),
      random = as.formula("~ID")
    ),
    ignore_attr = TRUE
  )
})

test_that("get_parameters", {
  expect_named(
    get_parameters(x, verbose = FALSE),
    c(
      "mu", "shape-round", "shape-square", "ID-1", "ID-2", "ID-3",
      "ID-4", "ID-5", "ID-6", "ID-7", "ID-8", "ID-9", "ID-10", "ID-11",
      "ID-12", "sig2", "g_shape", "g_ID"
    )
  )
})

test_that("get_parameters", {
  expect_identical(
    find_parameters(x[4]),
    list(
      conditional = c(
        "shape-round", "shape-square", "color-color", "color-monochromatic",
        "shape:color-round.&.color", "shape:color-round.&.monochromatic",
        "shape:color-square.&.color", "shape:color-square.&.monochromatic"
      ),
      random = c(
        "ID-1", "ID-2", "ID-3", "ID-4", "ID-5", "ID-6", "ID-7",
        "ID-8", "ID-9", "ID-10", "ID-11", "ID-12"
      ),
      extra = c("mu", "sig2", "g_shape", "g_color", "g_ID", "g_shape:color")
    )
  )
})

test_that("find_response", {
  expect_identical(find_response(x), "RT")
})

test_that("find_random", {
  expect_identical(find_random(x), list(random = "ID"))
})

test_that("find_variables", {
  expect_identical(
    find_variables(x),
    list(
      response = "RT",
      conditional = c("shape", "color"),
      random = "ID"
    )
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(x),
    list(
      response = "RT",
      conditional = c("shape", "color"),
      random = "ID"
    )
  )
})

test_that("get_priors", {
  expect_equal(
    get_priors(x),
    data.frame(
      Parameter = c(
        "shape-round", "shape-square", "color-color",
        "color-monochromatic", "ID-1", "ID-2", "ID-3", "ID-4", "ID-5",
        "ID-6", "ID-7", "ID-8", "ID-9", "ID-10", "ID-11", "ID-12", "mu",
        "sig2", "g_shape", "g_ID"
      ),
      Distribution = c(
        "cauchy", "cauchy",
        NA, NA, "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy",
        "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", NA,
        NA, NA, NA
      ),
      Location = c(
        0, 0, NA, NA, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, NA, NA, NA, NA
      ),
      Scale = c(
        0.5, 0.5, NA, NA, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, NA
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    tolerance = 1e-5
  )
})


x <- BayesFactor::lmBF(len ~ supp + dose, data = ToothGrowth, progress = FALSE)
test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})
test_that("find_formula", {
  expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose")), ignore_attr = TRUE)
})
test_that("get_parameters", {
  expect_named(
    get_parameters(x),
    c(
      "mu", "supp-OJ", "supp-VC", "dose-Low", "dose-Medium", "dose-High",
      "sig2", "g_supp", "g_dose"
    )
  )
})


x2 <- BayesFactor::lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth, progress = FALSE)
x <- x / x2
test_that("get_data", {
  expect_s3_class(get_data(x), "data.frame")
})
test_that("find_formula", {
  expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose")), ignore_attr = TRUE)
})
test_that("get_parameters", {
  expect_named(
    get_parameters(x, verbose = FALSE),
    c(
      "mu", "supp-OJ", "supp-VC", "dose-Low", "dose-Medium", "dose-High",
      "sig2", "g_supp", "g_dose"
    )
  )
})

test_that("get_priors", {
  expect_equal(
    get_priors(x),
    data.frame(
      Parameter = c("supp-OJ", "supp-VC", "dose-Low", "dose-Medium", "dose-High", "mu", "sig2", "g_supp", "g_dose"),
      Distribution = c("cauchy", "cauchy", "cauchy", "cauchy", "cauchy", NA, NA, NA, NA),
      Location = c(0, 0, 0, 0, 0, NA, NA, NA, NA),
      Scale = c(0.5, 0.5, 0.5, 0.5, 0.5, NA, NA, NA, NA),
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    ignore_attr = TRUE,
    tolerance = 1e-5
  )
})

test_that("find_statistic", {
  expect_null(find_statistic(x))
})


corr_BF1 <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width, progress = FALSE)
corr_BFk <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width,
  progress = FALSE,
  nullInterval = c(-1, 0)
)

data(raceDolls, package = "BayesFactor")
xtab_BF1 <- BayesFactor::contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols", priorConcentration = 2)

ttest_BF1 <- BayesFactor::ttestBF(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], progress = FALSE)
ttest_BFk <- BayesFactor::ttestBF(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2],
  progress = FALSE,
  nullInterval = c(-3, 0)
)

prop_BF1 <- BayesFactor::proportionBF(y = 15, N = 25, p = 0.5, progress = FALSE)
prop_BFk <- BayesFactor::proportionBF(
  y = 15, N = 25, p = 0.5, progress = FALSE,
  nullInterval = c(0, 0.3)
)


lm_BFk <- BayesFactor::generalTestBF(Sepal.Width ~ Sepal.Length + Species, data = iris, progress = FALSE)
lm_BFd <- lm_BFk[3] / lm_BFk[2]
lm_BF1 <- lm_BFk[2]

test_that("BFBayesFactor index model", {
  expect_message(get_parameters(corr_BFk))
  expect_message(get_parameters(ttest_BFk))
  expect_message(get_parameters(prop_BFk))
  expect_message(get_parameters(lm_BFk))
  expect_message(get_parameters(lm_BFd))

  expect_message(get_parameters(xtab_BF1), regexp = NA)
  expect_message(get_parameters(corr_BF1), regexp = NA)
  expect_message(get_parameters(ttest_BF1), regexp = NA)
  expect_message(get_parameters(prop_BF1), regexp = NA)
  expect_message(get_parameters(lm_BF1), regexp = NA)
})

test_that("get_priors for xtable", {
  expect_equal(
    get_priors(xtab_BF1),
    structure(
      list(
        Parameter = "Ratio",
        Distribution = "independent multinomial",
        Location = 0,
        Scale = 2
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    ),
    tolerance = 1e-5
  )
})

test_that("get_priors for correlation", {
  expect_equal(
    get_priors(corr_BF1),
    structure(list(
      Parameter = "rho", Distribution = "beta", Location = 3,
      Scale = 3
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    )),
    tolerance = 1e-5
  )
})

test_that("get_priors for t-test", {
  expect_equal(
    get_priors(ttest_BF1),
    structure(
      list(
        Parameter = "Difference",
        Distribution = "cauchy",
        Location = 0,
        Scale = 0.707106781186548
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    ),
    tolerance = 1e-5
  )
})


# Comes from https://github.com/easystats/bayestestR/issues/505

mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
model <- BayesFactor::lmBF(mpg ~ cyl + gear + cyl:gear, mtcars,
  progress = FALSE, whichRandom = c("gear", "cyl:gear")
)

test_that("find_formula for lmBF", {
  predicted_form <- find_formula(model)$conditional
  true_form <- formula(mpg ~ cyl + cyl:gear)

  expect_equal(
    predicted_form,
    true_form,
    ignore_attr = TRUE
  )
})
