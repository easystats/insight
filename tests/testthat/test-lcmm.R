skip_on_cran()
skip_if_not_installed("datawizard")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")

out <- tryCatch(
  datawizard::data_read("https://github.com/easystats/circus/raw/refs/heads/main/data/lcmm.rda"),
  error = function(e) NULL
)

skip_if(is.null(out))
skip_if_not_installed("lcmm")

m1 <- out$m1_linear
m2 <- out$m2_linear
m3 <- out$mx_linear

test_that("model_info", {
  expect_true(model_info(m1)$is_linear)
  expect_false(model_info(m3)$is_linear)
  expect_true(model_info(m3)$is_multinomial)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = "Time"))
  expect_identical(find_predictors(m2), list(conditional = "Time", mixture = "Time"))
  expect_identical(find_predictors(m3), list(classmb = c("X1", "X2", "X3")))
})

test_that("find_response", {
  expect_identical(find_response(m1), "Ydep2")
  expect_identical(find_response(m2), "Ydep2")
  expect_identical(find_response(m3), "")
})

test_that("link_inverse", {
  expect_equal(link_inverse(m3)(0.2), plogis(0.2), tolerance = 1e-4)
})

test_that("loglik", {
  expect_equal(get_loglikelihood(m1), m1$loglik, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(get_loglikelihood(m2), m2$loglik, ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(get_loglikelihood(m3), m3$loglik, ignore_attr = TRUE, tolerance = 1e-3)
})

test_that("get_df", {
  expect_equal(get_df(m1), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m2), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m3), Inf, ignore_attr = TRUE)
  expect_equal(get_df(m1, type = "model"), 5, ignore_attr = TRUE)
  expect_equal(get_df(m3, type = "model"), 8, ignore_attr = TRUE)
})

test_that("find_formula", {
  expect_length(find_formula(m1), 2)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("Sepal.Length ~ Petal.Width + Species")),
    ignore_attr = TRUE
  )

  expect_length(find_formula(m2), 1)
  expect_equal(
    find_formula(m2),
    list(
      conditional = as.formula(
        "log(mpg) ~ log(hp) + cyl + I(cyl^2) + poly(wt, degree = 2, raw = TRUE)"
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(
      response = "Sepal.Length",
      conditional = c("Petal.Width", "Species")
    )
  )
  expect_identical(
    find_terms(m2),
    list(
      response = "log(mpg)",
      conditional = c(
        "log(hp)",
        "cyl",
        "I(cyl^2)",
        "poly(wt, degree = 2, raw = TRUE)"
      )
    )
  )
  expect_identical(
    find_terms(m1, flatten = TRUE),
    c("Sepal.Length", "Petal.Width", "Species")
  )
  expect_identical(
    find_terms(m2, flatten = TRUE),
    c(
      "log(mpg)",
      "log(hp)",
      "cyl",
      "I(cyl^2)",
      "poly(wt, degree = 2, raw = TRUE)"
    )
  )
})

test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(
      response = "Sepal.Length",
      conditional = c("Petal.Width", "Species")
    )
  )
  expect_identical(find_variables(m2), list(
    response = "mpg",
    conditional = c("hp", "cyl", "wt")
  ))
  expect_identical(
    find_variables(m1, flatten = TRUE),
    c("Sepal.Length", "Petal.Width", "Species")
  )
  expect_identical(
    find_variables(m2, flatten = TRUE),
    c("mpg", "hp", "cyl", "wt")
  )
})

test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      conditional = c(
        "(Intercept)",
        "Petal.Width",
        "Speciesversicolor",
        "Speciesvirginica"
      )
    )
  )
  expect_identical(nrow(get_parameters(m1)), 4L)
  expect_identical(
    get_parameters(m1)$Parameter,
    c(
      "(Intercept)",
      "Petal.Width",
      "Speciesversicolor",
      "Speciesvirginica"
    )
  )
})


test_that("find_parameters summary.lm", {
  s <- summary(m1)
  expect_identical(
    find_parameters(s),
    list(
      conditional = c(
        "(Intercept)",
        "Petal.Width",
        "Speciesversicolor",
        "Speciesvirginica"
      )
    )
  )
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
  expect_false(is.null(link_function(m2)))
})

test_that("find_algorithm", {
  expect_identical(find_algorithm(m1), list(algorithm = "OLS"))
})

test_that("get_variance", {
  expect_warning(expect_null(get_variance(m1)))
  expect_warning(expect_null(get_variance_dispersion(m1)))
  expect_warning(expect_null(get_variance_distribution(m1)))
  expect_warning(expect_null(get_variance_fixed(m1)))
  expect_warning(expect_null(get_variance_intercept(m1)))
  expect_warning(expect_null(get_variance_random(m1)))
  expect_warning(expect_null(get_variance_residual(m1)))
})

test_that("is_model", {
  expect_true(is_model(m1))
})

test_that("all_models_equal", {
  expect_true(all_models_equal(m1, m2))
})

test_that("get_varcov", {
  expect_equal(diag(get_varcov(m1)), diag(vcov(m1)), tolerance = 1e-5)
})

test_that("get_statistic", {
  expect_equal(get_statistic(m1)$Statistic, c(57.5427, 4.7298, -0.2615, -0.1398), tolerance = 1e-3)
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
})


data("DNase")
DNase1 <- subset(DNase, Run == 1)
m3 <-
  stats::nls(
    density ~ stats::SSlogis(log(conc), Asym, xmid, scal),
    DNase1,
    start = list(
      Asym = 1,
      xmid = 1,
      scal = 1
    )
  )

## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m4 <- glm(counts ~ outcome + treatment, family = poisson())

test_that("is_model", {
  expect_true(is_model(m3))
})

test_that("is_model", {
  expect_false(is_model_supported(m3))
})

test_that("all_models_equal", {
  expect_false(all_models_equal(m1, m2, m3))
  expect_false(all_models_equal(m1, m2, m4))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "t-statistic")
  expect_identical(find_statistic(m2), "t-statistic")
  expect_identical(find_statistic(m3), "t-statistic")
  expect_identical(find_statistic(m4), "z-statistic")
})

test_that("find_statistic", {
  m <- lm(cbind(mpg, hp) ~ cyl + drat, data = mtcars)
  expect_message(
    get_predicted(m),
    "not yet supported for models of class `mlm`"
  )
  expect_s3_class(suppressMessages(get_predicted(m)), "get_predicted")
})
