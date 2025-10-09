skip_on_cran()
skip_if_not_installed("datawizard")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")

out <- tryCatch(
  datawizard::data_read(
    "https://github.com/easystats/circus/raw/refs/heads/main/data/lcmm.rda"
  ),
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
    list(conditional = Ydep2 ~ Time + I(Time^2), random = ~Time),
    ignore_attr = TRUE
  )

  expect_equal(
    find_formula(m3),
    list(classmb = ~ X1 + X2 + X3),
    ignore_attr = TRUE
  )
})


test_that("find_terms", {
  expect_identical(
    find_terms(m1),
    list(response = "Ydep2", conditional = c("Time", "I(Time^2)"), random = "Time")
  )
  expect_identical(
    find_terms(m3),
    list(classmb = c("X1", "X2", "X3"))
  )
})


test_that("find_variables", {
  expect_identical(
    find_variables(m1),
    list(response = "Ydep2", conditional = "Time")
  )
  expect_identical(
    find_variables(m3),
    list(response = "", classmb = c("X1", "X2", "X3"))
  )
})


test_that("find_parameters", {
  expect_identical(
    find_parameters(m1),
    list(
      linear = c("Linear 1 (intercept)", "Linear 2 (std err)"),
      longitudinal = c("Time", "I(Time^2)")
    )
  )
  expect_identical(
    find_parameters(m2),
    list(
      linear = c("Linear 1 (intercept)", "Linear 2 (std err)"),
      membership = c("intercept class1", "intercept class2"),
      longitudinal = c(
        "intercept class2",
        "intercept class3",
        "Time class1",
        "Time class2",
        "Time class3",
        "I(Time^2) class1",
        "I(Time^2) class2",
        "I(Time^2) class3"
      )
    )
  )
  expect_identical(
    find_parameters(m3),
    list(
      conditional = c(
        "intercept class1",
        "intercept class2",
        "X1 class1",
        "X1 class2",
        "X2 class1",
        "X2 class2",
        "X3 class1",
        "X3 class2"
      )
    )
  )
})


test_that("get_parameters", {
  out <- get_parameters(m1)
  est <- m1$best[!startsWith(names(m1$best), "varcov ")]
  expect_named(out, c("Parameter", "Estimate", "Component"))
  expect_equal(out$Estimate, est, ignore_attr = TRUE, tolerance = 1e-4)

  out <- get_parameters(m2)
  est <- m2$best[!startsWith(names(m2$best), "varcov ")]
  expect_named(out, c("Parameter", "Estimate", "Component", "Group"))
  expect_equal(out$Estimate, est, ignore_attr = TRUE, tolerance = 1e-4)

  out <- get_parameters(m3)
  est <- m3$best[!startsWith(names(m3$best), "varcov ")]
  expect_named(out, c("Parameter", "Estimate", "Group"))
  expect_equal(out$Estimate, est, ignore_attr = TRUE, tolerance = 1e-4)
})
