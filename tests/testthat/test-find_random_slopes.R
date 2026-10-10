test_that("find_random_slopes ignores grouping and correlation names inside slope names", {
  f <- structure(
    list(random = list(~ 1 + Days | D, ~ 1 + Days | a | Subject)),
    class = c("insight_formula", "list")
  )
  expect_identical(find_random_slopes(f), list(random = "Days"))

  f <- structure(
    list(random = ~ 1 + Days || Subject),
    class = c("insight_formula", "list")
  )
  expect_identical(find_random_slopes(f), list(random = "Days"))
})


test_that("find_random_slopes, brms Intercept is no random slope", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("brms")
  skip_if_not_installed("httr2")

  # ~ 0 + Intercept | Participant
  model <- suppressWarnings(insight::download_model("brms_chocomini_1"))
  skip_if(is.null(model))
  expect_null(find_random_slopes(model))
})
