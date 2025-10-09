test_that("get_model", {
  skip_if_not_installed("parameters")
  data(mtcars)
  out <- parameters::factor_analysis(mtcars, n = 2)

  expect_s3_class(get_model(out), "psych")
  expect_equal(get_model(out, element = "BIC"), -48.27782, tolerance = 1e-3)
  expect_equal(
    get_model(out, element = c("BIC", "CFI")),
    list(BIC = -48.2778171514184, CFI = 0.899274697945245),
    tolerance = 1e-3
  )

  # errors
  expect_error(
    get_model(out, name = "nonexistent"),
    regex = "No attribute named `nonexistent`",
    fixed = TRUE
  )

  expect_error(
    get_model(out, element = "nonexistent"),
    regex = "Element(s) `nonexistent`",
    fixed = TRUE
  )

  expect_error(
    get_model(out, element = c("BIC", "AFI")),
    regex = "Element(s) `AFI`",
    fixed = TRUE
  )

  expect_error(
    get_model(out, element = c("BIA", "AFI")),
    regex = "Element(s) `BIA`, `AFI` not",
    fixed = TRUE
  )
})
