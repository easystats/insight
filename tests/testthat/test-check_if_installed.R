test_that("check_if_installed", {
  skip_if(interactive())
  skip_if_not_installed("datawizard")
  skip_if_not_installed("rstanarm")

  # mimic package name if cat were to walk on a keyboard
  expect_error(check_if_installed("xklfueofi8eur3rnfalfb"))

  expect_error(check_if_installed(c(
    "datawizard",
    minimum_version = "9.9.9"
  )))

  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  )))

  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  ), minimum_version = c("0.8.0", "2.21.1")))

  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  ), minimum_version = c(NA, "2.21.1")))

  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  ), minimum_version = c("0.8.0", NA)))
})
