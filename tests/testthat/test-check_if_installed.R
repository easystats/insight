test_that("check_if_installed", {
  skip_if(interactive())
  skip_if_not_installed("datawizard")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("marginaleffects", minimum_version = "0.13.0")

  # mimic package name if cat were to walk on a keyboard
  expect_error(check_if_installed("xklfueofi8eur3rnfalfb"))
  expect_error(check_if_installed("datawizard", minimum_version = "9.9.9"))
  expect_no_error(check_if_installed(c("datawizard", "rstanarm")))
  expect_no_error(check_if_installed(
    c("datawizard", "rstanarm"),
    minimum_version = c("0.8.0", "2.21.1")
  ))
  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  ), minimum_version = c(NA, "2.21.1")))

  expect_no_error(check_if_installed(c(
    "datawizard", "rstanarm"
  ), minimum_version = c("0.8.0", NA)))

  expect_no_error(check_if_installed("marginaleffects", minimum_version = "0.9.0"))

  out <- check_if_installed(
    c("insight", "datawizard"),
    minimum_version = c("999.30.0", NA),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(FALSE, TRUE), ignore_attr = TRUE)

  out <- check_if_installed(
    c("insight", "datawizard"),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(TRUE, TRUE), ignore_attr = TRUE)

  out <- check_if_installed(
    c("insight", "datawizard"),
    minimum_version = c("0.1.0", "0.1.0"),
    prompt = FALSE,
    quiet = TRUE
  )
  expect_equal(out, c(TRUE, TRUE), ignore_attr = TRUE)
})
