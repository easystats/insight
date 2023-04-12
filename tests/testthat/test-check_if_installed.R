test_that("check_if_installed", {
  skip_if(interactive())
  # mimic package name if cat were to walk on a keyboard
  expect_error(check_if_installed("xklfueofi8eur3rnfalfb"))
})
