if (require("testthat") && require("insight")) {
  test_that("export_table", {
    # mimic package name if cat were to walk on a keyboard
    expect_error(check_if_installed("xklfueofi8eur3rnfalfb"))

    # just the message
    expect_snapshot(check_if_installed("xklfueofi8eur3rnfalfb", stop = FALSE))

    # customize the message
    expect_snapshot(check_if_installed("xklfueofi8eur3rnfalfb",
      reason = "for kittens to be happy",
      stop = FALSE
    ))
  })
}
