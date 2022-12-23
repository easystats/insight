if (requiet("testthat") && requiet("insight")) {
  test_that("find_response", {
    expect_equal(find_response(y1 + y2 ~ x, combine = FALSE), c("y1", "y2"))
    expect_equal(find_response(y1 + y2 ~ x, combine = TRUE), "y1 + y2")
  })
}
