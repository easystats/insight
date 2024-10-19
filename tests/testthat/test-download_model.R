skip_if_not_installed("httptest2")
skip_if_not_installed("httr2")

httptest2::with_mock_dir("download-model-success", {
  test_that("we can successfully get existing model", {
    model <- download_model("lm_0", verbose = FALSE)
    expect_s3_class(model, "lm")
  })
})

httptest2::with_mock_dir("download-model-failure", {
  test_that("we fail gracefully while getting non-existing model", {
    model <- download_model("xyz", verbose = FALSE)
    expect_null(model)
  })
})

test_that("request fails gracefully without internet", {
  httptest2::without_internet({
    model <- download_model("aov_1", verbose = FALSE)
    expect_null(model)
  })
})
