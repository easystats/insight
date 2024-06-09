httptest2::with_mock_dir("download-model-success", {
  test_that("we can successfully get existing model", {
    expect_s3_class(download_model("lm_0", verbose = FALSE), "lm")
  })
})

httptest2::with_mock_dir("download-model-failure", {
  test_that("we fail gracefully while getting non-existing model", {
    expect_null(download_model("xyz", verbose = FALSE))
  })
})

test_that("request fails gracefully without internet", {
  httptest2::without_internet({
    model <- download_model("aov_1", verbose = FALSE)
    expect_null(model)
  })
})
