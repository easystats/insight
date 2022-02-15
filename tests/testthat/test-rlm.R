.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (requiet("MASS") && requiet("testthat")) {
  test_that("model.matrix.rlm accepts `data` argument", {
    mod <- MASS::rlm(mpg ~ hp + factor(cyl), mtcars)
    mm <- get_modelmatrix(mod)
    expect_true(is.matrix(mm))
    expect_equal(dim(mm), c(32, 4))
    mm <- get_modelmatrix(mod, data = head(mtcars))
    expect_true(is.matrix(mm))
    expect_equal(dim(mm), c(6, 4))
  })

  if (.runThisTest) {
    test_that("predict.rlm", {
      mod <- MASS::rlm(mpg ~ hp + factor(cyl), mtcars)
      a <- get_predicted(mod)
      b <- get_predicted(mod, predict = NULL, type = "response", verbose = FALSE)
      expect_s3_class(a, "get_predicted")
      expect_s3_class(b, "get_predicted")
      expect_equal(a, b, ignore_attr = TRUE)
      expect_equal(as.vector(a), as.vector(b))
      expect_error(get_predicted(mod, predict = "link"))
      expect_error(get_predicted(mod, predict = NULL, type = "link"))
    })
  }
}
