skip_if_not_installed("bife")
requiet("bife")

dataset <- bife::psid
mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dataset)

test_that("get_predicted", {
  # link
  x <- get_predicted(mod, predict = "link", data = dataset)
  y <- get_predicted(mod, predict = NULL, type = "link", data = dataset)
  z <- predict(mod, type = "link", X_new = dataset)
  expect_equal(x, y)
  expect_equal(as.vector(x), z)
  # resopnse
  x <- get_predicted(mod, predict = "expectation", data = dataset)
  y <- get_predicted(mod, predict = NULL, type = "response", data = dataset)
  z <- predict(mod, type = "response", X_new = dataset)
  expect_equal(x, y)
  expect_equal(as.vector(x), z)
})


test_that("get_varcov", {
  out <- get_varcov(mod)
  expect_equal(colnames(mod), names(coef(mod)))
})
