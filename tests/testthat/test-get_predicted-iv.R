pkgs <- c("insight", "estimatr", "ivreg")
invisible(sapply(pkgs, requiet))


# iv_robust --------------------------------------------------------------
# =========================================================================

test_that("get_predicted.default - iv_robust", {

  data(Kmenta, package = "ivreg")
  x <- iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Kmenta)

  out <- get_predicted(x)
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = Kmenta)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  out <- get_predicted(x, predict = "expectation", data = head(Kmenta))
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = head(Kmenta))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  out <- get_predicted(x, predict = "expectation", data = head(Kmenta), ci = .95)
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = head(Kmenta))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(out)$SE,
    c(0.58353, 0.50232, 0.48325, 0.50327, 0.55574, 0.50894),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})



# ivreg --------------------------------------------------------------
# ====================================================================

test_that("get_predicted.default - ivreg", {

  data(Kmenta, package = "ivreg")
  x <- ivreg(Q ~ P + D | D + F + A, data = Kmenta)

  out <- get_predicted(x)
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = Kmenta)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  out <- get_predicted(x, predict = "expectation", data = head(Kmenta))
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = head(Kmenta))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  out <- get_predicted(x, predict = "expectation", data = head(Kmenta), ci = .95)
  expect_equal(
    as.numeric(out),
    as.numeric(predict(x, newdata = head(Kmenta))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(out)$SE,
    c(0.66092, 0.59968, 0.56441, 0.60494, 0.51642, 0.47378),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
