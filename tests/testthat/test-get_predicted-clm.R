pkgs <- c("testthat", "insight", "ordinal")
invisible(sapply(pkgs, requiet))

test_that("get_predicted.default - ordinal - match CI", {
  skip_if(getRversion() < "4.2.0")

  data(wine, package = "ordinal")
  m <- clm(rating ~ temp * contact, data = wine)
  dg <- get_datagrid(m, "temp", verbose = FALSE)

  out <- get_predicted(m, ci = 0.95, data = dg, verbose = FALSE)
  p <- predict(m, newdata = dg, interval = TRUE, se.fit = TRUE)

  expect_equal(
    out$Predicted,
    unname(unlist(lapply(as.data.frame(p$fit), as.vector))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_equal(
    attributes(out)$ci_data$SE,
    unname(unlist(lapply(as.data.frame(p$se.fit), as.vector))),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_equal(
    attributes(out)$ci_data$CI_low,
    unlist(lapply(as.data.frame(p$lwr), as.vector)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_equal(
    attributes(out)$ci_data$CI_high,
    unlist(lapply(as.data.frame(p$upr), as.vector)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
