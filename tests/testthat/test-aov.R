test_that("get_statistic", {
  skip_if(getRversion() < "4.5.0")
  data(penguins)
  fit <- aov(
    formula = body_mass ~ species,
    data = penguins
  )
  out <- get_statistic(fit)
  expect_equal(out$Statistic, 343.62628, tolerance = 1e-4)
})
