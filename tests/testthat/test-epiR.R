skip_on_os("mac")
skip_if_not(packageVersion("base") >= "4.2.0")
skip_if_not_or_load_if_installed("epiR")


dat <- matrix(c(13, 2163, 5, 3349), nrow = 2, byrow = TRUE)
rownames(dat) <- c("DF+", "DF-")
colnames(dat) <- c("FUS+", "FUS-")

# model
m <- epi.2by2(
  dat = as.table(dat),
  method = "cohort.count",
  conf.level = 0.95,
  units = 100,
  outcome = "as.columns"
)

params <- get_parameters(m)

test_that("get_parameters", {
  expect_equal(
    params$Estimate,
    c(4.00754, 4.02561, 0.44835, 0.75047, 0.17642, 0.54201),
    tolerance = 1e-3
  )
  expect_equal(
    params$Parameter,
    c("RR", "OR", "ARisk", "AFRisk", "PARisk", "PAFRisk")
  )
})


stat <- get_statistic(m)

test_that("get_statistic", {
  expect_equal(stat$Statistic, 8.177135, tolerance = 1e-3)
  expect_equal(stat$Parameter, "Chi2")
})
