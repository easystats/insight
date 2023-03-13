if (skip_if_not_or_load_if_installed("metafor")) {
  d <- data.frame(
    estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
    std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
  )
  mydat <<- d
  model <- metafor::rma(yi = estimate, sei = std.error, data = mydat)

  test_that("model_info", {
    expect_true(model_info(model)$is_linear)
    expect_true(model_info(model)$is_meta)
  })

  test_that("find_formula", {
    expect_equal(
      find_formula(model),
      list(conditional = estimate ~ 1),
      ignore_attr = TRUE
    )
  })

  out <- get_data(model)
  test_that("get_data", {
    expect_equal(
      out$estimate,
      c(0.111, 0.245, 0.8, 1.1, 0.03),
      tolerance = 1e-3
    )
    expect_identical(dim(out), as.integer(c(5, 3)))
    expect_identical(colnames(out), c("estimate", "std.error", "Weights"))
  })

  out <- get_data(model, source = "mf")
  test_that("get_data, modelframe", {
    expect_identical(dim(out), as.integer(c(5, 3)))
    expect_identical(colnames(out), c("estimate", "std.error", "Weights"))
  })

  data(dat.bcg)
  dat <- escalc(
    measure = "RR", ai = tpos, bi = tneg, ci = cpos,
    di = cneg, data = dat.bcg
  )
  dat$alloc <- ifelse(dat$alloc == "random", "random", "other")
  model <- rma(yi, vi,
    mods = ~alloc, data = dat, digits = 3,
    slab = author
  )
  test_that("get_data, modelframe", {
    expect_equal(
      find_formula(model),
      list(conditional = yi ~ alloc, dispersion = yi ~ alloc),
      ignore_attr = TRUE
    )
  })
}
