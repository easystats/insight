skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("correctly handle brms::gr()", {
    data(epilepsy, package = "brms")
    # assign function, without loading the package
    gr <- brms::gr
    m <- suppressWarnings(suppressMessages(
      brms::brm(count ~ Trt + (1 | gr(patient, by = Trt)), data = epilepsy, refresh = 0)
    ))
    expect_equal(
      find_formula(m),
      list(
        conditional = count ~ Trt,
        random = ~ 1 | gr(patient, by = Trt)
      ),
      ignore_attr = TRUE
    )
    expect_identical(find_predictors(m, "all"), list(conditional = "Trt", random = "patient"))
    expect_identical(find_random(m), list(random = "patient"))
    d <- get_data(m)
    expect_named(d, c("count", "Trt", "patient"))
  })
)
