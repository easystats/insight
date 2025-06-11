skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")

# Tests -------------------------------------------------------------------
test_that("clean_names works with multimembership", {
  m1 <- suppressWarnings(insight::download_model("brms_mm_4"))
  skip_if(is.null(m1))
  expect_identical(
    find_variables(m1),
    list(response = "y", conditional = "x", random = c("s1", "s2"))
  )
})

test_that("get_data works with multimembership", {
  m1 <- suppressWarnings(insight::download_model("brms_mm_4"))
  skip_if(is.null(m1))
  expect_named(
    get_data(m1),
    c("y", "x", "s1", "s2")
  )

  m2 <- suppressWarnings(insight::download_model("brms_mm_2"))
  skip_if(is.null(m2))
  expect_named(
    get_data(m2),
    c("y", "xc", "x1", "x2", "g1", "g2")
  )
})
