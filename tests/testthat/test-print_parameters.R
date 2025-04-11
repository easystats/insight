skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")

test_that("print_parameters brms", {
  m <- download_model("brms_zi_3")
  out <- print_parameters(m)
  expect_named(
    out,
    c(
      "fixed.conditional", "fixed.zi", "random.conditional.Intercept: persons",
      "random.zi.Intercept: persons", "random.conditional.SD/Cor: persons",
      "random.zi.SD/Cor: persons"
    )
  )
  att <- lapply(out, function(i) {
    attributes(i)$table_caption
  })
  expect_identical(
    att,
    list(
      fixed.conditional = c("# Fixed effects (conditional)", "blue"),
      fixed.zi = c("# Fixed effects (zero-inflated)", "blue"),
      `random.conditional.Intercept: persons` = c("# Random effects (conditional)", "blue"),
      `random.zi.Intercept: persons` = c("# Random effects (zero-inflated)", "blue"),
      `random.conditional.SD/Cor: persons` = c("# Random effects (conditional)", "blue"),
      `random.zi.SD/Cor: persons` = c("# Random effects (zero-inflated)", "blue")
    )
  )
})
