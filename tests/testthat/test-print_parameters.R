skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_on_os("mac")
skip_if_not_installed("brms")
skip_if_not_installed("httr2")
skip_if_not_installed("glmmTMB")

test_that("print_parameters brms", {
  m <- download_model("brms_zi_3")
  skip_if(is.null(m))
  out <- print_parameters(m)
  expect_named(
    out,
    c(
      "fixed.conditional",
      "fixed.zi",
      "random.conditional.Intercept: persons",
      "random.zi.Intercept: persons",
      "random.conditional.SD/Cor: persons",
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
      `random.conditional.Intercept: persons` = c(
        "# Random effects (conditional)",
        "blue"
      ),
      `random.zi.Intercept: persons` = c("# Random effects (zero-inflated)", "blue"),
      `random.conditional.SD/Cor: persons` = c("# Random effects (conditional)", "blue"),
      `random.zi.SD/Cor: persons` = c("# Random effects (zero-inflated)", "blue")
    )
  )
})


test_that("print_parameters glmmTMB", {
  data(fish, package = "insight")
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper + (1 + xb | persons),
    ziformula = ~ child + livebait + (1 + zg + nofish | ID),
    dispformula = ~xb,
    data = fish,
    family = poisson()
  ))
  out <- print_parameters(m)
  expect_named(
    out,
    c(
      "fixed.conditional",
      "fixed.zero_inflated",
      "random.conditional.persons",
      "random.zero_inflated.ID"
    )
  )
  att <- lapply(out, function(i) {
    attributes(i)$table_caption
  })
  expect_identical(
    att,
    list(
      fixed.conditional = c("# Fixed effects (conditional)", "blue"),
      fixed.zero_inflated = c("# Fixed effects (zero-inflated)", "blue"),
      random.conditional.persons = c("# Random effects (conditional)", "blue"),
      random.zero_inflated.ID = c("# Random effects (zero-inflated)", "blue")
    )
  )
})
