skip_if_not_installed("dbarts")
skip_if(getRversion() < "4.5.0")

test_that("find_formula", {
  data(penguins, package = "datasets")
  mod <- dbarts::bart2(
    bill_len ~ .,
    data = penguins,
    keepTrees = TRUE,
    verbose = FALSE
  )
  form <- find_formula(mod)
  expect_equal(
    form,
    list(
      conditional = bill_len ~
        species + island + bill_dep + flipper_len + body_mass + sex + year
    ), # nolint
    ignore_attr = TRUE
  )
})

test_that("get_data", {
  data(penguins, package = "datasets")
  mod <- dbarts::bart2(
    bill_len ~ .,
    data = penguins,
    keepTrees = TRUE,
    verbose = FALSE
  )
  d <- get_data(mod)
  expect_identical(dim(d), c(333L, 8L))
})
