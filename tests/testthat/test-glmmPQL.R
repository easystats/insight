skip_if_not_installed("nlme")
skip_if_not_installed("MASS")
skip_if(getRversion() < "4.3.0")
skip_on_ci()

test_that("find_formula, get_data glmmPQL", {
  example_dat <- data.frame(
    prop = c(0.2, 0.2, 0.5, 0.7, 0.1, 1, 1, 1, 0.1),
    size = c(
      "small",
      "small",
      "small",
      "large",
      "large",
      "large",
      "large",
      "small",
      "small"
    ),
    x = c(0.1, 0.1, 0.8, 0.7, 0.6, 0.5, 0.5, 0.1, 0.1),
    species = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp4", "sp4", "sp4"),
    stringsAsFactors = FALSE
  )

  mn <- MASS::glmmPQL(
    prop ~ x + size,
    random = ~ 1 | species,
    family = "quasibinomial",
    data = example_dat
  )
  expect_identical(find_formula(mn)$conditional, as.formula("prop ~ x + size"))
  expect_named(get_data(mn), c("prop", "x", "size", "species"))
})

test_that("find_formula, glmmPQL, correlation structure without `form`", {
  # `t` is an integer position, as in the lme and gls tests
  d <- nlme::Orthodont
  d$t <- as.integer(d$age)
  # nlme uses the grouping of the random effects
  m <- MASS::glmmPQL(
    I(distance > 25) ~ age,
    random = ~ 1 | Subject,
    family = binomial,
    data = d,
    correlation = nlme::corAR1(),
    verbose = FALSE
  )
  expect_equal(find_formula(m)$correlation, ~ 1 | Subject, ignore_attr = TRUE)
})

test_that("find_formula, glmmPQL, correlation object local to a function", {
  d <- nlme::Orthodont
  d$t <- as.integer(d$age)
  fit_local <- function() {
    glmmpql_corr_local <- nlme::corAR1(form = ~ t | Subject)
    MASS::glmmPQL(
      I(distance > 25) ~ age,
      random = ~ 1 | Subject,
      family = binomial,
      data = d,
      correlation = glmmpql_corr_local,
      verbose = FALSE
    )
  }
  expect_false(exists("glmmpql_corr_local", envir = globalenv()))
  m <- fit_local()
  expect_no_error(find_formula(m))
  # the expected formula is the `form` of the local structure
  expect_equal(find_formula(m)$correlation, ~ t | Subject, ignore_attr = TRUE)
})
