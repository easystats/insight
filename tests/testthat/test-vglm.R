skip_on_os("mac")
skip_if_not_installed("VGAM")

d.AD <- data.frame(
  treatment = gl(3, 3),
  outcome = gl(3, 1, 9),
  counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
)

m1 <-
  VGAM::vglm(
    counts ~ outcome + treatment,
    family = VGAM::poissonff,
    data = d.AD,
    trace = FALSE
  )

test_that("model_info", {
  expect_true(model_info(m1)$is_poisson)
  expect_false(model_info(m1)$is_bayesian)
  expect_false(model_info(m1)$is_linear)
})

test_that("find_predictors", {
  expect_identical(find_predictors(m1), list(conditional = c("outcome", "treatment")))
  expect_identical(
    find_predictors(m1, flatten = TRUE),
    c("outcome", "treatment")
  )
  expect_null(find_predictors(m1, effects = "random"))
})

test_that("find_random", {
  expect_null(find_random(m1))
})

test_that("get_random", {
  expect_warning(get_random(m1))
})

test_that("find_response", {
  expect_identical(find_response(m1), "counts")
})

test_that("get_response", {
  expect_equal(get_response(m1), d.AD$counts)
})

test_that("get_predictors", {
  expect_equal(colnames(get_predictors(m1)), c("outcome", "treatment"))
})

li <- suppressWarnings(link_inverse(m1)(0.2)[1, 1])
test_that("link_inverse", {
  expect_equal(li, exp(0.2), tolerance = 1e-5)
  expect_warning(link_inverse(m1)(0.2))
})

test_that("get_data", {
  expect_equal(nrow(get_data(m1, verbose = FALSE)), 9)
  expect_equal(
    colnames(get_data(m1, verbose = FALSE)),
    c("counts", "outcome", "treatment")
  )
})

test_that("find_formula", {
  expect_length(find_formula(m1), 1)
  expect_equal(
    find_formula(m1),
    list(conditional = as.formula("counts ~ outcome + treatment")),
    ignore_attr = TRUE
  )
})

test_that("find_terms", {
  expect_equal(
    find_terms(m1),
    list(
      response = "counts",
      conditional = c("outcome", "treatment")
    )
  )
  expect_equal(
    find_terms(m1, flatten = TRUE),
    c("counts", "outcome", "treatment")
  )
})

test_that("n_obs", {
  expect_equal(n_obs(m1), 9)
})

test_that("linkfun", {
  expect_false(is.null(link_function(m1)))
})

test_that("find_parameters", {
  expect_equal(
    find_parameters(m1),
    list(
      conditional = c(
        "(Intercept)",
        "outcome2",
        "outcome3",
        "treatment2",
        "treatment3"
      )
    )
  )
  expect_equal(nrow(get_parameters(m1)), 5)
  expect_equal(
    get_parameters(m1)$Parameter,
    c(
      "(Intercept)",
      "outcome2",
      "outcome3",
      "treatment2",
      "treatment3"
    )
  )
})

test_that("is_multivariate", {
  expect_false(is_multivariate(m1))
})

test_that("find_statistic", {
  expect_identical(find_statistic(m1), "z-statistic")
})

test_that("get_sigma, families with a standard deviation parameter", {
  set.seed(123)
  d <- data.frame(x2 = stats::runif(200))
  d$y1 <- stats::rnorm(200, 1 + 2 * d$x2, sd = 1.5)
  d$y2 <- stats::rnorm(200, 3 + 4 * d$x2, sd = 0.5)

  m <- suppressWarnings(VGAM::vglm(y1 ~ x2, VGAM::tobit, data = d, trace = FALSE))
  expect_equal(
    unclass(get_sigma(m)),
    exp(stats::coef(m)[["(Intercept):2"]]),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # one sigma per response
  m <- VGAM::vglm(cbind(y1, y2) ~ x2, VGAM::binormal, data = d, trace = FALSE)
  expect_equal(
    unclass(get_sigma(m)),
    stats::setNames(
      exp(stats::coef(m)[c("(Intercept):3", "(Intercept):4")]),
      c("sd1", "sd2")
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # sigma modelled with predictors
  m <- VGAM::vglm(y1 ~ x2, VGAM::uninormal(zero = NULL), data = d, trace = FALSE)
  expect_null(get_sigma(m))
})
