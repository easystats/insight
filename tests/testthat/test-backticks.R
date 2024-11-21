skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("text_remove_backticks", {
    iris$`a m` <- iris$Species
    iris$`Sepal Width` <- iris$Sepal.Width
    dat <- iris
    m <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = dat)
    m2 <- lm(`Sepal Width` ~ Petal.Length + `a m`, data = dat)

    d <- data.frame(Parameter = names(coef(m2)), Estimate = unname(coef(m2)), stringsAsFactors = FALSE)
    expect_identical(
      text_remove_backticks(d)$Parameter,
      c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica")
    )

    d <- data.frame(
      Parameter = names(coef(m2)),
      Term = names(coef(m2)),
      Estimate = unname(coef(m2)),
      stringsAsFactors = FALSE
    )
    x <- text_remove_backticks(d, c("Parameter", "Term"))
    expect_identical(x$Parameter, c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica"))
    expect_identical(x$Term, c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica"))

    d <- list(Parameter = names(coef(m2)), Estimate = unname(coef(m2)))
    expect_warning(text_remove_backticks(d, verbose = TRUE))
    expect_equal(
      text_remove_backticks(d),
      list(
        Parameter = c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica"),
        Estimate = c(2.99186937324135, 0.298310962215218, -1.49267407227818, -1.67409183546024)
      ),
      tolerance = 1e-3
    )
    expect_identical(
      find_parameters(m),
      list(conditional = c(
        "(Intercept)", "Petal.Length", "a mversicolor",
        "a mvirginica", "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)",
        "a mvirginica:log(Sepal.Length)"
      ))
    )
    expect_identical(
      get_parameters(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )
    expect_identical(
      get_statistic(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )
    expect_identical(
      clean_parameters(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )
    expect_identical(
      find_predictors(m, verbose = FALSE),
      list(conditional = c("Petal.Length", "a m", "Sepal.Length"))
    )
    expect_identical(
      find_variables(m, verbose = FALSE),
      list(
        response = "Sepal Width",
        conditional = c("Petal.Length", "a m", "Sepal.Length")
      )
    )
    expect_identical(
      find_terms(m, verbose = FALSE),
      list(
        response = "Sepal Width",
        conditional = c("Petal.Length", "a m", "log(Sepal.Length)")
      )
    )
    expect_identical(
      rownames(get_varcov(m)),
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
        "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )
    expect_identical(
      clean_names(m),
      c("Sepal Width", "Petal.Length", "a m", "Sepal.Length")
    )
    expect_identical(find_response(m), "Sepal Width")
    expect_identical(get_response(m), iris[["Sepal Width"]])
    expect_named(get_predictors(m), c("Petal.Length", "a m", "Sepal.Length"))
  })
)


test_that("text_remove_backticks, character", {
  x <- "`test`"
  expect_identical(text_remove_backticks(x), "test")
  x <- "test"
  expect_identical(text_remove_backticks(x), "test")
  x <- NULL
  expect_null(text_remove_backticks(x))
})


test_that("text_remove_backticks, matrix", {
  x <- matrix(1:9, nrow = 3)
  out <- text_remove_backticks(x)
  expect_identical(dimnames(x), dimnames(out))

  colnames(x) <- rownames(x) <- 1:3
  out <- text_remove_backticks(x)
  expect_identical(dimnames(x), dimnames(out))

  colnames(x) <- rownames(x) <- paste0("`", 1:3, "`")
  out <- text_remove_backticks(x)
  expect_identical(list(as.character(1:3), as.character(1:3)), dimnames(out))
})
