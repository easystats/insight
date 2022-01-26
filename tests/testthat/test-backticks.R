if (requiet("testthat") && requiet("insight")) {
  data(iris)
  iris$`a m` <- iris$Species
  iris$`Sepal Width` <- iris$Sepal.Width
  m <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
  m2 <- lm(`Sepal Width` ~ Petal.Length + `a m`, data = iris)

  test_that("text_remove_backticks", {
    d <- data.frame(Parameter = names(coef(m2)), Estimate = unname(coef(m2)))
    expect_equal(
      text_remove_backticks(d)$Parameter,
      c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica")
    )
  })

  test_that("backticks", {
    expect_equal(
      find_parameters(m),
      list(conditional = c(
        "(Intercept)", "Petal.Length", "a mversicolor",
        "a mvirginica", "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)",
        "a mvirginica:log(Sepal.Length)"
      ))
    )

    expect_equal(
      get_parameters(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )

    expect_equal(
      get_statistic(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )

    expect_equal(
      clean_parameters(m)$Parameter,
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
        "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )

    expect_equal(
      find_predictors(m),
      list(conditional = c("Petal.Length", "a m", "Sepal.Length"))
    )

    expect_equal(
      colnames(get_predictors(m)),
      c("Petal.Length", "a m", "Sepal.Length")
    )

    expect_equal(
      find_variables(m),
      list(
        response = "Sepal Width",
        conditional = c("Petal.Length", "a m", "Sepal.Length")
      )
    )

    expect_equal(
      find_terms(m),
      list(
        response = "Sepal Width",
        conditional = c("Petal.Length", "a m", "log(Sepal.Length)")
      )
    )

    expect_equal(
      rownames(get_varcov(m)),
      c(
        "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
        "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
      )
    )

    expect_equal(
      clean_names(m),
      c("Sepal Width", "Petal.Length", "a m", "Sepal.Length")
    )

    expect_equal(find_response(m), "Sepal Width")

    expect_equal(get_response(m), iris[["Sepal Width"]])
  })
}
