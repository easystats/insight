test_that("clean_names", {
  expect_identical(clean_names(""), "")
  expect_identical(clean_names("as.factor(test)"), "test")
  expect_identical(clean_names("log(test)"), "test")
  expect_identical(clean_names("log(test, base = exp(3))"), "test")
  expect_identical(clean_names("log(test,base=exp(3))"), "test")
  expect_identical(clean_names("log(test/10)"), "test")
  expect_identical(clean_names("log(test^2)"), "test")
  expect_identical(clean_names("log(log(test))"), "test")
  expect_identical(clean_names("log(log(test/10))"), "test")
  expect_identical(clean_names("log(log(test*2))"), "test")
  expect_identical(clean_names("scale(log(Days1))"), "Days1")
  expect_identical(clean_names("I(test^2)"), "test")
  expect_identical(clean_names("I(test/10)"), "test")
  expect_identical(clean_names("I(test ^ 2)"), "test")
  expect_identical(clean_names("I(test / 10)"), "test")
  expect_identical(clean_names("poly(test, 2)"), "test")
  expect_identical(clean_names("poly(test, degrees = 2)"), "test")
  expect_identical(clean_names("poly(test, degrees = 2, raw = TRUE)"), "test")
  expect_identical(clean_names("ns(test)"), "test")
  expect_identical(clean_names("ns(test, df = 2)"), "test")
  expect_identical(clean_names("bs(test)"), "test")
  expect_identical(clean_names("bs(test, df = 2)"), "test")
  expect_identical(clean_names("offset(test)"), "test")
  expect_identical(clean_names("offset(log(test))"), "test")
  expect_identical(clean_names("factor(test)"), "test")
  expect_identical(clean_names("as.factor(test)"), "test")
  expect_identical(clean_names("~ 1 | test"), "test")
  expect_identical(clean_names("~1|test"), "test")
  expect_identical(clean_names("1 | test"), "test")
  expect_identical(clean_names("as.factor(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("log(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("log(Sepal.Length, base = exp(3))"), "Sepal.Length")
  expect_identical(clean_names("log(Sepal.Length,base=exp(3))"), "Sepal.Length")
  expect_identical(clean_names("log(Sepal.Length/10)"), "Sepal.Length")
  expect_identical(clean_names("log(Sepal.Length^2)"), "Sepal.Length")
  expect_identical(clean_names("log(log(Sepal.Length))"), "Sepal.Length")
  expect_identical(clean_names("log(log(Sepal.Length/10))"), "Sepal.Length")
  expect_identical(clean_names("log(log(Sepal.Length*2))"), "Sepal.Length")
  expect_identical(clean_names("I(Sepal.Length^2)"), "Sepal.Length")
  expect_identical(clean_names("I(Sepal.Length/10)"), "Sepal.Length")
  expect_identical(clean_names("I(Sepal.Length ^ 2)"), "Sepal.Length")
  expect_identical(clean_names("I(Sepal.Length / 10)"), "Sepal.Length")
  expect_identical(clean_names("poly(Sepal.Length, 2)"), "Sepal.Length")
  expect_identical(clean_names("poly(Sepal.Length, degrees = 2)"), "Sepal.Length")
  expect_identical(clean_names("poly(Sepal.Length, degrees = 2, raw = TRUE)"), "Sepal.Length")
  expect_identical(clean_names("ns(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("ns(Sepal.Length, df = 2)"), "Sepal.Length")
  expect_identical(clean_names("bs(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("bs(Sepal.Length, df = 2)"), "Sepal.Length")
  expect_identical(clean_names("offset(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("offset(log(Sepal.Length))"), "Sepal.Length")
  expect_identical(clean_names("factor(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("as.factor(Sepal.Length)"), "Sepal.Length")
  expect_identical(clean_names("~ 1 | Sepal.Length"), "Sepal.Length")
  expect_identical(clean_names("~1|Sepal.Length"), "Sepal.Length")
  expect_identical(clean_names("1 | Sepal.Length"), "Sepal.Length")
  expect_identical(clean_names(c("scale(a)", "scale(b)", "scale(a):scale(b)")), c("a", "b", "a:b"))
  expect_identical(
    clean_names(c("scale(a)", "scale(b)", "scale(a):scale(b)"), include_names = TRUE),
    c(`scale(a)` = "a", `scale(b)` = "b", `scale(a):scale(b)` = "a:b")
  )
  expect_identical(clean_names("s(x1, x2)"), "x1, x2")
  expect_identical(clean_names("s(x1, x2, k = -1)"), "x1, x2")
  expect_identical(clean_names("s(x1, x2, x3)"), "x1, x2, x3")
})


test_that("clean_names, model", {
  m_rel1 <- lm(mpg ~ relevel(as.factor(cyl), "8") + gear, data = mtcars)
  expect_identical(insight::clean_names(m_rel1), c("mpg", "cyl", "gear"))
  mtcars2 <- mtcars
  mtcars2$cyl <- as.factor(mtcars2$cyl)
  m_rel2 <- lm(mpg ~ relevel(cyl, "8") + gear, data = mtcars2)
  expect_identical(insight::clean_names(m_rel2), c("mpg", "cyl", "gear"))
})


test_that("clean_names, multimembership", {
  skip_if_not_installed("gamlss")
  set.seed(123)
  dat <- data.frame(
    Y = sample(20:50, 100, replace = TRUE),
    date = sample(seq(as.Date("1999/01/01"), as.Date("2000/01/01"), by = "day"), 10),
    cont1 = rchisq(100, df = 2),
    cont2 = runif(100),
    cat1 = sample(LETTERS[1:3], 100, replace = TRUE)
  )
  junk <- capture.output({
    mod1 <- suppressWarnings(gamlss::gamlss(
      Y ~ date + scale(cont1) + scale(cont2) + I(scale(cont2)^2) * cat1,
      data = dat
    ))
  })
  expect_identical(
    clean_names(find_terms(mod1)$conditional),
    c("date", "cont1", "cont2", "cont2", "cat1")
  )
})


test_that("clean_names, multimembership", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("httr2")

  m1 <- suppressWarnings(insight::download_model("brms_mm_1"))
  skip_if(is.null(m1))
  out <- clean_names(m1)
  expect_identical(out, c("y", "x", "t1id", "t2id"))
  expect_identical(
    find_variables(m1),
    list(
      response = "y",
      conditional = "x",
      random = c("t1id", "t2id")
    )
  )

  m3 <- suppressWarnings(insight::download_model("brms_mm_3"))
  skip_if(is.null(m3))
  out <- clean_names(m3)
  expect_identical(out, c("mpg", "hp", "cyl", "carb", "am", "w"))
  expect_identical(
    find_variables(m3),
    list(
      response = "mpg",
      conditional = "hp",
      random = c("cyl", "carb", "am", "hp", "w")
    )
  )
})


test_that("clean_names, division in I()", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg")
  data("FoodExpenditure", package = "betareg")

  m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  m2 <- betareg::betareg(I(food / income) ~ income + persons, data = FoodExpenditure)
  expect_identical(clean_names(m1), c("yield", "batch", "temp"))
  expect_identical(clean_names(m2), c("food", "income", "persons"))
})
