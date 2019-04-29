if (require("testthat") && require("insight") && require("stats") && require("BayesFactor")) {

  context("BF correlation")
  x <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
  test_that("get_data", {
      expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_null(find_formula(x))
  })
  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(x)), 4000)
  })


  # ---------------------------
  context("BF t.test one sample")
  data(sleep)
  diffScores <- sleep$extra[1:10] - sleep$extra[11:20]
  x <- ttestBF(x = diffScores)
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_null(find_formula(x))
  })
  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(x)), 4000)
  })



  # ---------------------------
  context("BF t.test two samples")
  data(chickwts)
  chickwts <- chickwts[chickwts$feed %in% c("horsebean","linseed"),]
  chickwts$feed <- factor(chickwts$feed)
  x <- ttestBF(formula = weight ~ feed, data = chickwts)
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_null(find_formula(x))
  })
  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(x)), 4000)
  })


  # ---------------------------
  context("BF t.test meta-analytic")
  t <- c(-.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99)
  x <- meta.ttestBF(t = t, n1 = N, rscale = 1)
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_null(find_formula(x))
  })
  test_that("get_parameters", {
    expect_equal(nrow(get_parameters(x)), 4000)
  })

  # ---------------------------
  context("BF ANOVA")
  data(ToothGrowth)
  ToothGrowth$dose <- factor(ToothGrowth$dose)
  levels(ToothGrowth$dose) <- c("Low", "Medium", "High")
  x <- anovaBF(len ~ supp * dose, data = ToothGrowth)

  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })

  test_that("find_formula", {
    expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose + supp:dose")))
  })

  test_that("get_parameters", {
    expect_null(get_parameters(x))
  })


  # ---------------------------
  context("BF ANOVA Random")
  data(puzzles)
  x <- anovaBF(RT ~ shape * color + ID, data = puzzles, whichRandom = "ID")

  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })

  test_that("find_formula", {
    expect_equal(
      find_formula(x),
      list(
        conditional = as.formula("RT ~ shape + color + shape:color"),
        random = as.formula("~ID")
      )
    )
  })

  test_that("get_parameters", {
    expect_null(get_parameters(x))
  })

  test_that("find_response", {
    expect_equal(find_response(x), "RT")
  })

  test_that("find_random", {
    expect_equal(find_random(x), list(random = "ID"))
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(x),
      list(
        response = "RT",
        conditional = c("shape", "color"),
        random = "ID"
      )
    )
  })

  test_that("get_priors", {
    expect_equal(
      get_priors(x),
      structure(list(
        parameters = c("fixed", "random", "continuous"),
        distribution = c("Cauchy", "Cauchy", "Cauchy"),
        location = c(0, 0, 0),
        scale = c(0.5, 1, 0.353553390593274)),
        class = "data.frame",
        row.names = c("fixed", "random", "continuous")
      ),
      tolerance = 1e-5
    )
  })





  # ---------------------------
  context("BF lm")
  x <- lmBF(len ~ supp + dose, data = ToothGrowth)
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose")))
  })
  test_that("get_parameters", {
    expect_null(get_parameters(x))
  })



  x2 <- lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth)
  x <- x / x2
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose")))

  })
  test_that("get_parameters", {
    expect_null(get_parameters(x))
  })

  test_that("get_priors", {
    expect_equal(
      get_priors(x),
      structure(
        list(
          parameters = c("fixed", "random", "continuous"),
          distribution = c("Cauchy", "Cauchy", "Cauchy"),
          location = c(0, 0, 0),
          scale = c(0.5, 1, 0.353553390593274)
        ),
        class = "data.frame",
        row.names = c("fixed", "random", "continuous")
      ),
      tolerance = 1e-5
    )
  })

}
