if (require("testthat") &&
  require("insight") &&
  require("stats") &&
  require("BayesFactor")) {
  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

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
  # context("BF t.test one sample")
  # data(sleep)
  #
  # x <- ttestBF(x = sleep$extra[sleep$group == 1],
  #              y = sleep$extra[sleep$group == 2],
  #              paired = TRUE)
  #
  # test_that("get_data", {
  #   expect_true(is.data.frame(get_data(x)))
  # })
  # test_that("find_formula", {
  #   expect_null(find_formula(x))
  # })
  # test_that("get_parameters", {
  #   expect_equal(nrow(get_parameters(x)), 4000)
  # })



  # ---------------------------
  data(chickwts)
  chickwts <-
    chickwts[chickwts$feed %in% c("horsebean", "linseed"), ]
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
  if (.runThisTest || Sys.getenv("USER") == "travis") {
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
  }

  # ---------------------------
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
    expect_equal(colnames(get_parameters(x)), c("mu", "supp-OJ", "supp-VC", "sig2", "g_supp"))
  })


  # ---------------------------
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
    expect_equal(
      colnames(get_parameters(x)),
      c(
        "mu", "shape-round", "shape-square", "ID-1", "ID-2", "ID-3",
        "ID-4", "ID-5", "ID-6", "ID-7", "ID-8", "ID-9", "ID-10", "ID-11",
        "ID-12", "sig2", "g_shape", "g_ID"
      )
    )
  })

  test_that("get_parameters", {
    expect_equal(
      find_parameters(x[4]),
      list(
        conditional = c(
          "shape-round", "shape-square", "color-color", "color-monochromatic",
          "shape:color-round.&.color", "shape:color-round.&.monochromatic",
          "shape:color-square.&.color", "shape:color-square.&.monochromatic"
        ),
        random = c(
          "ID-1", "ID-2", "ID-3", "ID-4", "ID-5", "ID-6", "ID-7",
          "ID-8", "ID-9", "ID-10", "ID-11", "ID-12"
        ),
        extra = c("mu", "sig2", "g_shape", "g_color", "g_ID", "g_shape:color")
      )
    )
  })

  test_that("find_response", {
    expect_equal(find_response(x), "RT")
  })

  test_that("find_random", {
    expect_equal(find_random(x), list(random = "ID"))
  })

  test_that("find_variables", {
    expect_equal(
      find_variables(x),
      list(
        response = "RT",
        conditional = c("shape", "color"),
        random = "ID"
      )
    )
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
      data.frame(
        Parameter = c("fixed", "random", "continuous"),
        Distribution = c("cauchy", "cauchy", "cauchy"),
        Location = c(0, 0, 0),
        Scale = c(0.5, 1, 0.353553390593274),
        stringsAsFactors = FALSE,
        row.names = NULL
      ),
      tolerance = 1e-5
    )
  })





  # ---------------------------
  x <- lmBF(len ~ supp + dose, data = ToothGrowth)
  test_that("get_data", {
    expect_true(is.data.frame(get_data(x)))
  })
  test_that("find_formula", {
    expect_equal(find_formula(x), list(conditional = as.formula("len ~ supp + dose")))
  })
  test_that("get_parameters", {
    expect_equal(
      colnames(get_parameters(x)),
      c(
        "mu", "supp-OJ", "supp-VC", "dose-Low", "dose-Medium", "dose-High",
        "sig2", "g_supp", "g_dose"
      )
    )
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
    expect_equal(
      colnames(get_parameters(x)),
      c(
        "mu", "supp-OJ", "supp-VC", "dose-Low", "dose-Medium", "dose-High",
        "sig2", "g_supp", "g_dose"
      )
    )
  })

  test_that("get_priors", {
    expect_equal(
      get_priors(x),
      data.frame(
        Parameter = c("fixed", "random", "continuous"),
        Distribution = c("cauchy", "cauchy", "cauchy"),
        Location = c(0, 0, 0),
        Scale = c(0.5, 1, 0.353553390593274),
        stringsAsFactors = FALSE,
        row.names = NULL
      ),
      tolerance = 1e-5
    )
  })

  test_that("find_statistic", {
    expect_null(find_statistic(x))
  })
}
