
# switch for stats method type works ------------------------------------------

test_that(
  desc = "switch for stats method type works",
  code = {
    # abbreviated
    expect_identical(format_statistical_approach("p"), "parametric")
    expect_identical(format_statistical_approach("np"), "nonparametric")
    expect_identical(format_statistical_approach("r"), "robust")
    expect_identical(format_statistical_approach("bf"), "bayesian")

    # misspelled
    expect_identical(format_statistical_approach("parameteric"), "parametric")
    expect_identical(format_statistical_approach("non-parametric"), "nonparametric")

    # random
    expect_identical(format_statistical_approach("xxx"), "parametric")
  }
)
