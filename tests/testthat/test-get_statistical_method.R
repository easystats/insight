
# switch for stats method type works ------------------------------------------

test_that(
  desc = "switch for stats method type works",
  code = {
    # abbreviated
    expect_identical(format_approach("p"), "parametric")
    expect_identical(format_approach("np"), "nonparametric")
    expect_identical(format_approach("r"), "robust")
    expect_identical(format_approach("bf"), "bayesian")

    # misspelled
    expect_identical(format_approach("parameteric"), "parametric")
    expect_identical(format_approach("non-parametric"), "nonparametric")

    # random
    expect_identical(format_approach("xxx"), "parametric")
  }
)
