
# switch for stats method type works ------------------------------------------

test_that(
  desc = "switch for stats method type works",
  code = {
    # abbreviated
    expect_identical(get_statistical_method("p"), "parametric")
    expect_identical(get_statistical_method("np"), "nonparametric")
    expect_identical(get_statistical_method("r"), "robust")
    expect_identical(get_statistical_method("bf"), "bayesian")

    # misspelled
    expect_identical(get_statistical_method("parameteric"), "parametric")
    expect_identical(get_statistical_method("non-parametric"), "nonparametric")

    # random
    expect_identical(get_statistical_method("xxx"), "parametric")
  }
)
