# Checks if an object is a regression model or statistical test object

Small helper that checks if a model is a regression model or a
statistical object. `is_regression_model()` is stricter and only returns
`TRUE` for regression models, but not for, e.g., `htest` objects.

## Usage

``` r
is_model(x)

is_regression_model(x)
```

## Arguments

- x:

  An object.

## Value

A logical, `TRUE` if `x` is a (supported) model object.

## Details

This function returns `TRUE` if `x` is a model object.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)

is_model(m)
#> [1] TRUE
is_model(mtcars)
#> [1] FALSE

test <- t.test(1:10, y = c(7:20))
is_model(test)
#> [1] TRUE
is_regression_model(test)
#> [1] FALSE
```
