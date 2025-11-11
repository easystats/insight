# Find statistic for model

Returns the statistic for a regression model (*t*-statistic,
*z*-statistic, etc.).

Small helper that checks if a model is a regression model object and
return the statistic used.

## Usage

``` r
find_statistic(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Currently not used.

## Value

A character describing the type of statistic. If there is no statistic
available with a distribution, `NULL` will be returned.

## Examples

``` r
# regression model object
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_statistic(m)
#> [1] "t-statistic"
```
