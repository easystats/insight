# Get the value at the intercept

Returns the value at the intercept (i.e., the intercept parameter), and
`NA` if there isn't one.

## Usage

``` r
get_intercept(x, ...)
```

## Arguments

- x:

  A model.

- ...:

  Not used.

## Value

The value of the intercept.

## Examples

``` r
get_intercept(lm(Sepal.Length ~ Petal.Width, data = iris))
#> [1] 4.777629
get_intercept(lm(Sepal.Length ~ 0 + Petal.Width, data = iris))
#> [1] NA

get_intercept(lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
#> [1] 3.406167
get_intercept(gamm4::gamm4(Sepal.Length ~ s(Petal.Width), data = iris))
#> [1] 5.843333
```
