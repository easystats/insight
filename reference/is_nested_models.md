# Checks whether a list of models are nested models

Checks whether a list of models are nested models, strictly following
the order they were passed to the function.

## Usage

``` r
is_nested_models(...)
```

## Arguments

- ...:

  Multiple regression model objects.

## Value

`TRUE` if models are nested, `FALSE` otherwise. If models are nested,
also returns two attributes that indicate whether nesting of models is
in decreasing or increasing order.

## Details

The term "nested" here means that all the fixed predictors of a model
are contained within the fixed predictors of a larger model (sometimes
referred to as the encompassing model). Currently, `is_nested_models()`
ignores random effects parameters.

## Examples

``` r
m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m2 <- lm(Sepal.Length ~ Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m4 <- lm(Sepal.Length ~ 1, data = iris)

is_nested_models(m1, m2, m4)
#> [1] TRUE
#> attr(,"is_nested_increasing")
#> [1] FALSE
#> attr(,"is_nested_decreasing")
#> [1] TRUE
is_nested_models(m4, m2, m1)
#> [1] TRUE
#> attr(,"is_nested_increasing")
#> [1] TRUE
#> attr(,"is_nested_decreasing")
#> [1] FALSE
is_nested_models(m1, m2, m3)
#> [1] FALSE
```
