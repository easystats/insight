# Checks if model has an intercept

Checks if model has an intercept.

## Usage

``` r
has_intercept(x, verbose = TRUE)
```

## Arguments

- x:

  A model object.

- verbose:

  Toggle warnings.

## Value

`TRUE` if `x` has an intercept, `FALSE` otherwise.

## Examples

``` r
model <- lm(mpg ~ 0 + gear, data = mtcars)
has_intercept(model)
#> [1] FALSE

model <- lm(mpg ~ gear, data = mtcars)
has_intercept(model)
#> [1] TRUE

model <- lmer(Reaction ~ 0 + Days + (Days | Subject), data = sleepstudy)
has_intercept(model)
#> [1] FALSE

model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
has_intercept(model)
#> [1] TRUE
```
