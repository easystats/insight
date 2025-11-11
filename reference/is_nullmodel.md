# Checks if model is a null-model (intercept-only)

Checks if model is a null-model (intercept-only), i.e. if the
conditional part of the model has no predictors.

## Usage

``` r
is_nullmodel(x)
```

## Arguments

- x:

  A model object.

## Value

`TRUE` if `x` is a null-model, `FALSE` otherwise.

## Examples

``` r
model <- lm(mpg ~ 1, data = mtcars)
is_nullmodel(model)
#> [1] TRUE

model <- lm(mpg ~ gear, data = mtcars)
is_nullmodel(model)
#> [1] FALSE

data(sleepstudy, package = "lme4")
model <- lme4::lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
is_nullmodel(model)
#> [1] TRUE

model <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
is_nullmodel(model)
#> [1] FALSE
```
