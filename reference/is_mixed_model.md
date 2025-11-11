# Checks if a model is a mixed effects model

Small helper that checks if a model is a mixed effects model, i.e. if it
the model has random effects.

## Usage

``` r
is_mixed_model(x)
```

## Arguments

- x:

  A model object.

## Value

A logical, `TRUE` if `x` is a mixed model.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + cyl + vs, data = mtcars)
is_mixed_model(model)
#> [1] FALSE

data(sleepstudy, package = "lme4")
model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
is_mixed_model(model)
#> [1] TRUE
```
