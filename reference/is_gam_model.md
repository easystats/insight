# Checks if a model is a generalized additive model

Small helper that checks if a model is a generalized additive model.

## Usage

``` r
is_gam_model(x)
```

## Arguments

- x:

  A model object.

## Value

A logical, `TRUE` if `x` is a generalized additive model *and* has
smooth-terms

## Note

This function only returns `TRUE` when the model inherits from a typical
GAM model class *and* when smooth terms are present in the model
formula. If model has no smooth terms or is not from a typical gam
class, `FALSE` is returned.

## Examples

``` r
data(iris)
model1 <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)
model2 <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
is_gam_model(model1)
#> [1] FALSE
is_gam_model(model2)
#> [1] TRUE
```
