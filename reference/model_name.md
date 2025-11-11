# Name the model

Returns the "name" (class attribute) of a model, possibly including
further information.

## Usage

``` r
model_name(x, ...)

# Default S3 method
model_name(x, include_formula = FALSE, include_call = FALSE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Currently not used.

- include_formula:

  Should the name include the model's formula.

- include_call:

  If `TRUE`, will return the function call as a name.

## Value

A character string of a name (which usually equals the model's class
attribute).

## Examples

``` r
m <- lm(Sepal.Length ~ Petal.Width, data = iris)
model_name(m)
#> [1] "lm"
model_name(m, include_formula = TRUE)
#> [1] "lm(Sepal.Length ~ Petal.Width)"
model_name(m, include_call = TRUE)
#> [1] "lm(formula = Sepal.Length ~ Petal.Width, data = iris)"

model_name(lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
#> [1] "lmerMod"
```
