# Checks if an object stems from a multivariate response model

Small helper that checks if a model is a multivariate response model,
i.e. a model with multiple outcomes.

## Usage

``` r
is_multivariate(x)
```

## Arguments

- x:

  A model object, or an object returned by a function from this package.

## Value

A logical, `TRUE` if either `x` is a model object and is a multivariate
response model, or `TRUE` if a return value from a function of
**insight** is from a multivariate response model.

## Examples

``` r
# \donttest{
library(rstanarm)
data("pbcLong")
model <- suppressWarnings(stan_mvmer(
  formula = list(
    logBili ~ year + (1 | id),
    albumin ~ sex + year + (year | id)
  ),
  data = pbcLong,
  chains = 1, cores = 1, seed = 12345, iter = 1000,
  show_messages = FALSE, refresh = 0
))
#> Fitting a multivariate glmer model.
#> 
#> Please note the warmup may be much slower than later iterations!

f <- find_formula(model)
is_multivariate(model)
#> [1] TRUE
is_multivariate(f)
#> [1] TRUE
# }
```
