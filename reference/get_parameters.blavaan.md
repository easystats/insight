# Get model parameters from structural equation models

Returns the coefficients (or posterior samples for **blavaan** models
with `summary = FALSE`) from a structural equation model.

## Usage

``` r
# S3 method for class 'blavaan'
get_parameters(x, summary = FALSE, standardize = FALSE, ...)

# S3 method for class 'lavaan'
get_parameters(x, standardize = FALSE, ...)
```

## Arguments

- x:

  A model of class `lavaan` or `blavaan`.

- summary:

  Logical. For **blavaan** models, indicates whether the full posterior
  samples (`summary = FALSE`) or point estimates (`summary = TRUE`)
  should be returned.

- standardize:

  Logical. If `TRUE`, standardized parameters are returned.

- ...:

  If `standardize = TRUE`, a `type` argument can be used, which is
  passed to
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  (or to
  [`blavaan::standardizedPosterior()`](https://blavaan.org/reference/standardizedPosterior.html)
  for the posterior samples of **blavaan** models). For example, `type`
  can be `"std.all"` (the default), `"std.lv"` or `"std.nox"`. See
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  for all values. Other arguments are not passed on.

## Value

For **blavaan** models with `summary = FALSE`, the posterior samples as
data frame, with one row per draw (chains pooled) and one column per
free or defined parameter. If `standardize = TRUE`, fixed parameters are
included, too. Else, a data frame with the columns `Parameter`,
`Estimate` and `Component`.

## Examples

``` r
m <- lavaan::cfa("visual =~ x1 + x2 + x3", data = lavaan::HolzingerSwineford1939)
get_parameters(m, standardize = TRUE, type = "std.lv")
#>        Parameter  Estimate Component
#> 1     visual=~x1 0.7236899    latent
#> 2     visual=~x2 0.5629087    latent
#> 3     visual=~x3 0.8013092    latent
#> 4         x1~~x1 0.8346429  residual
#> 5         x2~~x2 1.0649177  residual
#> 6         x3~~x3 0.6327684  residual
#> 7 visual~~visual 1.0000000  residual
```
