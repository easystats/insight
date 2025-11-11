# Find names of model parameters from generalized additive models

Returns the names of model parameters, like they typically appear in the
[`summary()`](https://rdrr.io/r/base/summary.html) output.

## Usage

``` r
# S3 method for class 'gamlss'
find_parameters(x, flatten = FALSE, ...)

# S3 method for class 'gam'
find_parameters(x, component = "all", flatten = FALSE, ...)
```

## Arguments

- x:

  A fitted model.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

- ...:

  Currently not used.

- component:

  Which type of parameters to return, such as parameters for the
  conditional model, the zero-inflated part of the model, the dispersion
  term, the instrumental variables or marginal effects be returned?
  Applies to models with zero-inflated and/or dispersion formula, or to
  models with instrumental variables (so called fixed-effects
  regressions), or models with marginal effects (from **mfx**). See
  details in section *Model Components* .May be abbreviated. Note that
  the *conditional* component also refers to the *count* or *mean*
  component - names may differ, depending on the modeling package. There
  are three convenient shortcuts (not applicable to *all* model
  classes):

  - `component = "all"` returns all possible parameters.

  - If `component = "location"`, location parameters such as
    `conditional`, `zero_inflated`, `smooth_terms`, or `instruments` are
    returned (everything that are fixed or random effects - depending on
    the `effects` argument - but no auxiliary parameters).

  - For `component = "distributional"` (or `"auxiliary"`), components
    like `sigma`, `dispersion`, `beta` or `precision` (and other
    auxiliary parameters) are returned.

## Value

A list of parameter names. The returned list may have following
elements:

- `conditional`, the "fixed effects" part from the model.

- `smooth_terms`, the smooth parameters.

## Examples

``` r
data(mtcars)
m <- mgcv::gam(mpg ~ s(hp) + gear, data = mtcars)
find_parameters(m)
#> $conditional
#> [1] "(Intercept)" "gear"       
#> 
#> $smooth_terms
#> [1] "s(hp)"
#> 
```
