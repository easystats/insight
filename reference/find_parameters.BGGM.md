# Find names of model parameters from Bayesian models

Returns the names of model parameters, like they typically appear in the
[`summary()`](https://rdrr.io/r/base/summary.html) output. For Bayesian
models, the parameter names equal the column names of the posterior
samples after coercion from
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
# S3 method for class 'BGGM'
find_parameters(x, component = "correlation", flatten = FALSE, ...)

# S3 method for class 'brmsfit'
find_parameters(
  x,
  effects = "all",
  component = "all",
  flatten = FALSE,
  parameters = NULL,
  ...
)
```

## Arguments

- x:

  A fitted model.

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

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

- ...:

  Currently not used.

- effects:

  Should variables for fixed effects (`"fixed"`), random effects
  (`"random"`) or both (`"all"`) be returned? Only applies to mixed
  models. May be abbreviated.

  For models of from packages **brms** or **rstanarm** there are
  additional options:

  - `"fixed"` returns fixed effects.

  - `"random_variance"` return random effects parameters (variance and
    correlation components, e.g. those parameters that start with `sd_`
    or `cor_`).

  - `"grouplevel"` returns random effects group level estimates, i.e.
    those parameters that start with `r_`.

  - `"random"` returns both `"random_variance"` and `"grouplevel"`.

  - `"all"` returns fixed effects and random effects variances.

  - `"full"` returns all parameters.

- parameters:

  Regular expression pattern that describes the parameters that should
  be returned.

## Value

A list of parameter names. For simple models, only one list-element,
`conditional`, is returned. For more complex models, the returned list
may have following elements:

- `conditional`, the "fixed effects" part from the model

- `random`, the "random effects" part from the model

- `zero_inflated`, the "fixed effects" part from the zero-inflation
  component of the model. For **brms**, this is named `zi`.

- `zero_inflated_random`, the "random effects" part from the
  zero-inflation component of the model. For **brms**, this is named
  `zi_random`.

- `smooth_terms`, the smooth parameters

Furthermore, some models, especially from **brms**, can also return
other auxiliary (distributional) parameters. These may be one of the
following:

- `sigma`, the residual standard deviation (auxiliary parameter)

- `dispersion`, the dispersion parameters (auxiliary parameter)

- `beta`, the beta parameter (auxiliary parameter)

- and any pre-defined or arbitrary distributional parameter for models
  from package **brms**, like `mu`, `ndt`, `kappa`, etc.

Models of class **BGGM** additionally can return the elements
`correlation` and `intercept`.

Models of class **BFBayesFactor** additionally can return the element
`extra`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)
#> $conditional
#> [1] "(Intercept)" "wt"          "cyl"         "vs"         
#> 
```
