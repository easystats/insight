# Find names of model predictors

Returns the names of the predictor variables for the different parts of
a model (like fixed or random effects, zero-inflated component, ...).
Unlike
[`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md),
the names from `find_predictors()` match the original variable names
from the data that was used to fit the model.

## Usage

``` r
find_predictors(x, ...)

# Default S3 method
find_predictors(
  x,
  effects = "fixed",
  component = "all",
  flatten = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- effects:

  Should variables for fixed effects (`"fixed"`), random effects
  (`"random"`) or both (`"all"`) be returned? Only applies to mixed
  models. May be abbreviated.

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

- verbose:

  Toggle warnings.

## Value

A list of character vectors that represent the name(s) of the predictor
variables. Depending on the combination of the arguments `effects` and
`component`, the returned list can have following elements:

- `conditional`, the "fixed effects" terms from the model

- `random`, the "random effects" terms from the model

- `zero_inflated`, the "fixed effects" terms from the zero-inflation
  component of the model. For models from **brms**, this is named `zi`.

- `zero_inflated_random`, the "random effects" terms from the
  zero-inflation component of the model. For models from **brms**, this
  is named `zi_random`.

- `dispersion`, the dispersion terms

- `instruments`, for fixed-effects regressions like `ivreg`, `felm` or
  `plm`, the instrumental variables

- `correlation`, for models with correlation-component like `gls`, the
  variables used to describe the correlation structure

- `nonlinear`, for non-linear models (like models of class `nlmerMod` or
  `nls`), the staring estimates for the nonlinear parameters

- `smooth_terms` returns smooth terms, only applies to GAMs (or similar
  models that may contain smooth terms)

## Model components

Possible values for the `component` argument depend on the model class.
Following are valid options:

- `"all"`: returns all model components, applies to all models, but will
  only have an effect for models with more than just the conditional
  model component.

- `"conditional"`: only returns the conditional component, i.e. "fixed
  effects" terms from the model. Will only have an effect for models
  with more than just the conditional model component.

- `"smooth_terms"`: returns smooth terms, only applies to GAMs (or
  similar models that may contain smooth terms).

- `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.

- `"dispersion"`: returns the dispersion model component. This is common
  for models with zero-inflation or that can model the dispersion
  parameter.

- `"instruments"`: for instrumental-variable or some fixed effects
  regression, returns the instruments.

- `"nonlinear"`: for non-linear models (like models of class `nlmerMod`
  or `nls`), returns staring estimates for the nonlinear parameters.

- `"correlation"`: for models with correlation-component, like `gls`,
  the variables used to describe the correlation structure are returned.

- `"location"`: returns location parameters such as `conditional`,
  `zero_inflated`, `smooth_terms`, or `instruments` (everything that are
  fixed or random effects - depending on the `effects` argument - but no
  auxiliary parameters).

- `"distributional"` (or `"auxiliary"`): components like `sigma`,
  `dispersion`, `beta` or `precision` (and other auxiliary parameters)
  are returned.

**Special models**

Some model classes also allow rather uncommon options. These are:

- **mhurdle**: `"infrequent_purchase"`, `"ip"`, and `"auxiliary"`

- **BGGM**: `"correlation"` and `"intercept"`

- **BFBayesFactor**, **glmx**: `"extra"`

- **averaging**:`"conditional"` and `"full"`

- **mjoint**: `"survival"`

- **mfx**: `"precision"`, `"marginal"`

- **betareg**, **DirichletRegModel**: `"precision"`

- **mvord**: `"thresholds"` and `"correlation"`

- **clm2**: `"scale"`

- **selection**: `"selection"`, `"outcome"`, and `"auxiliary"`

- **lcmm**: `"membership"`, `"longitudinal"`, `"beta"`, `"splines"`, and
  `"linear"`

For models of class `brmsfit` (package **brms**), even more options are
possible for the `component` argument, which are not all documented in
detail here. It can be any pre-defined or arbitrary distributional
parameter, like `mu`, `ndt`, `kappa`, etc.

## Parameters, Variables, Predictors and Terms

There are four functions that return information about the variables in
a model: `find_predictors()`,
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
[`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
and
[`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md).
There are some differences between those functions, which are explained
using following model. Note that some, but not all of those functions
return information about the *dependent* and *independent* variables. In
this example, we only show the differences for the independent
variables.

    model <- lm(mpg ~ factor(gear), data = mtcars)

- `find_terms(model)` returns the model terms, i.e. how the variables
  were used in the model, e.g. applying transformations like
  [`factor()`](https://rdrr.io/r/base/factor.html),
  [`poly()`](https://rdrr.io/r/stats/poly.html) etc.
  [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  may return a variable name multiple times in case of multiple
  transformations. The return value would be `"factor(gear)"`.

- `find_parameters(model)` returns the names of the model parameters
  (coefficients). The return value would be `"(Intercept)"`,
  `"factor(gear)4"` and `"factor(gear)5"`.

- [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  returns the original variable names.
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  returns each variable name only once. The return value would be
  `"gear"`.

- `find_predictors()` is comparable to
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  and also returns the original variable names, but excluded the
  *dependent* (response) variables. The return value would be `"gear"`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_predictors(m)
#> $conditional
#> [1] "wt"  "cyl" "vs" 
#> 
```
