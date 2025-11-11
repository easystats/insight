# Get statistic associated with estimates

Returns the statistic (*t*, `z`, ...) for model estimates. In most
cases, this is the related column from `coef(summary())`.

## Usage

``` r
get_statistic(x, ...)

# Default S3 method
get_statistic(x, column_index = 3, verbose = TRUE, ...)

# S3 method for class 'glmmTMB'
get_statistic(x, component = "all", ...)

# S3 method for class 'emmGrid'
get_statistic(x, ci = 0.95, adjust = "none", merge_parameters = FALSE, ...)

# S3 method for class 'gee'
get_statistic(x, robust = FALSE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Currently not used.

- column_index:

  For model objects that have no defined `get_statistic()` method yet,
  the default method is called. This method tries to extract the
  statistic column from `coef(summary())`, where the index of the column
  that is being pulled is `column_index`. Defaults to 3, which is the
  default statistic column for most models' summary-output.

- verbose:

  Toggle warnings.

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

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`). Currently
  only applies to objects of class `emmGrid`.

- adjust:

  Character value naming the method used to adjust p-values or
  confidence intervals. See
  [`?emmeans::summary.emmGrid`](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)
  for details.

- merge_parameters:

  Logical, if `TRUE` and `x` has multiple columns for parameter names
  (like `emmGrid` objects may have), these are merged into a single
  parameter column, with parameters names and values as values.

- robust:

  Logical, if `TRUE`, test statistic based on robust standard errors is
  returned.

## Value

A data frame with the model's parameter names and the related test
statistic.

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

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_statistic(m)
#>     Parameter  Statistic
#> 1 (Intercept) 11.3994647
#> 2          wt -4.1204121
#> 3         cyl -2.2234114
#> 4          vs  0.3221477
```
