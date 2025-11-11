# Get model parameters from Bayesian models

Returns the coefficients (or posterior samples for Bayesian models) from
a model.

## Usage

``` r
# S3 method for class 'BGGM'
get_parameters(
  x,
  component = "correlation",
  summary = FALSE,
  centrality = "mean",
  ...
)

# S3 method for class 'BFBayesFactor'
get_parameters(
  x,
  effects = "all",
  component = "all",
  iterations = 4000,
  progress = FALSE,
  verbose = TRUE,
  summary = FALSE,
  centrality = "mean",
  ...
)

# S3 method for class 'brmsfit'
get_parameters(
  x,
  effects = "fixed",
  component = "all",
  parameters = NULL,
  summary = FALSE,
  centrality = "mean",
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

- summary:

  Logical, indicates whether the full posterior samples
  (`summary = FALSE`)) or the summarized centrality indices of the
  posterior samples (`summary = TRUE`)) should be returned as estimates.

- centrality:

  Only for models with posterior samples, and when `summary = TRUE`. In
  this case, `centrality = "mean"` would calculate means of posterior
  samples for each parameter, while `centrality = "median"` would use
  the more robust median value as measure of central tendency.

- ...:

  Currently only used for models of class `brmsfit`, where a `variable`
  argument can be used, which is directly passed to the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  (i.e., `as.data.frame(x, variable = variable)`).

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

- iterations:

  Number of posterior draws.

- progress:

  Display progress.

- verbose:

  Toggle messages and warnings.

- parameters:

  Regular expression pattern that describes the parameters that should
  be returned.

## Value

The posterior samples from the requested parameters as data frame. If
`summary = TRUE`, returns a data frame with two columns: the parameter
names and the related point estimates (based on `centrality`).

## Details

In most cases when models either return different "effects" (fixed,
random) or "components" (conditional, zero-inflated, ...), the arguments
`effects` and `component` can be used.

## BFBayesFactor Models

Note that for `BFBayesFactor` models (from the **BayesFactor** package),
posteriors are only extracted from the first numerator model (i.e.,
`model[1]`). If you want to apply some function `foo()` to another model
stored in the `BFBayesFactor` object, index it directly, e.g.
`foo(model[2])`, `foo(1/model[5])`, etc. See also
[`bayestestR::weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.html).

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
get_parameters(m)
#>     Parameter   Estimate
#> 1 (Intercept) 38.7460642
#> 2          wt -3.2463673
#> 3         cyl -1.3641033
#> 4          vs  0.5241721
```
