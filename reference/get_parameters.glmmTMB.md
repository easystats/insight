# Get model parameters from mixed models

Returns the coefficients from a model.

## Usage

``` r
# S3 method for class 'glmmTMB'
get_parameters(x, effects = "fixed", component = "all", ...)
```

## Arguments

- x:

  A fitted model.

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

- ...:

  Currently not used.

## Value

If `effects = "fixed"`, a data frame with two columns: the parameter
names and the related point estimates. If `effects = "random"`, a list
of data frames with the random effects (as returned by `ranef()`),
unless the random effects have the same simplified structure as fixed
effects (e.g. for models from **MCMCglmm**).

## Details

In most cases when models either return different "effects" (fixed,
random) or "components" (conditional, zero-inflated, ...), the arguments
`effects` and `component` can be used. See details in the section *Model
Components*.

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
data(Salamanders, package = "glmmTMB")
m <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
get_parameters(m)
#>     Parameter   Estimate     Component
#> 1 (Intercept)  0.0878965   conditional
#> 2     minedno  1.1419232   conditional
#> 3 (Intercept)  1.1393044 zero_inflated
#> 4     minedno -1.7360669 zero_inflated
```
