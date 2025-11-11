# Get the data that was used to fit the model

This functions tries to get the data that was used to fit the model and
returns it as data frame.

## Usage

``` r
get_data(x, ...)

# Default S3 method
get_data(x, source = "environment", verbose = TRUE, ...)

# S3 method for class 'glmmTMB'
get_data(
  x,
  effects = "all",
  component = "all",
  source = "environment",
  verbose = TRUE,
  ...
)

# S3 method for class 'afex_aov'
get_data(x, shape = c("long", "wide"), ...)

# S3 method for class 'rma'
get_data(
  x,
  source = "environment",
  verbose = TRUE,
  include_interval = FALSE,
  transf = NULL,
  transf_args = NULL,
  ci = 0.95,
  ...
)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- source:

  String, indicating from where data should be recovered. If
  `source = "environment"` (default), data is recovered from the
  environment (e.g. if the data is in the workspace). This option is
  usually the fastest way of getting data and ensures that the original
  variables used for model fitting are returned. Note that always the
  *current* data is recovered from the environment. Hence, if the data
  was modified *after* model fitting (e.g., variables were recoded or
  rows filtered), the returned data may no longer equal the model data.
  If `source = "frame"` (or `"mf"`), the data is taken from the model
  frame. Any transformed variables are back-transformed, if possible.
  This option returns the data even if it is not available in the
  environment, however, in certain edge cases back-transforming to the
  original data may fail. If `source = "environment"` fails to recover
  the data, it tries to extract the data from the model frame; if
  `source = "frame"` and data cannot be extracted from the model frame,
  data will be recovered from the environment. Both ways only returns
  observations that have no missing data in the variables used for model
  fitting.

  For objects from package **survey**, `"mf"` extracts data from the
  model frame of the survey design object, which is usually equivalent
  to the original data. `source = "environment"` extracts data from the
  model-object in the environment, which usually includes processed
  variables (like the `"(weights)"` variable for weights).

- verbose:

  Toggle messages and warnings.

- effects:

  Should model data for fixed effects (`"fixed"`), random effects
  (`"random"`) or both (`"all"`) be returned? Only applies to mixed or
  gee models.

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

- shape:

  Return long or wide data? Only applicable in repeated measures
  designs.

- include_interval:

  For meta-analysis models, should normal-approximation confidence
  intervals be added for each response effect size?

- transf:

  For meta-analysis models, if intervals are included, a function
  applied to each response effect size and its interval.

- transf_args:

  For meta-analysis models, an optional list of arguments passed to the
  `transf` function.

- ci:

  For meta-analysis models, the Confidence Interval (CI) level if
  `include_interval = TRUE`. Default to 0.95 (95%).

## Value

The data that was used to fit the model.

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
data(cbpp, package = "lme4")
cbpp$trials <- cbpp$size - cbpp$incidence
m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
head(get_data(m))
#>   incidence trials period
#> 1         2     12      1
#> 2         3      9      2
#> 3         4      5      3
#> 4         0      5      4
#> 5         3     19      1
#> 6         1     17      2
```
