# Find names of model parameters

Returns the names of model parameters, like they typically appear in the
[`summary()`](https://rdrr.io/r/base/summary.html) output. For Bayesian
models, the parameter names equal the column names of the posterior
samples after coercion from
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html). See the
documentation for your object's class:

- [Bayesian
  models](https://easystats.github.io/insight/reference/find_parameters.BGGM.md)
  (**rstanarm**, **brms**, **MCMCglmm**, ...)

- [Generalized additive
  models](https://easystats.github.io/insight/reference/find_parameters.gamlss.md)
  (**mgcv**, **VGAM**, ...)

- [Marginal effects
  models](https://easystats.github.io/insight/reference/find_parameters.betamfx.md)
  (**mfx**)

- [Estimated marginal
  means](https://easystats.github.io/insight/reference/find_parameters.emmGrid.md)
  (**emmeans**)

- [Mixed
  models](https://easystats.github.io/insight/reference/find_parameters.glmmTMB.md)
  (**lme4**, **glmmTMB**, **GLMMadaptive**, ...)

- [Zero-inflated and hurdle
  models](https://easystats.github.io/insight/reference/find_parameters.zeroinfl.md)
  (**pscl**, ...)

- [Models with special
  components](https://easystats.github.io/insight/reference/find_parameters.averaging.md)
  (**betareg**, **MuMIn**, ...)

## Usage

``` r
find_parameters(x, ...)

# Default S3 method
find_parameters(x, flatten = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

- verbose:

  Toggle messages and warnings.

## Value

A list of parameter names. For simple models, only one list-element,
`conditional`, is returned.

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
a model:
[`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md),
[`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
[`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
and `find_parameters()`. There are some differences between those
functions, which are explained using following model. Note that some,
but not all of those functions return information about the *dependent*
and *independent* variables. In this example, we only show the
differences for the independent variables.

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

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  is comparable to
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  and also returns the original variable names, but excluded the
  *dependent* (response) variables. The return value would be `"gear"`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)
#> $conditional
#> [1] "(Intercept)" "wt"          "cyl"         "vs"         
#> 
```
