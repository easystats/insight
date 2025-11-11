# Get model parameters

Returns the coefficients (or posterior samples for Bayesian models) from
a model. See the documentation for your object's class:

- [Bayesian
  models](https://easystats.github.io/insight/reference/get_parameters.BGGM.md)
  (**rstanarm**, **brms**, **MCMCglmm**, ...)

- [Estimated marginal
  means](https://easystats.github.io/insight/reference/get_parameters.emmGrid.md)
  (**emmeans**)

- [Generalized additive
  models](https://easystats.github.io/insight/reference/get_parameters.gamm.md)
  (**mgcv**, **VGAM**, ...)

- [Marginal effects
  models](https://easystats.github.io/insight/reference/get_parameters.betamfx.md)
  (**mfx**)

- [Mixed
  models](https://easystats.github.io/insight/reference/get_parameters.glmmTMB.md)
  (**lme4**, **glmmTMB**, **GLMMadaptive**, ...)

- [Zero-inflated and hurdle
  models](https://easystats.github.io/insight/reference/get_parameters.zeroinfl.md)
  (**pscl**, ...)

- [Models with special
  components](https://easystats.github.io/insight/reference/get_parameters.betareg.md)
  (**betareg**, **MuMIn**, ...)

- [Hypothesis
  tests](https://easystats.github.io/insight/reference/get_parameters.htest.md)
  (`htest`)

## Usage

``` r
get_parameters(x, ...)

# Default S3 method
get_parameters(x, verbose = TRUE, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- verbose:

  Toggle messages and warnings.

## Value

- for non-Bayesian models, a data frame with two columns: the parameter
  names and the related point estimates.

- for Anova ([`aov()`](https://rdrr.io/r/stats/aov.html)) with error
  term, a list of parameters for the conditional and the random effects
  parameters

## Details

In most cases when models either return different "effects" (fixed,
random) or "components" (conditional, zero-inflated, ...), the arguments
`effects` and `component` can be used.

`get_parameters()` is comparable to
[`coef()`](https://rdrr.io/r/stats/coef.html), however, the coefficients
are returned as data frame (with columns for names and point estimates
of coefficients). For Bayesian models, the posterior samples of
parameters are returned.

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
