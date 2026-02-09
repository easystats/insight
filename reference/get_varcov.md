# Get variance-covariance matrix from models

Returns the variance-covariance, as retrieved by
[`stats::vcov()`](https://rdrr.io/r/stats/vcov.html), but works for more
model objects that probably don't provide a
[`vcov()`](https://rdrr.io/r/stats/vcov.html)-method.

## Usage

``` r
get_varcov(x, ...)

# Default S3 method
get_varcov(x, verbose = TRUE, vcov = NULL, vcov_args = NULL, ...)

# S3 method for class 'glmgee'
get_varcov(x, verbose = TRUE, vcov = "robust", ...)

# S3 method for class 'hurdle'
get_varcov(
  x,
  component = "conditional",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'aov'
get_varcov(x, complete = FALSE, verbose = TRUE, ...)

# S3 method for class 'mixor'
get_varcov(x, effects = "all", verbose = TRUE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Currently not used.

- verbose:

  Toggle warnings.

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

    - Kenward-Roger approximation: `kenward-roger`. See
      [`?pbkrtest::vcovAdj`](https://rdrr.io/pkg/pbkrtest/man/kr-vcovAdj.html).

  Exceptions are following models:

  - Model of class `glmgee`, which have pre-defined options for the
    variance-covariance matrix calculation. These are `"robust"`,
    `"df-adjusted"`, `"model"`, `"bias-corrected"`, and `"jackknife"`.
    See
    [`?glmtoolbox::vcov.glmgee`](https://rdrr.io/pkg/glmtoolbox/man/vcov.glmgee.html)
    for details.

  - Model of class `glmmTMB` currently only support the `"HC0"` option.

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`.

- component:

  Should the complete variance-covariance matrix of the model be
  returned, or only for specific model components only (like count or
  zero-inflated model parts)? Applies to models with zero-inflated
  component, or models with precision (e.g. `betareg`) component.
  `component` may be one of `"conditional"`, `"zi"`, `"zero-inflated"`,
  `"dispersion"`, `"precision"`, or `"all"`. May be abbreviated. Note
  that the *conditional* component also refers to the *count* or *mean*
  component - names may differ, depending on the modeling package. See
  section *Model components* for details. For models of class `glmmTMB`,
  the `component` argument can also be `"full"`, to return the full
  variance-covariance matrix (including random effects, called `theta`).

- complete:

  Logical, if `TRUE`, for `aov`, returns the full variance-covariance
  matrix.

- effects:

  Should the complete variance-covariance matrix of the model be
  returned, or only for specific model parameters only? Currently only
  applies to models of class `mixor` and `MixMod`.

## Value

The variance-covariance matrix, as `matrix`-object.

## Note

`get_varcov()` tries to return the nearest positive definite matrix in
case of negative eigenvalues of the variance-covariance matrix. This
ensures that it is still possible, for instance, to calculate standard
errors of model parameters. A message is shown when the matrix is
negative definite and a corrected matrix is returned.

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
get_varcov(m)
#>             (Intercept)         wt        cyl         vs
#> (Intercept)   11.552774  0.1680680 -1.5843752 -4.7487893
#> wt             0.168068  0.6207461 -0.3301421 -0.2797924
#> cyl           -1.584375 -0.3301421  0.3764045  0.7257641
#> vs            -4.748789 -0.2797924  0.7257641  2.6475113

# vcov of zero-inflation component from hurdle-model
data("bioChemists", package = "pscl")
mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
get_varcov(mod, component = "zero_inflated")
#>               (Intercept)          ment
#> (Intercept)  0.0115917010 -0.0009744732
#> ment        -0.0009744732  0.0001561139

# robust vcov of, count component from hurdle-model
data("bioChemists", package = "pscl")
mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
get_varcov(
  mod,
  component = "conditional",
  vcov = "BS",
  vcov_args = list(R = 50)
)
#>                   count_(Intercept)     count_phd count_femWomen
#> count_(Intercept)       0.039696561 -0.0095445393  -0.0044100376
#> count_phd              -0.009544539  0.0029020956   0.0006759074
#> count_femWomen         -0.004410038  0.0006759074   0.0119929959
```
