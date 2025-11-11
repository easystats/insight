# Count number of parameters in a model

Returns the number of parameters (coefficients) of a model.

## Usage

``` r
n_parameters(x, ...)

# Default S3 method
n_parameters(x, remove_nonestimable = FALSE, ...)

# S3 method for class 'merMod'
n_parameters(x, effects = "fixed", remove_nonestimable = FALSE, ...)

# S3 method for class 'glmmTMB'
n_parameters(
  x,
  effects = "fixed",
  component = "all",
  remove_nonestimable = FALSE,
  ...
)
```

## Arguments

- x:

  A statistical model.

- ...:

  Arguments passed to or from other methods.

- remove_nonestimable:

  Logical, if `TRUE`, removes (i.e. does not count) non-estimable
  parameters (which may occur for models with rank-deficient model
  matrix).

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

## Value

The number of parameters in the model.

## Note

This function returns the number of parameters for the fixed effects by
default, as returned by `find_parameters(x, effects = "fixed")`. It does
not include *all* estimated model parameters, i.e. auxiliary parameters
like sigma or dispersion are not counted. To get the number of *all
estimated* parameters, use `get_df(x, type = "model")`.

## Examples

``` r
data(iris)
model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
n_parameters(model)
#> [1] 6
```
