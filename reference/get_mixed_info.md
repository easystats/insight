# Extract various information from mixed models

Small helper function that returns essential information on
coefficients, model matrix, variance and correlation parameters, as well
as random effects parameters of mixed effects models as list. Mainly
used for internal purposes.

## Usage

``` r
get_mixed_info(model, ...)

# Default S3 method
get_mixed_info(model, verbose = TRUE, ...)

# S3 method for class 'glmmTMB'
get_mixed_info(model, component = "conditional", verbose = TRUE, ...)
```

## Arguments

- model:

  A mixed effects model.

- ...:

  Not used.

- verbose:

  Toggle off warnings.

- component:

  For `glmmTMB` and `MixMod` models, this argument specifies the
  component of the model to extract. Possible values are `"conditional"`
  (default) and `"zero_inflated"` (or `"zi"`).

## Value

This function returns a list that has the same structure for any mixed
models with the following components:

- `beta` (contains fixed effects, as returned by `lme4::fixef(model)`)

- `X` (contains the model matrix, as returned by
  `lme4::getME(model, "X")`)

- `vc` (contains the variance and correlation parameters, as returned by
  `lme4::VarCorr(model)`)

- `re` (random effects parameters, as returned by `lme4::ranef(model)`)
