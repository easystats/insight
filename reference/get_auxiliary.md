# Get auxiliary parameters from models

Returns the requested auxiliary parameters from models, like dispersion,
sigma, or beta...

## Usage

``` r
get_auxiliary(
  x,
  type = "sigma",
  summary = TRUE,
  centrality = "mean",
  verbose = TRUE,
  ...
)

get_dispersion(x, ...)

# Default S3 method
get_dispersion(x, ...)
```

## Arguments

- x:

  A model.

- type:

  The name of the auxiliary parameter that should be retrieved.
  `"sigma"` is available for most models, `"dispersion"` for models of
  class `glm`, `glmerMod` or `glmmTMB` as well as `brmsfit`. `"beta"`
  and other parameters are currently only returned for `brmsfit` models.
  See 'Details'.

- summary:

  Logical, indicates whether the full posterior samples
  (`summary = FALSE`)) or the summarized centrality indices of the
  posterior samples (`summary = TRUE`)) should be returned as estimates.

- centrality:

  Only for models with posterior samples, and when `summary = TRUE`. In
  this case, `centrality = "mean"` would calculate means of posterior
  samples for each parameter, while `centrality = "median"` would use
  the more robust median value as measure of central tendency.

- verbose:

  Toggle warnings.

- ...:

  Currently not used.

## Value

The requested auxiliary parameter, or `NULL` if this information could
not be accessed.

## Details

Currently, only sigma and the dispersion parameter are returned, and
only for a limited set of models.

## Sigma Parameter

See
[`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md).

## Dispersion Parameter

There are many different definitions of "dispersion", depending on the
context. `get_auxiliary()` returns the dispersion parameters that
usually can be considered as variance-to-mean ratio for generalized
(linear) mixed models. Exceptions are models of class `glmmTMB`, where
the dispersion equals σ². In detail, the computation of the dispersion
parameter for generalized linear models is the ratio of the sum of the
squared working-residuals and the residual degrees of freedom. For mixed
models of class `glmer`, the dispersion parameter is also called φ and
is the ratio of the sum of the squared Pearson-residuals and the
residual degrees of freedom. For models of class `glmmTMB`, dispersion
is σ².

## brms-models

For models of class `brmsfit`, there are different options for the
`type` argument. See a list of supported auxiliary parameters here:
[`find_parameters.BGGM()`](https://easystats.github.io/insight/reference/find_parameters.BGGM.md).

## Examples

``` r
# from ?glm
clotting <- data.frame(
  u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
  lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
  lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
)
model <- glm(lot1 ~ log(u), data = clotting, family = Gamma())
get_auxiliary(model, type = "dispersion") # same as summary(model)$dispersion
#> [1] 0.002446059
```
