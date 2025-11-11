# Model Deviance

Returns model deviance (see
[`stats::deviance()`](https://rdrr.io/r/stats/deviance.html)).

## Usage

``` r
get_deviance(x, ...)

# Default S3 method
get_deviance(x, verbose = TRUE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Not used.

- verbose:

  Toggle warnings and messages.

## Value

The model deviance.

## Details

For GLMMs of class `glmerMod`, `glmmTMB` or `MixMod`, the *absolute
unconditional* deviance is returned (see 'Details' in
`?lme4::merMod-class`), i.e. minus twice the log-likelihood. To get the
*relative conditional* deviance (relative to a saturated model,
conditioned on the conditional modes of random effects), use
[`deviance()`](https://rdrr.io/r/stats/deviance.html). The value
returned `get_deviance()` usually equals the deviance-value from the
[`summary()`](https://rdrr.io/r/base/summary.html).

## Examples

``` r
data(mtcars)
x <- lm(mpg ~ cyl, data = mtcars)
get_deviance(x)
#> [1] 308.3342
```
