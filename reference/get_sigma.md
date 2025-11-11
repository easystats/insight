# Get residual standard deviation from models

Returns `sigma`, which corresponds the estimated standard deviation of
the residuals. This function extends the
[`sigma()`](https://rdrr.io/r/stats/sigma.html) base R generic for
models that don't have implemented it. It also computes the confidence
interval (CI), which is stored as an attribute.

Sigma is a key-component of regression models, and part of the so-called
auxiliary parameters that are estimated. Indeed, linear models for
instance assume that the residuals comes from a normal distribution with
mean 0 and standard deviation `sigma`. See the details section below for
more information about its interpretation and calculation.

## Usage

``` r
get_sigma(x, ci = NULL, verbose = TRUE, ...)
```

## Arguments

- x:

  A model.

- ci:

  Scalar, the CI level. The default (`NULL`) returns no CI.

- verbose:

  Toggle messages and warnings.

- ...:

  For internal use.

## Value

The residual standard deviation (sigma), or `NULL` if this information
could not be accessed.

## Interpretation of Sigma

The residual standard deviation, σ, indicates that the predicted outcome
will be within +/- σ units of the linear predictor for approximately
`68%` of the data points (*Gelman, Hill & Vehtari 2020, p.84*). In other
words, the residual standard deviation indicates the accuracy for a
model to predict scores, thus it can be thought of as "a measure of the
average distance each observation falls from its prediction from the
model" (*Gelman, Hill & Vehtari 2020, p.168*). σ can be considered as a
measure of the unexplained variation in the data, or of the precision of
inferences about regression coefficients.

## Calculation of Sigma

By default, `get_sigma()` tries to extract sigma by calling
[`stats::sigma()`](https://rdrr.io/r/stats/sigma.html). If the
model-object has no [`sigma()`](https://rdrr.io/r/stats/sigma.html)
method, the next step is calculating sigma as square-root of the
model-deviance divided by the residual degrees of freedom. Finally, if
even this approach fails, and `x` is a mixed model, the residual
standard deviation is accessed using the square-root from
[`get_variance_residual()`](https://easystats.github.io/insight/reference/get_variance.md).

## References

Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other
Stories. Cambridge University Press.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_sigma(m)
#> [1] 2.608133
#> attr(,"class")
#> [1] "insight_aux" "numeric"    
```
