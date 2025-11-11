# Log-Likelihood and Log-Likelihood correction

A robust function to compute the log-likelihood of a model, as well as
individual log-likelihoods (for each observation) whenever possible. Can
be used as a replacement for
[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) out of the box,
as the returned object is of the same class (and it gives the same
results by default).

`get_loglikelihood_adjustment()` can be used to correct the
log-likelihood for models with transformed response variables. The
adjustment value can be added to the log-likelihood to get the corrected
value. This is done automatically in `get_loglikelihood()` if
`check_response = TRUE`.

## Usage

``` r
get_loglikelihood(x, ...)

loglikelihood(x, ...)

get_loglikelihood_adjustment(x)

# S3 method for class 'lm'
get_loglikelihood(
  x,
  estimator = "ML",
  REML = FALSE,
  check_response = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A model.

- ...:

  Passed down to [`logLik()`](https://rdrr.io/r/stats/logLik.html), if
  possible.

- estimator:

  Corresponds to the different estimators for the standard deviation of
  the errors. If `estimator="ML"` (default), the scaling is done by n
  (the biased ML estimator), which is then equivalent to using
  [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html). If
  `estimator="OLS"`, it returns the unbiased OLS estimator.
  `estimator="REML"` will give same results as `logLik(..., REML=TRUE)`.

- REML:

  Only for linear models. This argument is present for compatibility
  with [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html). Setting
  it to `TRUE` will overwrite the `estimator` argument and is thus
  equivalent to setting `estimator="REML"`. It will give the same
  results as `stats::logLik(..., REML=TRUE)`. Note that individual
  log-likelihoods are not available under REML.

- check_response:

  Logical, if `TRUE`, checks if the response variable is transformed
  (like [`log()`](https://rdrr.io/r/base/Log.html) or
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html)), and if so, returns a
  corrected log-likelihood. To get back to the original scale, the
  likelihood of the model is multiplied by the Jacobian/derivative of
  the transformation.

- verbose:

  Toggle warnings and messages.

## Value

`get_loglikelihood()` returns an object of class `"logLik"`, also
containing the log-likelihoods for each observation as a
`per_observation` attribute
(`attributes(get_loglikelihood(x))$per_observation`) when possible. The
code was partly inspired from the **nonnest2** package.

`get_loglikelihood_adjustment()` returns the adjustment value to be
added to the log-likelihood to correct for transformed response
variables, or `NULL` if the adjustment could not be computed.

## Examples

``` r
x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)

get_loglikelihood(x, estimator = "ML") # Equivalent to stats::logLik(x)
#> 'log Lik.' -101.0339 (df=5)
get_loglikelihood(x, estimator = "REML") # Equivalent to stats::logLik(x, REML=TRUE)
#> 'log Lik.' -107.0896 (df=5)
get_loglikelihood(x, estimator = "OLS")
#> 'log Lik.' -101.0611 (df=5)
```
