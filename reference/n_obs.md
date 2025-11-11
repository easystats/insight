# Get number of observations from a model

This method returns the number of observation that were used to fit the
model, as numeric value.

## Usage

``` r
n_obs(x, ...)

# S3 method for class 'glm'
n_obs(x, disaggregate = FALSE, ...)

# S3 method for class 'svyolr'
n_obs(x, weighted = FALSE, ...)

# S3 method for class 'afex_aov'
n_obs(x, shape = c("long", "wide"), ...)

# S3 method for class 'stanmvreg'
n_obs(x, select = NULL, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- disaggregate:

  For binomial models with aggregated data, `n_obs()` returns the number
  of data rows by default. If `disaggregate = TRUE`, the total number of
  trials is returned instead (determined by summing the results of
  [`weights()`](https://rdrr.io/r/stats/weights.html) for aggregated
  data, which will be either the weights input for proportion success
  response or the row sums of the response matrix if matrix response,
  see 'Examples').

- weighted:

  For survey designs, returns the weighted sample size.

- shape:

  Return long or wide data? Only applicable in repeated measures
  designs.

- select:

  Optional name(s) of response variables for which to extract values.
  Can be used in case of regression models with multiple response
  variables.

## Value

The number of observations used to fit the model, or `NULL` if this
information is not available.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
n_obs(m)
#> [1] 32

data(cbpp, package = "lme4")
m <- glm(
  cbind(incidence, size - incidence) ~ period,
  data = cbpp,
  family = binomial(link = "logit")
)
n_obs(m)
#> [1] 56
n_obs(m, disaggregate = TRUE)
#> [1] 842
```
