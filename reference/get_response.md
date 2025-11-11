# Get the values from the response variable

Returns the values the response variable(s) from a model object. If the
model is a multivariate response model, a data frame with values from
all response variables is returned.

## Usage

``` r
get_response(x, ...)

# Default S3 method
get_response(
  x,
  select = NULL,
  as_proportion = TRUE,
  source = "environment",
  verbose = TRUE,
  ...
)

# S3 method for class 'nestedLogit'
get_response(x, dichotomies = FALSE, source = "environment", ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- select:

  Optional name(s) of response variables for which to extract values.
  Can be used in case of regression models with multiple response
  variables.

- as_proportion:

  Logical, if `TRUE` and the response value is a proportion (e.g.
  `y1 / y2`), then the returned response value will be a vector with the
  result of this proportion. Else, always a data frame is returned.

- source:

  String, indicating from where data should be recovered. If
  `source = "environment"` (default), data is recovered from the
  environment (e.g. if the data is in the workspace). This option is
  usually the fastest way of getting data and ensures that the original
  variables used for model fitting are returned. Note that always the
  *current* data is recovered from the environment. Hence, if the data
  was modified *after* model fitting (e.g., variables were recoded or
  rows filtered), the returned data may no longer equal the model data.
  If `source = "frame"` (or `"mf"`), the data is taken from the model
  frame. Any transformed variables are back-transformed, if possible.
  This option returns the data even if it is not available in the
  environment, however, in certain edge cases back-transforming to the
  original data may fail. If `source = "environment"` fails to recover
  the data, it tries to extract the data from the model frame; if
  `source = "frame"` and data cannot be extracted from the model frame,
  data will be recovered from the environment. Both ways only returns
  observations that have no missing data in the variables used for model
  fitting.

  For objects from package **survey**, `"mf"` extracts data from the
  model frame of the survey design object, which is usually equivalent
  to the original data. `source = "environment"` extracts data from the
  model-object in the environment, which usually includes processed
  variables (like the `"(weights)"` variable for weights).

- verbose:

  Toggle warnings.

- dichotomies:

  Logical, if model is a `nestedLogit` objects, returns the response
  values for the dichotomies.

## Value

The values of the response variable, as vector, or a data frame if `x`
has more than one defined response variable.

## Examples

``` r
data(cbpp)
cbpp$trials <- cbpp$size - cbpp$incidence
dat <<- cbpp

m <- glm(cbind(incidence, trials) ~ period, data = dat, family = binomial)
head(get_response(m))
#>   incidence trials
#> 1         2     12
#> 2         3      9
#> 3         4      5
#> 4         0      5
#> 5         3     19
#> 6         1     17
get_response(m, select = "incidence")
#> [1] 2

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_response(m)
#>  [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4
#> [16] 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7
#> [31] 15.0 21.4
```
