# Standardize column order

Standardizes order of columns for dataframes and other objects from
*easystats* and *broom* ecosystem packages.

## Usage

``` r
standardize_column_order(data, ...)

# S3 method for class 'parameters_model'
standardize_column_order(data, style = "easystats", ...)
```

## Arguments

- data:

  A data frame. In particular, objects from *easystats* package
  functions like
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  or
  [`effectsize::effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.html)
  are accepted, but also data frames returned by
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) are
  valid objects.

- ...:

  Currently not used.

- style:

  Standardization can either be based on the naming conventions from the
  [easystats-project](https://easystats.github.io/easystats/), or on
  **broom**'s naming scheme.

## Value

A data frame, with standardized column order.

## Examples

``` r
# easystats conventions
df1 <- cbind.data.frame(
  CI_low      = -2.873,
  t           = 5.494,
  CI_high     = -1.088,
  p           = 0.00001,
  Parameter   = -1.980,
  CI          = 0.95,
  df          = 29.234,
  Method      = "Student's t-test"
)

standardize_column_order(df1, style = "easystats")
#>   Parameter   CI CI_low CI_high           Method     t     df     p
#> 1     -1.98 0.95 -2.873  -1.088 Student's t-test 5.494 29.234 1e-05

# broom conventions
df2 <- cbind.data.frame(
  conf.low   = -2.873,
  statistic  = 5.494,
  conf.high  = -1.088,
  p.value    = 0.00001,
  estimate   = -1.980,
  conf.level = 0.95,
  df         = 29.234,
  method     = "Student's t-test"
)

standardize_column_order(df2, style = "broom")
#>   estimate conf.level conf.low conf.high           method statistic     df
#> 1    -1.98       0.95   -2.873    -1.088 Student's t-test     5.494 29.234
#>   p.value
#> 1   1e-05
```
