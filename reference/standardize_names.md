# Standardize column names

Standardize column names from data frames, in particular objects
returned from
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
so column names are consistent and the same for any model object.

## Usage

``` r
standardize_names(data, ...)

# S3 method for class 'parameters_model'
standardize_names(
  data,
  style = c("easystats", "broom"),
  ignore_estimate = FALSE,
  ...
)
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

- ignore_estimate:

  Logical, if `TRUE`, column names like `"mean"` or `"median"` will
  *not* be converted to `"Coefficient"` resp. `"estimate"`.

## Value

A data frame, with standardized column names.

## Details

This method is in particular useful for package developers or users who
use, e.g.,
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
in their own code or functions to retrieve model parameters for further
processing. As `model_parameters()` returns a data frame with varying
column names (depending on the input), accessing the required
information is probably not quite straightforward. In such cases,
`standardize_names()` can be used to get consistent, i.e. always the
same column names, no matter what kind of model was used in
`model_parameters()`.

For `style = "broom"`, column names are renamed to match **broom**'s
naming scheme, i.e. `Parameter` is renamed to `term`, `Coefficient`
becomes `estimate` and so on.

For `style = "easystats"`, when `data` is an object from
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
column names are converted from "broom"-style into "easystats"-style.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_parameters(model)

as.data.frame(mp)
#>     Parameter Coefficient        SE   CI    CI_low    CI_high         t
#> 1 (Intercept)   39.686261 1.7149840 0.95 36.178725 43.1937976 23.140893
#> 2          wt   -3.190972 0.7569065 0.95 -4.739020 -1.6429245 -4.215808
#> 3         cyl   -1.507795 0.4146883 0.95 -2.355928 -0.6596622 -3.635972
#>   df_error            p
#> 1       29 3.043182e-20
#> 2       29 2.220200e-04
#> 3       29 1.064282e-03
standardize_names(mp)
#>     Parameter Coefficient        SE   CI    CI_low    CI_high Statistic df
#> 1 (Intercept)   39.686261 1.7149840 0.95 36.178725 43.1937976 23.140893 29
#> 2          wt   -3.190972 0.7569065 0.95 -4.739020 -1.6429245 -4.215808 29
#> 3         cyl   -1.507795 0.4146883 0.95 -2.355928 -0.6596622 -3.635972 29
#>              p
#> 1 3.043182e-20
#> 2 2.220200e-04
#> 3 1.064282e-03
standardize_names(mp, style = "broom")
#>          term  estimate std.error conf.level  conf.low  conf.high statistic
#> 1 (Intercept) 39.686261 1.7149840       0.95 36.178725 43.1937976 23.140893
#> 2          wt -3.190972 0.7569065       0.95 -4.739020 -1.6429245 -4.215808
#> 3         cyl -1.507795 0.4146883       0.95 -2.355928 -0.6596622 -3.635972
#>   df.error      p.value
#> 1       29 3.043182e-20
#> 2       29 2.220200e-04
#> 3       29 1.064282e-03
```
