# Easystats columns

Returns all valid column names that are used or defined across easystats
packages as character vector.

## Usage

``` r
easystats_columns(select = "all")

broom_columns(select = "all")
```

## Arguments

- select:

  String, indicating which columns to return.

## Value

A character vector with all (or selected) column names that are in use
across the easystats-ecosystem, or broom-alike column names for
`broom_columns()`.

## Examples

``` r
easystats_columns("uncertainty")
#>  [1] "SE"                 "Std. Error"         "SD"                
#>  [4] "Deviance_error"     "CI"                 "CI_low"            
#>  [7] "CI_high"            "Difference_CI_low"  "Difference_CI_high"
#> [10] "CI_Method"          "CI_Distribution"    "CI_Iterations"     
#> [13] "Sum_Squares"        "Mean_Square"       
```
