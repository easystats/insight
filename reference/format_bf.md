# Bayes Factor formatting

Bayes Factor formatting

## Usage

``` r
format_bf(
  bf,
  stars = FALSE,
  stars_only = FALSE,
  inferiority_star = "°",
  name = "BF",
  protect_ratio = FALSE,
  na_reference = NA,
  exact = FALSE
)
```

## Arguments

- bf:

  Bayes Factor.

- stars:

  Add significance stars (e.g., p \< .001\*\*\*). For Bayes factors, the
  thresholds for "significant" results are values larger than 3, 10, and
  30.

- stars_only:

  Return only significance stars.

- inferiority_star:

  String, indicating the symbol that is used to indicate inferiority,
  i.e. when the Bayes Factor is smaller than one third (the thresholds
  are smaller than one third, 1/10 and 1/30).

- name:

  Name prefixing the text. Can be `NULL`.

- protect_ratio:

  Should values smaller than 1 be represented as ratios?

- na_reference:

  How to format missing values (`NA`).

- exact:

  Should very large or very small values be reported with a scientific
  format (e.g., 4.24e5), or as truncated values (as "\> 1000" and "\<
  1/1000").

## Value

A formatted string.

## Examples

``` r
bfs <- c(0.000045, 0.033, NA, 1557, 3.54)
format_bf(bfs)
#> [1] "BF < 0.001" "BF = 0.033" ""           "BF > 1000"  "BF = 3.54" 
format_bf(bfs, exact = TRUE, name = NULL)
#> [1] "4.50e-05" "0.033"    ""         "1.56e+03" "3.54"    
format_bf(bfs, stars = TRUE)
#> [1] "BF < 0.001°°°" "BF = 0.033°°°" ""              "BF > 1000***" 
#> [5] "BF = 3.54*"   
format_bf(bfs, protect_ratio = TRUE)
#> [1] "BF < 1/1000"  "BF = 1/30.30" ""             "BF > 1000"    "BF = 3.54"   
format_bf(bfs, protect_ratio = TRUE, exact = TRUE)
#> [1] "BF = 1/2.22e+04" "BF = 1/30.30"    ""                "BF = 1.56e+03"  
#> [5] "BF = 3.54"      
format_bf(bfs, na_reference = 1)
#> [1] "BF < 0.001" "BF = 0.033" "BF = 1.00"  "BF > 1000"  "BF = 3.54" 
```
