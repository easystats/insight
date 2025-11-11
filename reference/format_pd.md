# Probability of direction (pd) formatting

Probability of direction (pd) formatting

## Usage

``` r
format_pd(pd, stars = FALSE, stars_only = FALSE, name = "pd")
```

## Arguments

- pd:

  Probability of direction (pd).

- stars:

  Add significance stars (e.g., p \< .001\*\*\*). For Bayes factors, the
  thresholds for "significant" results are values larger than 3, 10, and
  30.

- stars_only:

  Return only significance stars.

- name:

  Name prefixing the text. Can be `NULL`.

## Value

A formatted string.

## Examples

``` r
format_pd(0.12)
#> [1] "pd = 12.00%"
format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), name = NULL)
#> [1] "12.00%" "100%"   "99.99%" "98.00%" "99.50%" "96.00%"
format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), stars = TRUE)
#> [1] "pd = 12.00%"    "pd = 100%***"   "pd = 99.99%***" "pd = 98.00%*"  
#> [5] "pd = 99.50%**"  "pd = 96.00%"   
```
