# Obtain finite-population-adjusted variance-covariance matrix

This function returns the variance-covariance matrix as returned by
[`vcov()`](https://rdrr.io/r/stats/vcov.html), but with finite
population correction (FPC) applied.

## Usage

``` r
vcovFPC(model, ...)

# S3 method for class 'merMod'
vcovFPC(model, population_size = NULL, cluster_size = NULL, kr = FALSE, ...)
```

## Arguments

- model:

  A model inheriting from `lm`, `glm`, `merMod` or `glmmTMB` (2-level
  only).

- ...:

  Not used.

- population_size:

  The finite population size (for mixed models: for level-1).

- cluster_size:

  The finite population size for level-2 (cluster groups).

- kr:

  Logical, if `TRUE`, also applies Kenward-Roger adjustment to the
  returned variance-covariance matrix.

## Value

The variance-covariance matrix of the fixed effect estimates, as
returned by [`vcov()`](https://rdrr.io/r/stats/vcov.html), but with FPC
applied.

## Details

The FPC is defined as:

\$\$FPC = \frac{N - n}{N - 1}\$\$

FPC for multilevel models is based on the method described by Lai et al.
(2018).

## References

Lai, M. H., Kwok, O. M., Hsiao, Y. Y., & Cao, Q. (2018). Finite
population correction for two-level hierarchical linear models.
*Psychological methods, 23*(1), 94.
