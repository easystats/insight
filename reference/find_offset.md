# Find possible offset terms in a model

Returns a character vector with the name(s) of offset terms.

## Usage

``` r
find_offset(x, as_term = FALSE)
```

## Arguments

- x:

  A fitted model.

- as_term:

  Logical, if `TRUE`, the offset is returned as term, including possible
  transformations, like `log(variable)`. If `FALSE` (default), only the
  variable name is returned.

## Value

A character vector with the name(s) of offset terms.

## Examples

``` r
# Generate some zero-inflated data
set.seed(123)
N <- 100 # Samples
x <- runif(N, 0, 10) # Predictor
off <- rgamma(N, 3, 2) # Offset variable
yhat <- -1 + x * 0.5 + log(off) # Prediction on log scale
dat <- data.frame(y = NA, x, logOff = log(off), raw_off = off)
dat$y <- rpois(N, exp(yhat)) # Poisson process
dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) # Zero-inflation process

m1 <- pscl::zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
find_offset(m1)
#> [1] "logOff"

m2 <- pscl::zeroinfl(
  y ~ offset(log(raw_off)) + x | 1,
  data = dat,
  dist = "poisson"
)
find_offset(m2)
#> [1] "raw_off"
find_offset(m2, as_term = TRUE)
#> [1] "log(raw_off)"

m3 <- pscl::zeroinfl(y ~ x | 1, data = dat, offset = logOff, dist = "poisson")
find_offset(m3)
#> [1] "logOff"
```
