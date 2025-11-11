# Find sampling algorithm and optimizers

Returns information on the sampling or estimation algorithm as well as
optimization functions, or for Bayesian model information on chains,
iterations and warmup-samples.

## Usage

``` r
find_algorithm(x, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

## Value

A list with elements depending on the model.

For frequentist models:

- `algorithm`, for instance `"OLS"` or `"ML"`

- `optimizer`, name of optimizing function, only applies to specific
  models (like `gam`)

For frequentist mixed models:

- `algorithm`, for instance `"REML"` or `"ML"`

- `optimizer`, name of optimizing function

For Bayesian models:

- `algorithm`, the algorithm

- `chains`, number of chains

- `iterations`, number of iterations per chain

- `warmup`, number of warmups per chain

## Examples

``` r
data(sleepstudy, package = "lme4")
m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
find_algorithm(m)
#> $algorithm
#> [1] "REML"
#> 
#> $optimizer
#> [1] "nloptwrap"
#> 
# \donttest{
data(sleepstudy, package = "lme4")
m <- suppressWarnings(rstanarm::stan_lmer(
  Reaction ~ Days + (1 | Subject),
  data = sleepstudy,
  refresh = 0
))
find_algorithm(m)
#> $algorithm
#> [1] "sampling"
#> 
#> $chains
#> [1] 4
#> 
#> $iterations
#> [1] 2000
#> 
#> $warmup
#> [1] 1000
#> 
# }
```
