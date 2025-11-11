# Find auxiliary (distributional) parameters from models

Returns the names of all auxiliary / distributional parameters from
brms-models, like dispersion, sigma, kappa, phi, or beta...

## Usage

``` r
find_auxiliary(x, ...)

# Default S3 method
find_auxiliary(x, verbose = TRUE, ...)
```

## Arguments

- x:

  A model of class `brmsfit`.

- ...:

  Currently not used.

- verbose:

  Toggle warnings.

## Value

The names of all available auxiliary parameters used in the model, or
`NULL` if no auxiliary parameters were found.
