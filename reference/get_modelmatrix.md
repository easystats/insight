# Model Matrix

Creates a design matrix from the description. Any character variables
are coerced to factors.

## Usage

``` r
get_modelmatrix(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Passed down to other methods (mainly
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)).

## Examples

``` r
data(mtcars)

model <- lm(am ~ vs, data = mtcars)
get_modelmatrix(model)
#>                     (Intercept) vs
#> Mazda RX4                     1  0
#> Mazda RX4 Wag                 1  0
#> Datsun 710                    1  1
#> Hornet 4 Drive                1  1
#> Hornet Sportabout             1  0
#> Valiant                       1  1
#> Duster 360                    1  0
#> Merc 240D                     1  1
#> Merc 230                      1  1
#> Merc 280                      1  1
#> Merc 280C                     1  1
#> Merc 450SE                    1  0
#> Merc 450SL                    1  0
#> Merc 450SLC                   1  0
#> Cadillac Fleetwood            1  0
#> Lincoln Continental           1  0
#> Chrysler Imperial             1  0
#> Fiat 128                      1  1
#> Honda Civic                   1  1
#> Toyota Corolla                1  1
#> Toyota Corona                 1  1
#> Dodge Challenger              1  0
#> AMC Javelin                   1  0
#> Camaro Z28                    1  0
#> Pontiac Firebird              1  0
#> Fiat X1-9                     1  1
#> Porsche 914-2                 1  0
#> Lotus Europa                  1  1
#> Ford Pantera L                1  0
#> Ferrari Dino                  1  0
#> Maserati Bora                 1  0
#> Volvo 142E                    1  1
#> attr(,"assign")
#> [1] 0 1
```
