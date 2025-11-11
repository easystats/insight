# Get the values from model weights

Returns weighting variable of a model.

## Usage

``` r
get_weights(x, ...)

# Default S3 method
get_weights(x, remove_na = FALSE, null_as_ones = FALSE, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Used for objects from package **survey**, to pass the `source`
  argument to
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md).
  See related documentation of that argument for further details.

- remove_na:

  Logical, if `TRUE`, removes possible missing values.

- null_as_ones:

  Logical, if `TRUE`, will return a vector of `1` if no weights were
  specified in the model (as if the weights were all set to 1).

## Value

The weighting variable, or `NULL` if no weights were specified. If the
weighting variable should also be returned (instead of `NULL`) when all
weights are set to 1 (i.e. no weighting), set `null_as_ones = TRUE`.

## Examples

``` r
data(mtcars)
set.seed(123)
mtcars$weight <- rnorm(nrow(mtcars), 1, .3)

# LMs
m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
get_weights(m)
#>  [1] 0.8318573 0.9309468 1.4676125 1.0211525 1.0387863 1.5145195 1.1382749
#>  [8] 0.6204816 0.7939441 0.8663014 1.3672245 1.1079441 1.1202314 1.0332048
#> [15] 0.8332477 1.5360739 1.1493551 0.4100149 1.2104068 0.8581626 0.6796529
#> [22] 0.9346075 0.6921987 0.7813326 0.8124882 0.4939920 1.2513361 1.0460119
#> [29] 0.6585589 1.3761445 1.1279393 0.9114786

get_weights(lm(mpg ~ wt, data = mtcars), null_as_ones = TRUE)
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# GLMs
m <- glm(vs ~ disp + mpg, data = mtcars, weights = weight, family = quasibinomial)
get_weights(m)
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>           0.8318573           0.9309468           1.4676125           1.0211525 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>           1.0387863           1.5145195           1.1382749           0.6204816 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>           0.7939441           0.8663014           1.3672245           1.1079441 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>           1.1202314           1.0332048           0.8332477           1.5360739 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>           1.1493551           0.4100149           1.2104068           0.8581626 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>           0.6796529           0.9346075           0.6921987           0.7813326 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>           0.8124882           0.4939920           1.2513361           1.0460119 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>           0.6585589           1.3761445           1.1279393           0.9114786 
m <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = weight, family = binomial)
get_weights(m)
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>            8.318573            9.309468           11.740900            9.190373 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>           11.426650           13.630675           12.521023            4.963853 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>            6.351553            8.663014           13.672245           12.187386 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>           12.322546           11.365253            9.165724           16.896813 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>           12.642907            3.280119            9.683254            6.865301 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>            4.757570           10.280683            7.614185            8.594659 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>            8.937370            3.951936           11.262025            9.414107 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>            8.561266           15.137589           14.663210            7.291828 
```
