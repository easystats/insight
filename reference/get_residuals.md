# Extract model residuals

Returns the residuals from regression models.

## Usage

``` r
get_residuals(x, ...)

# Default S3 method
get_residuals(x, weighted = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Passed down to
  [`residuals()`](https://rdrr.io/r/stats/residuals.html), if possible.

- weighted:

  Logical, if `TRUE`, returns weighted residuals.

- verbose:

  Toggle warnings and messages.

## Value

The residuals, or `NULL` if this information could not be accessed.

## Note

This function returns the default type of residuals, i.e. for the
response from linear models, the deviance residuals for models of class
`glm` etc. To access different types, pass down the `type` argument (see
'Examples').

This function is a robust alternative to
[`residuals()`](https://rdrr.io/r/stats/residuals.html), as it works for
some special model objects that otherwise do not respond properly to
calling [`residuals()`](https://rdrr.io/r/stats/residuals.html).

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_residuals(m)
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>          -1.0559619          -0.2281383          -3.4822509           0.7514545 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>           2.0342659          -1.7531855          -1.9437064           0.9420887 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>          -0.7877660          -0.7181129          -2.1181129           1.7794773 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>           1.5757124          -0.3619692          -0.3898093           0.1750587 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>           4.2185956           5.7281850           1.8290601           6.0432610 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>          -4.3115276          -0.9060247          -1.4819660          -2.0671872 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>           3.8490446          -0.2321023          -0.3424249           1.4979307 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>          -1.7422533          -1.8690068          -1.2437064          -3.3889219 

m <- glm(vs ~ wt + cyl + mpg, data = mtcars, family = binomial())
get_residuals(m) # type = "deviance" by default
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>         -0.63287224         -0.81557679          0.24153475          1.23515879 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>         -0.09537483          1.09436219         -0.09536895          0.08620842 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>          0.09512715          1.08274703          1.12137225         -0.17947784 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>         -0.12620688         -0.12450351         -0.53935361         -0.64596525 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>         -0.67827839          0.20117929          0.40994430          0.28751494 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>          0.21446014         -0.09381913         -0.08439431         -0.12504287 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>         -0.15299615          0.31935163         -2.59315645          0.45762007 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>         -0.06384435         -0.70853216         -0.09761985          0.15116709 
get_residuals(m, type = "response")
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>        -0.181485065        -0.282930670         0.028748195         0.533645229 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>        -0.004537852         0.450537255        -0.004537293         0.003709050 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>         0.004514367         0.443545885         0.466735567        -0.015977138 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>        -0.007932459        -0.007720604        -0.135367880        -0.188308998 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>        -0.205490858         0.020033170         0.080593721         0.040489884 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>         0.022734170        -0.004391345        -0.003554867        -0.007787380 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>        -0.011635687         0.049714423        -0.965342121         0.099412603 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>        -0.002035975        -0.221984564        -0.004753484         0.011360719 
```
