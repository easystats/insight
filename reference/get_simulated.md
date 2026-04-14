# Simulate values from fitted models

Simulate responses from fitted statistical models.

## Usage

``` r
get_simulated(x, ...)

# S3 method for class 'lm'
get_simulated(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  ...
)

# S3 method for class 'glmmTMB'
get_simulated(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  centrality = NULL,
  re.form = NULL,
  ...
)

# S3 method for class 'merMod'
get_simulated(
  x,
  data = NULL,
  iterations = 1,
  include_data = FALSE,
  seed = NULL,
  use.u = FALSE,
  re.form = NA,
  newparams = NULL,
  family = NULL,
  cluster.rand = stats::rnorm,
  allow.new.levels = FALSE,
  na.action = stats::na.pass,
  ...
)

# Default S3 method
get_simulated(x, ...)

# S3 method for class 'data.frame'
get_simulated(x, data = NULL, include_data = FALSE, ...)
```

## Arguments

- x:

  A model.

- ...:

  Additional arguments passed to the underlying prediction or simulation
  methods.

- data:

  An optional data frame in which to evaluate predictions before
  simulation. This can be a data grid created with
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

- iterations:

  Number of response vectors to simulate.

- include_data:

  Logical, if `TRUE`, `data` is returned alongside with the simulated
  draws. If `data = NULL`, the data used to fit the model is included.

- seed:

  An optional integer random seed.

- centrality:

  Function, indicating how to summarize aggregated simulated values when
  `data` is a data grid. Only applies to models from package `glmmTMB`,
  because a special handling for the `data` argument is required here.
  `simulate.glmmTMB()` always returns simulated draws for the full data
  that was used to fit the model. If a data grid is passed via `data`,
  `get_simulated.glmmTMB()` internally, first, filters the results using
  [`datawizard::data_match()`](https://easystats.github.io/datawizard/reference/data_match.html)
  with the model data and `data`. This may return more than one row of
  simulated draws per group defined in `data`, thus, in a second step,
  the filtered data are aggregated by the groups defined in `data`.
  Resulting estimates of simulated values are summarized using
  `centrylity`, which, by default is the mode for categorical, ordinal
  or count response variables, or the mean otherwise.

- re.form:

  For `glmmTMB` and `merMod` models, random effects formula passed to
  simulation (`NULL`, `NA` or `~0`).

- use.u:

  For `merMod` models, logical indicating whether to condition on the
  current conditional modes of the random effects when simulating.

- newparams:

  For `merMod` models, optional list or vector of alternative parameter
  values (e.g., fixed- and random-effect parameters) to be used for
  simulation instead of those from the fitted model.

- family:

  For `merMod` models, optional `family` object or function to override
  the model's family when simulating responses.

- cluster.rand:

  For `merMod` models, function used to generate random draws for the
  random effects (e.g., `rnorm`).

- allow.new.levels:

  For `merMod` models, logical indicating whether new levels of grouping
  variables are allowed in `data` when simulating.

- na.action:

  For `merMod` models, function specifying how to handle missing values
  in `data` before simulation (e.g., `"na.pass"`).

## Value

A data frame with one column per simulation (`iter_1`, `iter_2`, ...).
The attribute `seed` contains information about the RNG state used.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + cyl, data = mtcars)

# Simulations on the original data
get_simulated(m, iterations = 2, seed = 123)
#>                        iter_1    iter_2
#> Mazda RX4           20.840115 24.577394
#> Mazda RX4 Wag       20.874462 23.720068
#> Datsun 710          30.254034 28.361449
#> Hornet 4 Drive      20.561548 22.148611
#> Hornet Sportabout   16.978906 18.069150
#> Valiant             24.002184 19.439769
#> Duster 360          17.415541 15.446567
#> Merc 240D           20.227816 22.499015
#> Merc 230            21.840014 21.819848
#> Merc 280            18.518303 19.128717
#> Merc 280C           22.805397 16.413623
#> Merc 450SE          15.560473 20.205474
#> Merc 450SL          16.750563 18.823037
#> Merc 450SLC         15.846207 12.678428
#> Cadillac Fleetwood   9.444167  9.836885
#> Lincoln Continental 14.903996  9.117924
#> Chrysler Imperial   11.846395 12.570728
#> Fiat 128            21.585623 26.420892
#> Honda Civic         30.302404 29.152061
#> Toyota Corolla      26.585748 27.726353
#> Toyota Corona       23.047681 25.679265
#> Dodge Challenger    15.832026 19.905588
#> AMC Javelin         14.028630 16.083242
#> Camaro Z28          13.499129 19.264131
#> Pontiac Firebird    13.749816 11.378167
#> Fiat X1-9           23.149939 28.981555
#> Porsche 914-2       28.977433 27.144399
#> Lotus Europa        29.220929 29.381574
#> Ford Pantera L      14.586336 18.483250
#> Ferrari Dino        25.019688 20.510776
#> Maserati Bora       17.327085 15.376616
#> Volvo 142E          24.026578 22.168971

# Simulations on a data grid
dg <- get_datagrid(m, wt = c(2, 3), cyl = c(4, 6))
get_simulated(dg, m, iterations = 2, seed = 123)
#>       iter_1   iter_2
#> 1  27.388111 29.92209
#> 2  26.848084 26.68147
#> 3  30.056194 28.35244
#> 4  24.847144 26.92073
#> 5  23.613179 25.39065
#> 6  26.296615 23.66125
#> 7  21.691686 21.93047
#> 8  15.872139 18.96124
#> 9  15.971816 16.94976
#> 10 15.203005 15.37038
#> 11 28.954400 24.02788
#> 12 25.347306 23.88965
#> 13 24.067583 19.78967
#> 14 21.934703 27.21935
#> 15 18.838510 23.36710
#> 16 23.465496 15.99397
#> 17 18.770925 16.45827
#> 18 11.055293 14.90647
#> 19 16.520474 16.72230
#> 20 12.117759 13.11761
#> 21 20.054307 23.44636
#> 22 20.848234 21.33459
#> 23 17.388724 19.91294
#> 24 16.763494 22.14884
#> 25 15.645253 16.67038
#> 26 11.531367 19.75554
#> 27 16.628128 10.50065
#> 28 13.482812 14.59003
#> 29  8.781957 12.02214
#> 30 13.535258 10.87050
```
