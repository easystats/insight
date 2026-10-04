# Confidence intervals around predicted values

Confidence intervals around predicted values

## Usage

``` r
get_predicted_ci(x, ...)

# Default S3 method
get_predicted_ci(
  x,
  predictions = NULL,
  data = NULL,
  se = NULL,
  ci = 0.95,
  ci_type = "confidence",
  ci_method = NULL,
  dispersion_method = "sd",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A statistical model (can also be a data.frame, in which case the
  second argument has to be a model).

- ...:

  Other argument to be passed, for instance to the model's
  [`predict()`](https://rdrr.io/r/stats/predict.html) method, or
  `get_predicted_ci()`.

- predictions:

  A vector of predicted values (as obtained by
  [`stats::fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) or
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)).

- data:

  An optional data frame in which to look for variables with which to
  predict. If omitted, the data used to fit the model is used.
  Visualization matrices can be generated using
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

- se:

  Numeric vector of standard error of predicted values. If `NULL`,
  standard errors are calculated based on the variance-covariance
  matrix.

- ci:

  The interval level. Default is `NULL`, to be fast even for larger
  models. Set the interval level to an explicit value, e.g. `0.95`, for
  `95%` CI).

- ci_type:

  Can be `"prediction"` or `"confidence"`. Prediction intervals show the
  range that likely contains the value of a new observation (in what
  range it would fall), whereas confidence intervals reflect the
  uncertainty around the estimated parameters (and gives the range of
  the link; for instance of the regression line in a linear
  regressions). Prediction intervals account for both the uncertainty in
  the model's parameters, plus the random variation of the individual
  values. Thus, prediction intervals are always wider than confidence
  intervals. Moreover, prediction intervals will not necessarily become
  narrower as the sample size increases (as they do not reflect only the
  quality of the fit). This applies mostly for "simple" linear models
  (like `lm`), as for other models (e.g., `glm`), prediction intervals
  are somewhat useless (for instance, for a binomial model for which the
  dependent variable is a vector of 1s and 0s, the prediction interval
  is... `[0, 1]`).

- ci_method:

  The method for computing p values and confidence intervals. Possible
  values depend on model type.

  - `NULL` uses the default method, which varies based on the model
    type.

  - Most frequentist models: `"wald"` (default), `"residual"` or
    `"normal"`.

  - Bayesian models: `"quantile"` (default), `"hdi"`, `"eti"`, and
    `"spi"`.

  - Mixed effects **lme4** models: `"wald"` (default), `"residual"`,
    `"normal"`, `"satterthwaite"`, and `"kenward-roger"`.

  See
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  for details.

- dispersion_method:

  Bootstrap dispersion and Bayesian posterior summary: `"sd"` or
  `"mad"`.

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://zeileis.codeberg.page/sandwich/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

    - Kenward-Roger approximation: `"kenward-roger"`. See
      [`?pbkrtest::vcovAdj`](https://rdrr.io/pkg/pbkrtest/man/kr-vcovAdj.html).

    - Finite Population Correction: `"fpc"` applies the finite
      population correction. Requires the `population_size` to be
      specified in `vcov_args`. For mixed models, FPC is based on *Lai
      et al. 2018*. When `vcov = "fpc"`, at least one of
      `population_size` (size of the finite population, must be larger
      than the number of observations in the model) or `cluster_size`
      (the finite size of cluster groups in the population, must be
      larger than the number of groups of the random effects) in the
      `vcov_args` argument. You can additionally apply the Kenward-Roger
      approximation with the `kr` argument, e.g.
      `vcov_args = list(cluster_size = 15, kr = TRUE)`.

  Exceptions are following models:

  - Model of class `glmgee`, which have pre-defined options for the
    variance-covariance matrix calculation. These are `"robust"`,
    `"df-adjusted"`, `"model"`, `"bias-corrected"`, and `"jackknife"`.
    See
    [`?glmtoolbox::vcov.glmgee`](https://rdrr.io/pkg/glmtoolbox/man/vcov.glmgee.html)
    for details.

  - Model of class `glmmTMB` currently only support the `"HC0"` option.

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://zeileis.codeberg.page/sandwich/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`. For `vcov = "fpc"`, `vcov_args` must specify either
  `population_size` or `cluster_size`, depending on the model.

- verbose:

  Toggle warnings.

## Details

Typically,
[`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
returns confidence intervals based on the standard errors as returned by
the [`predict()`](https://rdrr.io/r/stats/predict.html)-function,
assuming normal distribution (`+/- 1.96 * SE`) resp. a Student's
t-distribution (if degrees of freedom are available). If
[`predict()`](https://rdrr.io/r/stats/predict.html) for a certain class
does *not* return standard errors (for example, *merMod*-objects), these
are calculated manually, based on following steps: matrix-multiply `X`
by the parameter vector `B` to get the predictions, then extract the
variance-covariance matrix `V` of the parameters and compute `XVX'` to
get the variance-covariance matrix of the predictions. The square-root
of the diagonal of this matrix represent the standard errors of the
predictions, which are then multiplied by the critical test-statistic
value (e.g., ~1.96 for normal distribution) for the confidence
intervals.

If `ci_type = "prediction"`, prediction intervals are calculated. These
are wider than confidence intervals, because they also take into account
the uncertainty of the model itself. Before taking the square-root of
the diagonal of the variance-covariance matrix, `get_predicted_ci()`
adds the residual variance to these values. For mixed models,
[`get_variance_residual()`](https://easystats.github.io/insight/reference/get_variance.md)
is used, while `get_sigma()^2` is used for non-mixed models.

It is preferred to rely on standard errors returned by
[`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
(i.e. returned by the
[`predict()`](https://rdrr.io/r/stats/predict.html)-function), because
these are more accurate than manually calculated standard errors. Use
`get_predicted_ci()` only if standard errors are not available
otherwise. An exception are Bayesian models or bootstrapped predictions,
where `get_predicted_ci()` returns quantiles of the posterior
distribution or bootstrapped samples of the predictions. These are
actually accurate standard errors resp. confidence (or uncertainty)
intervals.

## Examples

``` r
# Confidence Intervals for Model Predictions
# ------------------------------------------

data(mtcars)

# Linear model
# ------------
x <- lm(mpg ~ cyl + hp, data = mtcars)
predictions <- predict(x)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
#>         SE    CI_low  CI_high
#> 1 3.255505 14.558527 27.87504
#> 2 3.255505 14.558527 27.87504
#> 3 3.305931 19.309850 32.83263
#> 4 3.255505 14.558527 27.87504
#> 5 3.303717  8.687625 22.20134
#> 6 3.266957 14.630713 27.99407
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)
#>          SE   CI_low  CI_high
#> 1 0.7281647 19.72752 22.70605
#> 2 0.7281647 19.72752 22.70605
#> 3 0.9279509 24.17337 27.96911
#> 4 0.7281647 19.72752 22.70605
#> 5 0.9200310 13.56281 17.32616
#> 6 0.7777664 19.72168 22.90310
ci_vals <- get_predicted_ci(x, predictions, ci = c(0.8, 0.9, 0.95))
head(ci_vals)
#>                          SE CI_low_0.8 CI_high_0.8 CI_low_0.9 CI_high_0.9
#> Mazda RX4         0.7281647   20.26184    22.17172   19.97954    22.45403
#> Mazda RX4 Wag     0.7281647   20.26184    22.17172   19.97954    22.45403
#> Datsun 710        0.9279509   24.85429    27.28818   24.49453    27.64794
#> Hornet 4 Drive    0.7281647   20.26184    22.17172   19.97954    22.45403
#> Hornet Sportabout 0.9200310   14.23793    16.65104   13.88124    17.00773
#> Valiant           0.7777664   20.29240    22.33238   19.99087    22.63391
#>                   CI_low_0.95 CI_high_0.95
#> Mazda RX4            19.72752     22.70605
#> Mazda RX4 Wag        19.72752     22.70605
#> Datsun 710           24.17337     27.96911
#> Hornet 4 Drive       19.72752     22.70605
#> Hornet Sportabout    13.56281     17.32616
#> Valiant              19.72168     22.90310

# Bootstrapped
# ------------
predictions <- get_predicted(x, iterations = 500)
get_predicted_ci(x, predictions)
#>           SE    CI_low  CI_high
#> 1  0.7006056 19.963015 22.74545
#> 2  0.7006056 19.963015 22.74545
#> 3  1.1532103 24.002040 28.24791
#> 4  0.7006056 19.963015 22.74545
#> 5  0.7250733 13.936479 16.84349
#> 6  0.7449227 20.044868 22.94963
#> 7  0.9488100 11.608502 15.22088
#> 8  1.1340843 24.514631 29.13979
#> 9  1.1608003 23.926529 28.18054
#> 10 0.6136387 19.801650 22.23018
#> 11 0.6136387 19.801650 22.23018
#> 12 0.6927429 13.892890 16.66277
#> 13 0.6927429 13.892890 16.66277
#> 14 0.6927429 13.892890 16.66277
#> 15 0.6466944 13.429524 15.96133
#> 16 0.6868151 13.070747 15.73551
#> 17 0.7973228 12.418535 15.50000
#> 18 1.1259085 24.475683 28.98180
#> 19 1.1677369 24.701160 29.14757
#> 20 1.1276610 24.485420 29.02651
#> 21 1.1691056 23.855521 28.13333
#> 22 0.9604360 14.150357 17.79760
#> 23 0.9604360 14.150357 17.79760
#> 24 0.9488100 11.608502 15.22088
#> 25 0.7250733 13.936479 16.84349
#> 26 1.1259085 24.475683 28.98180
#> 27 1.1463495 24.069949 28.31528
#> 28 1.2593395 23.286432 27.85780
#> 29 1.1745553 10.466140 15.00963
#> 30 0.8211555 18.143960 21.30038
#> 31 2.1442974  6.343698 14.59706
#> 32 1.2330584 23.404826 27.89512

ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
head(ci_vals)
#>          SE CI_low_0.8 CI_high_0.8 CI_low_0.95 CI_high_0.95
#> 1 0.7006056   20.46146    22.19078    19.96301     22.74545
#> 2 0.7006056   20.46146    22.19078    19.96301     22.74545
#> 3 1.1532103   24.59493    27.52907    24.00204     28.24791
#> 4 0.7006056   20.46146    22.19078    19.96301     22.74545
#> 5 0.7250733   14.51319    16.29560    13.93648     16.84349
#> 6 0.7449227   20.52974    22.37892    20.04487     22.94963
datawizard::reshape_ci(ci_vals)
#>           SE   CI    CI_low  CI_high
#> 1  0.7006056 0.80 20.461458 22.19078
#> 2  0.7006056 0.95 19.963015 22.74545
#> 3  0.7006056 0.80 20.461458 22.19078
#> 4  0.7006056 0.95 19.963015 22.74545
#> 5  1.1532103 0.80 24.594930 27.52907
#> 6  1.1532103 0.95 24.002040 28.24791
#> 7  0.7006056 0.80 20.461458 22.19078
#> 8  0.7006056 0.95 19.963015 22.74545
#> 9  0.7250733 0.80 14.513189 16.29560
#> 10 0.7250733 0.95 13.936479 16.84349
#> 11 0.7449227 0.80 20.529738 22.37892
#> 12 0.7449227 0.95 20.044868 22.94963
#> 13 0.9488100 0.80 12.674437 14.92139
#> 14 0.9488100 0.95 11.608502 15.22088
#> 15 1.1340843 0.80 25.337747 28.14572
#> 16 1.1340843 0.95 24.514631 29.13979
#> 17 1.1608003 0.80 24.560893 27.50455
#> 18 1.1608003 0.95 23.926529 28.18054
#> 19 0.6136387 0.80 20.275472 21.81384
#> 20 0.6136387 0.95 19.801650 22.23018
#> 21 0.6136387 0.80 20.275472 21.81384
#> 22 0.6136387 0.95 19.801650 22.23018
#> 23 0.6927429 0.80 14.445163 16.15862
#> 24 0.6927429 0.95 13.892890 16.66277
#> 25 0.6927429 0.80 14.445163 16.15862
#> 26 0.6927429 0.95 13.892890 16.66277
#> 27 0.6927429 0.80 14.445163 16.15862
#> 28 0.6927429 0.95 13.892890 16.66277
#> 29 0.6466944 0.80 13.960058 15.57139
#> 30 0.6466944 0.95 13.429524 15.96133
#> 31 0.6868151 0.80 13.602583 15.39801
#> 32 0.6868151 0.95 13.070747 15.73551
#> 33 0.7973228 0.80 13.071633 15.14232
#> 34 0.7973228 0.95 12.418535 15.50000
#> 35 1.1259085 0.80 25.265361 28.06548
#> 36 1.1259085 0.95 24.475683 28.98180
#> 37 1.1677369 0.80 25.499470 28.49726
#> 38 1.1677369 0.95 24.701160 29.14757
#> 39 1.1276610 0.80 25.286881 28.10269
#> 40 1.1276610 0.95 24.485420 29.02651
#> 41 1.1691056 0.80 24.513126 27.48328
#> 42 1.1691056 0.95 23.855521 28.13333
#> 43 0.9604360 0.80 14.803850 17.17541
#> 44 0.9604360 0.95 14.150357 17.79760
#> 45 0.9604360 0.80 14.803850 17.17541
#> 46 0.9604360 0.95 14.150357 17.79760
#> 47 0.9488100 0.80 12.674437 14.92139
#> 48 0.9488100 0.95 11.608502 15.22088
#> 49 0.7250733 0.80 14.513189 16.29560
#> 50 0.7250733 0.95 13.936479 16.84349
#> 51 1.1259085 0.80 25.265361 28.06548
#> 52 1.1259085 0.95 24.475683 28.98180
#> 53 1.1463495 0.80 24.668848 27.58310
#> 54 1.1463495 0.95 24.069949 28.31528
#> 55 1.2593395 0.80 24.044869 27.31105
#> 56 1.2593395 0.95 23.286432 27.85780
#> 57 1.1745553 0.80 11.963180 14.68406
#> 58 1.1745553 0.95 10.466140 15.00963
#> 59 0.8211555 0.80 18.834859 20.79811
#> 60 0.8211555 0.95 18.143960 21.30038
#> 61 2.1442974 0.80  9.015084 13.89369
#> 62 2.1442974 0.95  6.343698 14.59706
#> 63 1.2330584 0.80 24.214099 27.35368
#> 64 1.2330584 0.95 23.404826 27.89512

ci_vals <- get_predicted_ci(x,
  predictions,
  dispersion_method = "MAD",
  ci_method = "HDI"
)
head(ci_vals)
#>          SE   CI_low  CI_high
#> 1 0.7136662 19.86000 22.53026
#> 2 0.7136662 19.86000 22.53026
#> 3 1.1599936 23.91404 28.17927
#> 4 0.7136662 19.86000 22.53026
#> 5 0.6894465 14.10790 16.93747
#> 6 0.7516932 19.95404 22.80568


# Logistic model
# --------------
x <- glm(vs ~ wt, data = mtcars, family = "binomial")
predictions <- predict(x, type = "link")
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
#>                   CI_low CI_high
#> Mazda RX4           -Inf     Inf
#> Mazda RX4 Wag       -Inf     Inf
#> Datsun 710          -Inf     Inf
#> Hornet 4 Drive      -Inf     Inf
#> Hornet Sportabout   -Inf     Inf
#> Valiant             -Inf     Inf
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)
#>          SE     CI_low   CI_high
#> 1 0.5623444 -0.3931282 1.8112213
#> 2 0.4690190 -0.6974034 1.1411172
#> 3 0.7195076 -0.1279982 2.6924199
#> 4 0.4459072 -1.3016913 0.4462326
#> 5 0.5021936 -1.8418839 0.1266787
#> 6 0.5094490 -1.8943152 0.1026881
```
