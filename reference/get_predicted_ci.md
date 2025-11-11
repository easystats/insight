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
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

    - Kenward-Roger approximation: `kenward-roger`. See
      [`?pbkrtest::vcovAdj`](https://rdrr.io/pkg/pbkrtest/man/kr-vcovAdj.html).

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
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`.

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
#> 1  0.6949363 19.982569 22.79179
#> 2  0.6949363 19.982569 22.79179
#> 3  1.1455753 24.006699 28.24791
#> 4  0.6949363 19.982569 22.79179
#> 5  0.7184624 13.936479 16.85555
#> 6  0.7382678 20.057173 22.98248
#> 7  0.9496988 11.576133 15.27176
#> 8  1.1315103 24.600536 29.13979
#> 9  1.1526901 23.929159 28.18054
#> 10 0.6100143 19.821321 22.18819
#> 11 0.6100143 19.821321 22.18819
#> 12 0.6880375 13.892890 16.65417
#> 13 0.6880375 13.892890 16.65417
#> 14 0.6880375 13.892890 16.65417
#> 15 0.6494368 13.454805 15.96677
#> 16 0.6905605 13.062597 15.74784
#> 17 0.8004374 12.373435 15.55210
#> 18 1.1229745 24.530915 29.05339
#> 19 1.1656447 24.733250 29.14757
#> 20 1.1248259 24.548320 29.08646
#> 21 1.1605076 23.876767 28.13347
#> 22 0.9447152 14.193405 17.83975
#> 23 0.9447152 14.193405 17.83975
#> 24 0.9496988 11.576133 15.27176
#> 25 0.7184624 13.936479 16.85555
#> 26 1.1229745 24.530915 29.05339
#> 27 1.1391763 24.069949 28.31528
#> 28 1.2465079 23.316083 27.92838
#> 29 1.1715977 10.407908 15.05999
#> 30 0.8126709 18.122765 21.30038
#> 31 2.1247876  6.343698 14.61190
#> 32 1.2213277 23.522816 27.96462

ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
head(ci_vals)
#>          SE CI_low_0.8 CI_high_0.8 CI_low_0.95 CI_high_0.95
#> 1 0.6949363   20.46216    22.20649    19.98257     22.79179
#> 2 0.6949363   20.46216    22.20649    19.98257     22.79179
#> 3 1.1455753   24.66184    27.52907    24.00670     28.24791
#> 4 0.6949363   20.46216    22.20649    19.98257     22.79179
#> 5 0.7184624   14.50305    16.32141    13.93648     16.85555
#> 6 0.7382678   20.53239    22.38169    20.05717     22.98248
datawizard::reshape_ci(ci_vals)
#>           SE   CI    CI_low  CI_high
#> 1  0.6949363 0.80 20.462162 22.20649
#> 2  0.6949363 0.95 19.982569 22.79179
#> 3  0.6949363 0.80 20.462162 22.20649
#> 4  0.6949363 0.95 19.982569 22.79179
#> 5  1.1455753 0.80 24.661838 27.52907
#> 6  1.1455753 0.95 24.006699 28.24791
#> 7  0.6949363 0.80 20.462162 22.20649
#> 8  0.6949363 0.95 19.982569 22.79179
#> 9  0.7184624 0.80 14.503054 16.32141
#> 10 0.7184624 0.95 13.936479 16.85555
#> 11 0.7382678 0.80 20.532388 22.38169
#> 12 0.7382678 0.95 20.057173 22.98248
#> 13 0.9496988 0.80 12.697100 14.92842
#> 14 0.9496988 0.95 11.576133 15.27176
#> 15 1.1315103 0.80 25.347228 28.15504
#> 16 1.1315103 0.95 24.600536 29.13979
#> 17 1.1526901 0.80 24.615108 27.50811
#> 18 1.1526901 0.95 23.929159 28.18054
#> 19 0.6100143 0.80 20.263464 21.81579
#> 20 0.6100143 0.95 19.821321 22.18819
#> 21 0.6100143 0.80 20.263464 21.81579
#> 22 0.6100143 0.95 19.821321 22.18819
#> 23 0.6880375 0.80 14.447121 16.16415
#> 24 0.6880375 0.95 13.892890 16.65417
#> 25 0.6880375 0.80 14.447121 16.16415
#> 26 0.6880375 0.95 13.892890 16.65417
#> 27 0.6880375 0.80 14.447121 16.16415
#> 28 0.6880375 0.95 13.892890 16.65417
#> 29 0.6494368 0.80 13.943400 15.58492
#> 30 0.6494368 0.95 13.454805 15.96677
#> 31 0.6905605 0.80 13.607265 15.41091
#> 32 0.6905605 0.95 13.062597 15.74784
#> 33 0.8004374 0.80 13.118137 15.15680
#> 34 0.8004374 0.95 12.373435 15.55210
#> 35 1.1229745 0.80 25.265349 28.00866
#> 36 1.1229745 0.95 24.530915 29.05339
#> 37 1.1656447 0.80 25.508819 28.46030
#> 38 1.1656447 0.95 24.733250 29.14757
#> 39 1.1248259 0.80 25.287929 28.03353
#> 40 1.1248259 0.95 24.548320 29.08646
#> 41 1.1605076 0.80 24.537754 27.50315
#> 42 1.1605076 0.95 23.876767 28.13347
#> 43 0.9447152 0.80 14.779598 17.19662
#> 44 0.9447152 0.95 14.193405 17.83975
#> 45 0.9447152 0.80 14.779598 17.19662
#> 46 0.9447152 0.95 14.193405 17.83975
#> 47 0.9496988 0.80 12.697100 14.92842
#> 48 0.9496988 0.95 11.576133 15.27176
#> 49 0.7184624 0.80 14.503054 16.32141
#> 50 0.7184624 0.95 13.936479 16.85555
#> 51 1.1229745 0.80 25.265349 28.00866
#> 52 1.1229745 0.95 24.530915 29.05339
#> 53 1.1391763 0.80 24.705589 27.58400
#> 54 1.1391763 0.95 24.069949 28.31528
#> 55 1.2465079 0.80 24.048307 27.31105
#> 56 1.2465079 0.95 23.316083 27.92838
#> 57 1.1715977 0.80 11.985385 14.68405
#> 58 1.1715977 0.95 10.407908 15.05999
#> 59 0.8126709 0.80 18.859425 20.80681
#> 60 0.8126709 0.95 18.122765 21.30038
#> 61 2.1247876 0.80  9.088326 13.89369
#> 62 2.1247876 0.95  6.343698 14.61190
#> 63 1.2213277 0.80 24.190157 27.35523
#> 64 1.2213277 0.95 23.522816 27.96462

ci_vals <- get_predicted_ci(x,
  predictions,
  dispersion_method = "MAD",
  ci_method = "HDI"
)
head(ci_vals)
#>          SE   CI_low  CI_high
#> 1 0.6947532 19.86000 22.55479
#> 2 0.6947532 19.86000 22.55479
#> 3 1.1754014 23.91404 28.16061
#> 4 0.6947532 19.86000 22.55479
#> 5 0.7055012 13.86619 16.70287
#> 6 0.7513246 19.90145 22.77948


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
