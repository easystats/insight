# Model predictions (robust) and their confidence intervals

The `get_predicted()` function is a robust, flexible and user-friendly
alternative to base R
[`predict()`](https://rdrr.io/r/stats/predict.html) function. Additional
features and advantages include availability of uncertainty intervals
(CI), bootstrapping, a more intuitive API and the support of more models
than base R's [`predict()`](https://rdrr.io/r/stats/predict.html)
function. However, although the interface are simplified, it is still
very important to read the documentation of the arguments. This is
because making "predictions" (a lose term for a variety of things) is a
non-trivial process, with lots of caveats and complications. Read the
'Details' section for more information.

[`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
returns the confidence (or prediction) interval (CI) associated with
predictions made by a model. This function can be called separately on a
vector of predicted values. `get_predicted()` usually returns confidence
intervals (included as attribute, and accessible via the
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method)
by default. It is preferred to rely on the `get_predicted()` function
for standard errors and confidence intervals - use
[`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
only if standard errors and confidence intervals are not available
otherwise.

## Usage

``` r
get_predicted(x, ...)

# Default S3 method
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  ci_type = "confidence",
  ci_method = NULL,
  dispersion_method = "sd",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'lm'
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  iterations = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'stanreg'
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  iterations = NULL,
  ci = NULL,
  ci_method = NULL,
  include_random = "default",
  include_smooth = TRUE,
  verbose = TRUE,
  ...
)

# S3 method for class 'gam'
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  include_random = TRUE,
  include_smooth = TRUE,
  iterations = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'lmerMod'
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  ci_method = NULL,
  include_random = "default",
  iterations = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'principal'
get_predicted(x, data = NULL, ...)
```

## Arguments

- x:

  A statistical model (can also be a data.frame, in which case the
  second argument has to be a model).

- ...:

  Other argument to be passed, for instance to the model's
  [`predict()`](https://rdrr.io/r/stats/predict.html) method, or
  [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md).

- data:

  An optional data frame in which to look for variables with which to
  predict. If omitted, the data used to fit the model is used.
  Visualization matrices can be generated using
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

- predict:

  string or `NULL`

  - `"link"` returns predictions on the model's link-scale (for logistic
    models, that means the log-odds scale) with a confidence interval
    (CI). This option should also be used for finite mixture models
    (currently only family
    [`brms::mixture()`](https://paulbuerkner.com/brms/reference/mixture.html)
    from package *brms*), when predicted values of the response for each
    class is required.

  - `"expectation"` (default) also returns confidence intervals, but
    this time the output is on the response scale (for logistic models,
    that means probabilities).

  - `"prediction"` also gives an output on the response scale, but this
    time associated with a prediction interval (PI), which is larger
    than a confidence interval (though it mostly make sense for linear
    models).

  - `"classification"` is relevant only for binomial, ordinal or mixture
    models.

    - For binomial models, `predict = "classification"` will
      additionally transform the predictions into the original
      response's type (for instance, to a factor).

    - For ordinal models (e.g., classes `clm` or `multinom`), gives the
      predicted response class membership, defined as highest
      probability prediction.

    - For finite mixture models (currently only family
      [`brms::mixture()`](https://paulbuerkner.com/brms/reference/mixture.html)
      from package *brms*) also returns the predicted response class
      membership (similar as for ordinal models).

  - Other strings are passed directly to the `type` argument of the
    [`predict()`](https://rdrr.io/r/stats/predict.html) method supplied
    by the modelling package.

  - Specifically for models of class `brmsfit` (package *brms*), the
    `predict` argument can be any valid option for the `dpar` argument,
    to predict distributional parameters (such as `"sigma"`, `"beta"`,
    `"kappa"`, `"phi"` and so on, see
    [`?brms::brmsfamily`](https://paulbuerkner.com/brms/reference/brmsfamily.html)).

  - When `predict = NULL`, alternative arguments such as `type` will be
    captured by the `...` ellipsis and passed directly to the
    [`predict()`](https://rdrr.io/r/stats/predict.html) method supplied
    by the modelling package. Note that this might result in conflicts
    with multiple matching `type` arguments - thus, the recommendation
    is to use the `predict` argument for those values.

  - Notes: You can see the four options for predictions as on a gradient
    from "close to the model" to "close to the response data": "link",
    "expectation", "prediction", "classification". The `predict`
    argument modulates two things: the scale of the output and the type
    of certainty interval. Read more about in the **Details** section
    below.

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

- iterations:

  For Bayesian models, this corresponds to the number of posterior
  draws. If `NULL`, will return all the draws (one for each iteration of
  the model). For frequentist models, if not `NULL`, will generate
  bootstrapped draws, from which bootstrapped CIs will be computed.
  Iterations can be accessed by running
  `as.data.frame(..., keep_iterations = TRUE)` on the output.

- include_random:

  If `"default"`, include all random effects in the prediction, unless
  random effect variables are not in the data. If `TRUE`, include all
  random effects in the prediction (in this case, it will be checked if
  actually all random effect variables are in `data`). If `FALSE`, don't
  take them into account. Can also be a formula to specify which random
  effects to condition on when predicting (passed to the `re.form`
  argument). If `include_random = TRUE` and `data` is provided, make
  sure to include the random effect variables in `data` as well.

- include_smooth:

  For General Additive Models (GAMs). If `FALSE`, will fix the value of
  the smooth to its average, so that the predictions are not depending
  on it. (default), [`mean()`](https://rdrr.io/r/base/mean.html), or
  [`bayestestR::map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.html).

## Value

The fitted values (i.e. predictions for the response). For Bayesian or
bootstrapped models (when `iterations != NULL`), iterations (as columns
and observations are rows) can be accessed via
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Details

In `insight::get_predicted()`, the `predict` argument jointly modulates
two separate concepts, the **scale** and the **uncertainty interval**.

## Confidence Interval (CI) vs. Prediction Interval (PI))

- **Linear models** - [`lm()`](https://rdrr.io/r/stats/lm.html): For
  linear models, prediction intervals (`predict="prediction"`) show the
  range that likely contains the value of a new observation (in what
  range it is likely to fall), whereas confidence intervals
  (`predict="expectation"` or `predict="link"`) reflect the uncertainty
  around the estimated parameters (and gives the range of uncertainty of
  the regression line). In general, Prediction Intervals (PIs) account
  for both the uncertainty in the model's parameters, plus the random
  variation of the individual values. Thus, prediction intervals are
  always wider than confidence intervals. Moreover, prediction intervals
  will not necessarily become narrower as the sample size increases (as
  they do not reflect only the quality of the fit, but also the
  variability within the data).

- **Generalized Linear models** -
  [`glm()`](https://rdrr.io/r/stats/glm.html): For binomial models,
  prediction intervals are somewhat useless (for instance, for a
  binomial (Bernoulli) model for which the dependent variable is a
  vector of 1s and 0s, the prediction interval is... `[0, 1]`).

## Link scale vs. Response scale

When users set the `predict` argument to `"expectation"`, the
predictions are returned on the response scale, which is arguably the
most convenient way to understand and visualize relationships of
interest. When users set the `predict` argument to `"link"`, predictions
are returned on the link scale, and no transformation is applied. For
instance, for a logistic regression model, the response scale
corresponds to the predicted probabilities, whereas the link-scale makes
predictions of log-odds (probabilities on the logit scale). Note that
when users select `predict = "classification"` in binomial models, the
`get_predicted()` function will first calculate predictions as if the
user had selected `predict = "expectation"`. Then, it will round the
responses in order to return the most likely outcome. For ordinal or
mixture models, it returns the predicted class membership, based on the
highest probability of classification.

## Heteroscedasticity consistent standard errors

The arguments `vcov` and `vcov_args` can be used to calculate robust
standard errors for confidence intervals of predictions. These
arguments, when provided in `get_predicted()`, are passed down to
[`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md),
thus, see the related documentation there for more details.

## Finite mixture models

For finite mixture models (currently, only the `mixture()` family from
package *brms* is supported), use `predict = "classification"` to
predict the class membership. To predict outcome values by class, use
`predict = "link"`. Other `predict` options will return predicted values
of the outcome for the full data, not stratified by class membership.

## Bayesian and Bootstrapped models and iterations

For predictions based on multiple iterations, for instance in the case
of Bayesian models and bootstrapped predictions, the function used to
compute the centrality (point-estimate predictions) can be modified via
the `centrality_function` argument. For instance,
`get_predicted(model, centrality_function = stats::median)`. The default
is `mean`. Individual draws can be accessed by running
`iter <- as.data.frame(get_predicted(model))`, and their iterations can
be reshaped into a long format by
`bayestestR::reshape_iterations(iter)`.

## Hypothesis tests

There is limited support for hypothesis tests, i.e. objects of class
`htest`:

- [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html): returns the
  expected values of the contingency table.

## See also

[`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)

## Examples

``` r
data(mtcars)
x <- lm(mpg ~ cyl + hp, data = mtcars)

predictions <- get_predicted(x, ci = 0.95)
predictions
#> Predicted values:
#> 
#>  [1] 21.21678 21.21678 26.07124 21.21678 15.44448 21.31239 14.10597 26.66401
#>  [9] 26.03299 20.96820 20.96820 15.34888 15.34888 15.34888 14.87083 14.67962
#> [17] 14.39279 26.58752 26.85523 26.60665 25.99475 15.92253 15.92253 14.10597
#> [25] 15.44448 26.58752 26.10948 25.68880 13.74265 19.97387 12.38501 25.76529
#> 
#> NOTE: Confidence intervals, if available, are stored as attributes and can be accessed using `as.data.frame()` on this output.
#> 

# Options and methods ---------------------
get_predicted(x, predict = "prediction")
#> Predicted values:
#> 
#>  [1] 21.21678 21.21678 26.07124 21.21678 15.44448 21.31239 14.10597 26.66401
#>  [9] 26.03299 20.96820 20.96820 15.34888 15.34888 15.34888 14.87083 14.67962
#> [17] 14.39279 26.58752 26.85523 26.60665 25.99475 15.92253 15.92253 14.10597
#> [25] 15.44448 26.58752 26.10948 25.68880 13.74265 19.97387 12.38501 25.76529
#> 
#> NOTE: Confidence intervals, if available, are stored as attributes and can be accessed using `as.data.frame()` on this output.
#> 

# Get CI
as.data.frame(predictions)
#>    Predicted        SE    CI_low  CI_high
#> 1   21.21678 0.7281647 19.727518 22.70605
#> 2   21.21678 0.7281647 19.727518 22.70605
#> 3   26.07124 0.9279509 24.173366 27.96911
#> 4   21.21678 0.7281647 19.727518 22.70605
#> 5   15.44448 0.9200310 13.562810 17.32616
#> 6   21.31239 0.7777664 19.721680 22.90310
#> 7   14.10597 1.0080670 12.044237 16.16769
#> 8   26.66401 0.9225132 24.777260 28.55076
#> 9   26.03299 0.9362657 24.118117 27.94787
#> 10  20.96820 0.6234320 19.693139 22.24326
#> 11  20.96820 0.6234320 19.693139 22.24326
#> 12  15.34888 0.8862558 13.536280 17.16147
#> 13  15.34888 0.8862558 13.536280 17.16147
#> 14  15.34888 0.8862558 13.536280 17.16147
#> 15  14.87083 0.8057154 13.222961 16.51871
#> 16  14.67962 0.8206255 13.001249 16.35798
#> 17  14.39279 0.8911693 12.570146 16.21544
#> 18  26.58752 0.9099596 24.726448 28.44860
#> 19  26.85523 0.9695585 24.872258 28.83820
#> 20  26.60665 0.9127445 24.739874 28.47342
#> 21  25.99475 0.9454598 24.061069 27.92843
#> 22  15.92253 1.1490264 13.572504 18.27255
#> 23  15.92253 1.1490264 13.572504 18.27255
#> 24  14.10597 1.0080670 12.044237 16.16769
#> 25  15.44448 0.9200310 13.562810 17.32616
#> 26  26.58752 0.9099596 24.726448 28.44860
#> 27  26.10948 0.9205392 24.226768 27.99220
#> 28  25.68880 1.0474287 23.546572 27.83104
#> 29  13.74265 1.2011595 11.286007 16.19930
#> 30  19.97387 0.7635547 18.412227 21.53552
#> 31  12.38501 2.1153615  8.058613 16.71141
#> 32  25.76529 1.0175965 23.684073 27.84651

# Bootstrapped
as.data.frame(get_predicted(x, iterations = 4))
#>    Predicted    iter_1   iter_2   iter_3   iter_4
#> 1   21.54652 21.545133 22.28933 21.04761 21.30402
#> 2   21.54652 21.545133 22.28933 21.04761 21.30402
#> 3   26.78335 25.562393 27.98169 27.80512 25.78421
#> 4   21.54652 21.545133 22.28933 21.04761 21.30402
#> 5   14.89821 14.754365 15.08317 14.10354 15.65176
#> 6   21.69355 21.834040 22.44701 21.06704 21.42611
#> 7   12.83979 10.709666 12.87554 13.83148 13.94248
#> 8   27.69494 27.353616 28.95935 27.92561 26.54118
#> 9   26.72454 25.446830 27.91861 27.79735 25.73537
#> 10  21.16425 20.793975 21.87934 20.99708 20.98658
#> 11  21.16425 20.793975 21.87934 20.99708 20.98658
#> 12  14.75118 14.465458 14.92548 14.08410 15.52966
#> 13  14.75118 14.465458 14.92548 14.08410 15.52966
#> 14  14.75118 14.465458 14.92548 14.08410 15.52966
#> 15  14.01603 13.020923 14.13704 13.98694 14.91921
#> 16  13.72197 12.443109 13.82167 13.94807 14.67502
#> 17  13.28088 11.576387 13.34860 13.88977 14.30875
#> 18  27.57731 27.122491 28.83320 27.91006 26.44350
#> 19  27.98900 27.931431 29.27473 27.96447 26.78536
#> 20  27.60672 27.180272 28.86474 27.91395 26.46792
#> 21  26.66573 25.331267 27.85554 27.78958 25.68654
#> 22  15.63335 16.198901 15.87160 14.20070 16.26221
#> 23  15.63335 16.198901 15.87160 14.20070 16.26221
#> 24  12.83979 10.709666 12.87554 13.83148 13.94248
#> 25  14.89821 14.754365 15.08317 14.10354 15.65176
#> 26  27.57731 27.122491 28.83320 27.91006 26.44350
#> 27  26.84217 25.677955 28.04476 27.81290 25.83305
#> 28  26.19524 24.406764 27.35094 27.72739 25.29585
#> 29  12.28108  9.611819 12.27633 13.75763 13.47853
#> 30  19.63514 17.789341 20.23939 20.79498 19.71683
#> 31  10.19325  5.509339 10.03716 13.48168 11.74483
#> 32  26.31286 24.637890 27.47709 27.74294 25.39352
# Same as as.data.frame(..., keep_iterations = FALSE)
summary(get_predicted(x, iterations = 4))
#>    Predicted
#> 1   21.38040
#> 2   21.38040
#> 3   26.38374
#> 4   21.38040
#> 5   15.25410
#> 6   21.49737
#> 7   13.61645
#> 8   27.10898
#> 9   26.33695
#> 10  21.07626
#> 11  21.07626
#> 12  15.13712
#> 13  15.13712
#> 14  15.13712
#> 15  14.55225
#> 16  14.31830
#> 17  13.96738
#> 18  27.01540
#> 19  27.34293
#> 20  27.03880
#> 21  26.29016
#> 22  15.83897
#> 23  15.83897
#> 24  13.61645
#> 25  15.25410
#> 26  27.01540
#> 27  26.43053
#> 28  25.91584
#> 29  13.17195
#> 30  19.85973
#> 31  11.51091
#> 32  26.00942

# Different prediction types ------------------------
data(iris)
data <- droplevels(iris[1:100, ])

# Fit a logistic model
x <- glm(Species ~ Sepal.Length, data = data, family = "binomial")

# Expectation (default): response scale + CI
pred <- get_predicted(x, predict = "expectation", ci = 0.95)
head(as.data.frame(pred))
#>    Predicted         SE      CI_low    CI_high
#> 1 0.16579367 0.05943589 0.078854431 0.31573138
#> 2 0.06637193 0.03625646 0.022083989 0.18286787
#> 3 0.02479825 0.01843411 0.005675609 0.10175666
#> 4 0.01498061 0.01261461 0.002839122 0.07513285
#> 5 0.10623680 0.04779474 0.042437982 0.24173444
#> 6 0.48159935 0.07901420 0.333158095 0.63336131

# Prediction: response scale + PI
pred <- get_predicted(x, predict = "prediction", ci = 0.95)
head(as.data.frame(pred))
#>    Predicted       CI_low      CI_high
#> 1 0.16579367 2.220446e-16 1.000000e+00
#> 2 0.06637193 2.220446e-16 1.000000e+00
#> 3 0.02479825 2.220446e-16 2.220446e-16
#> 4 0.01498061 2.220446e-16 2.220446e-16
#> 5 0.10623680 2.220446e-16 1.000000e+00
#> 6 0.48159935 2.220446e-16 1.000000e+00

# Link: link scale + CI
pred <- get_predicted(x, predict = "link", ci = 0.95)
head(as.data.frame(pred))
#>     Predicted        SE     CI_low    CI_high
#> 1 -1.61573668 0.4297415 -2.4580146 -0.7734588
#> 2 -2.64380391 0.5850960 -3.7905709 -1.4970369
#> 3 -3.67187114 0.7622663 -5.1658856 -2.1778567
#> 4 -4.18590475 0.8548690 -5.8614172 -2.5103923
#> 5 -2.12977030 0.5033646 -3.1163467 -1.1431939
#> 6 -0.07363584 0.3164854 -0.6939359  0.5466642

# Classification: classification "type" + PI
pred <- get_predicted(x, predict = "classification", ci = 0.95)
head(as.data.frame(pred))
#>   Predicted CI_low    CI_high
#> 1    setosa setosa versicolor
#> 2    setosa setosa versicolor
#> 3    setosa setosa     setosa
#> 4    setosa setosa     setosa
#> 5    setosa setosa versicolor
#> 6    setosa setosa versicolor
```
