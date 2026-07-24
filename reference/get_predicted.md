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

# S3 method for class 'nestedLogit'
get_predicted(
  x,
  data = NULL,
  predict = "expectation",
  submodel = "nested",
  ci = NULL,
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
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`. For `vcov = "fpc"`, `vcov_args` must specify either
  `population_size` or `cluster_size`, depending on the model.

- verbose:

  Toggle warnings.

- iterations:

  For Bayesian models, this corresponds to the number of posterior
  draws. If `NULL`, will return all the draws (one for each iteration of
  the model). For frequentist models, if not `NULL`, will generate
  bootstrapped draws, from which bootstrapped CIs will be computed.
  Iterations can be accessed by running
  `as.data.frame(..., keep_iterations = TRUE)` on the output.

- submodel:

  Only applies to models of class `nestedLogit`. Can be `"nested"` or
  `"dichotomies"`. If `"nested"` (default), the fitted probabilities
  under the nested logit model are returned. For `"dichotomies"`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) is invoked for
  each binary logit model.

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
#>    Predicted   iter_1   iter_2   iter_3   iter_4
#> 1   21.39083 22.35246 21.41531 20.88844 20.90712
#> 2   21.39083 22.35246 21.41531 20.88844 20.90712
#> 3   26.32867 26.36933 26.98082 26.73586 25.22866
#> 4   21.39083 22.35246 21.41531 20.88844 20.90712
#> 5   15.33673 16.42520 15.29085 14.16874 15.46214
#> 6   21.50711 22.55146 21.47354 20.97931 21.02415
#> 7   13.70884 13.63921 14.47571 12.89665 13.82379
#> 8   27.04959 27.60312 27.34181 27.29921 25.95422
#> 9   26.28216 26.28973 26.95753 26.69951 25.18185
#> 10  21.08851 21.83507 21.26393 20.65220 20.60286
#> 11  21.08851 21.83507 21.26393 20.65220 20.60286
#> 12  15.22045 16.22621 15.23263 14.07787 15.34511
#> 13  15.22045 16.22621 15.23263 14.07787 15.34511
#> 14  15.22045 16.22621 15.23263 14.07787 15.34511
#> 15  14.63906 15.23121 14.94151 13.62355 14.75999
#> 16  14.40651 14.83321 14.82506 13.44183 14.52594
#> 17  14.05767 14.23621 14.65038 13.16924 14.17486
#> 18  26.95657 27.44393 27.29523 27.22652 25.86060
#> 19  27.28215 28.00112 27.45826 27.48094 26.18827
#> 20  26.97983 27.48373 27.30688 27.24469 25.88401
#> 21  26.23565 26.21013 26.93424 26.66317 25.13504
#> 22  15.91812 17.42020 15.58197 14.62305 16.04726
#> 23  15.91812 17.42020 15.58197 14.62305 16.04726
#> 24  13.70884 13.63921 14.47571 12.89665 13.82379
#> 25  15.33673 16.42520 15.29085 14.16874 15.46214
#> 26  26.95657 27.44393 27.29523 27.22652 25.86060
#> 27  26.37518 26.44893 27.00411 26.77221 25.27547
#> 28  25.86356 25.57333 26.74792 26.37241 24.76056
#> 29  13.26698 12.88302 14.25446 12.55137 13.37909
#> 30  19.87922 19.76547 20.65840 19.70722 19.38580
#> 31  11.61584 10.05722 13.42768 11.26111 11.71733
#> 32  25.95658 25.73253 26.79450 26.44510 24.85418
# Same as as.data.frame(..., keep_iterations = FALSE)
summary(get_predicted(x, iterations = 4))
#>    Predicted
#> 1   21.00214
#> 2   21.00214
#> 3   26.03266
#> 4   21.00214
#> 5   15.42824
#> 6   21.05874
#> 7   14.63582
#> 8   26.38359
#> 9   26.01002
#> 10  20.85497
#> 11  20.85497
#> 12  15.37164
#> 13  15.37164
#> 14  15.37164
#> 15  15.08863
#> 16  14.97543
#> 17  14.80562
#> 18  26.33831
#> 19  26.49679
#> 20  26.34963
#> 21  25.98738
#> 22  15.71125
#> 23  15.71125
#> 24  14.63582
#> 25  15.42824
#> 26  26.33831
#> 27  26.05530
#> 28  25.80626
#> 29  14.42073
#> 30  20.26632
#> 31  13.61700
#> 32  25.85154

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
