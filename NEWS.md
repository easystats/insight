# insight 0.19.7

## General

* Support for objects of class `ggcomparisons` from `ggeffects::hypothesis_test()`.

* `brms::gr()` is now supported, meaning that functions like `get_data()` or
  `find_predictors()` now also work for models with group-specific random effects.

* Fix CRAN check issues due to the last *fixest* update.

## Changes to functions

* `get_varcov()` for models of class `pgmm` (package *plm*) now also supported
  robust variance-covariance matrices (i.e. argument `vcov`).

## Bug fixes

* Fixed issue in `find_predictors()` for survival models with `strata()`,
  containing more that one variable.

* Fixed issue in `model_info()`, where in some cases logistic regression models
  were erroneously considered as `"bernoulli"` models.

* Fixed issue in `find_formula()` for models of class `gamlss` when the `random()`
  function was used with namespace in the formula (i.e. `... + gamlss::random()`).

* `model_info()` now detects models with zero-inflation part from package
  *glmmTMB* when models have truncated-families but no `ziformula`.

# insight 0.19.6

## General

* Improved documentation for `get_predicted_ci()`.

## Changes to functions

* `model_info()` now recognized ordered beta families.

* `find_formula` and `get_response` for `nestedLogit` models gain a `dichotomies`
  argument, to return values for the dichotomies used to fit the model.

## Bug fixes

* `find_transformation()` better detects power-transformation of the response
  variable.

* Corrected return value from `find_statistic` for `nnet::multinom()` models.

* `clean_parameters()` did not return the `"clean_parameters"` class attributes
  for some object. This caused issued in upstream packages.

* Fixed issue in `model_info()`, which did not correctly detect "Bernoulli"
  property for some models classes (like `glmmTMB` or `glmerMod`).

# insight 0.19.5

## Bug fixes

* Fixed critical issue with `check_if_installed()` for old R releases.

# insight 0.19.4

## Changes to functions

* `get_predicted()` now accepts `predict = "link"` for gaussian models with
  log-link (i.e. `glm(..., family = gaussian("log"))`), to return predictions
  on the link scale.
  
* `check_if_installed()` now automatically checks the package DESCRIPTION file to 
  determine the correct minimum version required.

## Bug fixes

* Fixed issue with invalid multibyte strings in `trim_ws()`.

* Fixed issue in `find_statistic()` for models from package *fixest*.

# insight 0.19.3

## Breaking changes

* `standardize_column_order()` has changed the position when re-ordering Bayes
  factors, ROPEs and ESS / Rhat (mainly relevant for Bayesian models).

## Changes to functions

* `standardize_names()` and `standardize_column_order()` now also recognize the
  `"response.level"` column name.

* `get_data()` for _lavaan_ models is now more stable at retrieving model data
  when this is not available in the environment.

* `find_terms()` gets an `as_term_labels` argument, to extract model terms
  from the formula's `"term.labels"` attribute. This is closer to the behaviour
  of `stats::terms()`, but may be insufficient, e.g. for mixed models.

## Bug fixes

* `get_random()` now returns the same observations as `get_data()` and correctly
  removes missing values from the data before returning it.

* `find_parameters()` for marginal effects ignores the `"s.value"` column (which
  was added in a recent update).

* Fixed issue in `get_response()` for _brms_ models with `trunc()` function in
  the response variable.

# insight 0.19.2

## Breaking changes

* The minimum needed R version has been bumped to `3.6`.

* `download_model()` no longer errors when a model object could not be downloaded,
  but instead returns `NULL`. This prevents test failures, and allows to skip
  tests when the return value of `download_model()` is `NULL`.

## General

* Improved support for `mclogit` models (package *mclogit*) and `mipo` objects
  (package *mice*) for models with ordinal or categorical response.

## New supported models

* `phylolm` and `phyloglm` (package *phylolm*), `nestedLogit` (package *nestedLogit*).

## Bug fixes

* Fixed issue in `get_variance()` for *glmmTMB* models with rank deficient
  coefficients.

* Fixed issues in `get_weights()` for `glm` models without weights and `na.action`
  not set to default in the model call.

* `clean_names()` now also removes the `relevel()` pattern.

* Fixed issue in `model_info()` for models of class `gamlss`.

* Fixed problems preventing `get_data()` from locating data defined in
  non-global environments.

* Fixed issue in `get_predicted()` for variables of class numeric matrix created
  by `scale()`, which were correctly handled only when `get_data()` failed to
  find the data in the appropriate environment.

* Fixed issue in `model_info()` for `gee` models from `binomial` families.

# insight 0.19.1

## New supported models

* `hglm` (package *hglm*).

## Changes to functions

* Minor improvements to `get_data()` for `t.test()`.

* `format_value()` gets a `lead_zero` argument, to keep or drop the leading
  zero of a formatted value, as well as arguments `style_positive` and
  `style_negative` to style positive or negative numbers.

* `format_table()` now also formats columns named `SGPV` (second generation
  p-values) as p-values.

* Functions for models of class `clm` (like `find_formula()`, `find_variables()`,
  `get_data()` etc.) now also include variables that were defined as `scale` or
  `nominal` component.

## Bug fixes

* Fixed issue in `get_data()` for results from `kruskal.test()`.

* Fixed issue in `find_weights()` for models of class `lme` and `gls`.

* Fixed issue in `get_datagrid()` for models with multiple weight variables.

# insight 0.19.0

## New supported models

* `mmrm` (package *mmrm*), `flac` and `flic` (*logistf*)

## Breaking changes

* `get_data()` was revised and now always tries to recover the data that was
  used to fit a model from the environment. If this fails, it falls back to
  recovering data from the model frame (the former default behaviour).
  Furthermore, the `source` argument can be used to explicitly force the old
  behaviour: `source = "mf"` will try to recover data from the model frame first,
  then possibly falling back to look in the environment.

## New functions

* `n_grouplevels()`, to return random effect groups and number of group levels
  for mixed models.

## Changes to functions

* `get_datagrid()` preserves all factor levels for factors that are hold constant
  at their reference level. This is required to work together with
  `get_modelmatrix()` when calculating standard errors for `get_predicted()`.

## Bug fixes

* Fixed but in `get_modelmatrix()` handling of incomplete factors which
  sometimes had downstream implications for numerical results in the uncertainty
  estimates produced by `get_predicted()`.

* Fixed minor issues for HTML tables in `export_table()` when model parameters
  were grouped.

* Fixed issue with incorrect back-transforming in `get_data()` for models with
  log-transformed variables.

* Fixes issue in `compact_list()`.

* `has_single_value()` now returns `FALSE` when the object only has `NA` and 
  `na.rm = TRUE`.

* Fixed issue in `get_parameters()` for gam-models without smooth terms, or with
  only smooth terms and removed intercept.

# insight 0.18.8

## Bug fixes

* Fixed test due to changes in the _performance_ package.

# insight 0.18.7

## General

* Minor revisions to `get_predicted.glmmTMB()` due to changes in behaviour
  of `predict.glmmTMB()` for truncated-family models since _glmmTMB_ 1.1.5.
  
* New function `has_single_value()` that is equivalent to `length(unique()) == 1`
  (or `n_unique() == 1`) but faster.

## Changes to functions

* `ellipses_info()` now includes an attribute `$is_binomial`, which is `TRUE`
  for each model from binomial family.

## Bug fixes

* Fixed behaviour of the `at` argument in `get_datagrid()`.

* Fixed issue for accessing model data in `get_datagrid()` for some edge cases.

# insight 0.18.6

## New supported models

* Support the *logitr* package: `get_data()`, `find_variables()` and more.

## Bug fixes

* Better detection of unicode-support, to avoid failures when building
  vignettes.

* `get_predicted()` now correctly handles variables of class numeric matrix
  created by `scale()`, which fixes a bug in `performance::check_model()`
  (easystats/performance#432).

* Fixed issue with `iterations` argument in `get_predicted()` with _brms_
  models.

# insight 0.18.5

## Breaking

* `get_df(type = "satterthwaite")` for `lmerMod` objects now return degrees of
  freedom per parameter, and no longer per observation. Use `df_per_obs TRUE`
  to return degrees of freedom per observation.

## New functions

* `safe_deparse_symbol()` to only deparses a substituted expressions when
  possible,which increases performance in case many calls to
  `deparse(substitute())`.

## Changes to functions

* `format_table()` gets a `use_symbols` argument. If `TRUE`, column names that
  refer to particular effectsizes (like Phi, Omega or Epsilon) include the related unicode-character instead of the written name. This only works on Windows for
  R >= 4.2, and on OS X or Linux for R >= 4.0.

* The `stars` argument in `format_table()` can now also be a character vector,
  naming the columns that should include stars for significant values. This is
  especially useful for Bayesian models, where we might have multiple columns
  with significant values, e.g. `"BF"` for the Bayes factor or `"pd"` for the
  probability of direction.

* `get_df()` gets more `type` options to return different type of degrees of
  freedom (namely, `"wald"` and `"normal"`, and for mixed models, `"ml1"`,
  `"betwithin"`, `"satterthwaite"` and `"kenward-roger"`).

* `standardize_names()` now recognized more classes from package _marginaleffects_.

* Minor improvements to `find_parameters()` for models with nonlinear formula.

* Minor speed improvements.

## Bug fixes

* Fixed issue in `get_data()` for models of class `plm`, which accidentally
  converted factors into character vectors.

* Fixed issue with column alignment in `export_table()` when the data frame 
  to print contained unicode-characters longer than 1 byte.

* Correctly extract predictors for `fixest::i(f1, i.f2)` interactions (#649 by 
  @grantmcdermott).
  
# insight 0.18.4

## Changes to functions

* `model_info()` now includes information for `htest` objects from
  `shapiro.test()` and `bartlett.test()` (will return `$is_variancetest = TRUE`).

## Bug fixes

* Fixed issue in `get_data()` which did not correctly backtransform to original
  data when terms had log-transformations such as `log(1 + x)` or `log(x + 1)`.

* Fixed CRAN check issues.

# insight 0.18.3

## New functions

* `format_alert()`, `format_warning()` and `format_error()`, as convenient
  wrappers around `message()`, `warning()` or `stop()` in combination with
  `format_message()`. You can use these funcionts to format messages, warnings
  or errors.

## Changes to functions

* `get_predicted()` for models of class `clm` now includes confidence intervals
  of predictions.

* `format_message()` gets some additional formatting features. See 'Details'
  in `?format_message` for more information and some current limitations.

* `format_message()` gets an `indent` argument, to specify indention string
  for subsequent lines.

* `format_table()` now merges IC and IC weights columns into one column (e.g.,
  former columns `"AIC"` and `"AIC_wt"` will now be printed as one column, named
  `"AIC (weights)"`). Furthermore, an `ic_digits` argument was added to control
  the number of significant digits for the IC values.

* `print_color()` and `color_text()` now support bright variants of colors and
  background colors.

* `get_datagrid()` gets more options for `at` and `range`, to provide more
  control how to generate the reference grid.

* `get_data()` for models of class `geeglm` and `fixest`now more reliably
  retrieves the model data.

## New supported models

* Support for models of class `mblogit` and `mclogit`.

## Bug fixes

* Fixed issues with wrong attribute `adjusted_for` in `insight::get_datagrid()`.

* Fixed issue (resp. implemented workaround) in `get_data.iv_robust()`, which
  failed due to a bug in the _estimatr_ package.

* Fixed issue where `get_predicted()` failed when data contains factors with 
  only one or incomplete levels.

* Fixed issue in `get_predicted()` for models of class `mlm`.

* Fixed issue where `get_predicted()` failed to compute confidence intervals
  of predictions when model contained matrix-alike response columns, e.g. a 
  response variable created with `cbind()`.

# insight 0.18.2

## New functions

* `format_percent()` as short-cut for `format_value(as_percent = TRUE)`.

* `is_converged()`, to check whether a mixed model has converged or not.

## Changes to functions

* `format_table()` gains an `exact` argument, to either report exact or rounded
  Bayes factors.

* `get_predicted()` gets a method for models of class `gamlss` (and thereby,
  `get_loglikelihood()` now also works for those model classes).

* `get_predicted()` now better handles models of class `polr`, `multinom` and 
  `rlm`.

## Bug fixes

* Fixed test failures.

* Minor fixes to address changes in other packages.

# insight 0.18.0

## Breaking changes

* The `ci` argument in `get_predicted()` now defaults to `NULL`. One reason was
  to make the function faster if confidence intervals are not required, which 
  was the case for many downstream usages of that function. Please set `ci` 
  explicitly to compute confidence intervals for predictions.
  
* `get_data()` no longer returns logical types for numeric variables that have
  been converted to logicals on-the-fly within formulas (like `y ~ as.logical(x)`).
  Instead, for each numeric variable that was coerced to logical within a formula
  gets a `logical` attribute (set to `TRUE`), and the returned data frame gets
  a `logicals` attribute including all names of affected variables.

* `parameters_table()`, the alias for `format_table()`, was removed.

## Changes to functions

* `find_transformation()` and `get_transformation()` now also work for models 
  where the response was transformed using `log2()` or `log10()`.

## Bug fixes

* `get_sigma()` for models from package _VGAM_ returned wrong sigma-parameter.

* `find_predictors()` for models from package _fixest_ that contained 
  interaction terms in the endogenous formula part did not correctly return
  all instruments.

* Fixed formatting of HTML table footers in `export_table()`.

* Several fixes to `get_predicted()` for models from `mgcv::gam()`.

* The `component` argument in `find_parameters()` for `stanmvreg` models did
  not accept the `"location"` value.

* `null_model()` did not consider offset-terms if these were specified inside
  formulas.

* Argument `allow.new.levels` was not passed to `predict()` for 
  `get_predicted.glmmTMB()`.
  
* `clean_names()` now works correctly when several variables are specified in 
  `s()` (#573, @etiennebacher).

# insight 0.17.1

## New supported model classes

* `deltaMethod` (*car*), `marginaleffects`, `marginaleffects.summary`
  (*marginaleffects*)

## General

* `get_predicted()` now supports models of class `iv_robust` and `ivreg`.

* For `get_predicted()`, when both `type` and `predict` are given, `type` 
  will overwrite `predict`. Note that this will print a message, because 
  `predict` is the preferred argument.

* `get_varcov()` gains `vcov` and `vcov_args` arguments, to specify the
  variance-covariance matrix used to compute uncertainty estimates (e.g., for 
  robust standard errors).

* `get_loglikehood()` improved handling of models from package *estimator*.

## Bug fixes

* Fixed bug in `get_data()` for model objects whose data needs to be recovered
  from the environment, and where the data name was a reserved word (e.g., named
  like an R function).

* The matrix returned by `get_varcov()` for models of class *bife* now returns 
  row and column names.
  
* `find_offset()` did not find offset-terms for `merMod` objects when the 
  offset was specified as `offset` argument in the function call.

# insight 0.17.0

## Breaking changes

* Arguments `vcov_estimation` and `vcov_type` in `get_predicted()`, 
  `get_predicted_se()` and `get_predicted_ci()` are replaced by `vcov` and
  `vcov_args`, to have a more simplified and common interface to control
  robust covariance matrix estimation.

## General

* Improved performance for various functions, in particular `get_data()` and
  `model_info()`.

## New functions

* To check for names: `object_has_names()` and `object_has_rownames()`

* To work with lists: `is_empty_object()` and `compact_list()`

* To work with strings: `compact_character()`

* Further utility functions are `safe_deparse()`, `trim_ws()` and `n_unique()`.

## Changes to functions

* `export_table()` now better checks for invalid values of caption and footer
  for tables in HTML format, and silently removes, e.g., ansi-colour codes that
  only work for text-format.
  
* `get_data.coxph()` returns the original data frame instead of data with type 
   coercion.

* `get_loglikelihood()` gets a `check_response` argument, to check if a model
  has a transformed response variable (like `log()` or `sqrt()` transformation), 
  and if so, returns a corrected log-likelihood.
  
* `get_modelmatrix()` now supports *BayesFactor* models.

* `get_loglikelihood()` and `get_df()` now support more model classes.

* `get_predicted()` was improved for multinomial models from *brms*.

* `get_variance()` was improved to cover more edge cases of (more complex)
  random effect structures.

* `get_data()` now includes variables in the returned data frame that were
  used in the `subset` argument of regression functions (like `lm()`).

* In some edge cases, where `get_data()` is unable to retrieve the data that 
  was used to fit the model, now a more informative error is printed.

* `ellipses_info()` now also accepts a list of model objects, is more stable
  and returns more information about the provided models (like if all fixed 
  or random effects are the same across models, if all models are mixed models
  or null-models, etc.)

* `check_if_installed()` now works interactively and lets the user prompt
  whether to automatically update or install packages.

## Bug fixes

* Fixed incorrect column name conversion in `standardize_names()` for certain
  columns returned by `broom::glance()`.

* Fixed issue with correctly detecting Tweedie-models in `model_info()`.

* Fixed issue with `get_datagrid()` for *brms* models with monotonic factors.

* Fixed issue in `find_formula()` when argument `correlation` was defined
  outside of `lme()` and `gls()` (@etiennebacher, #525).

* Fixed issue with `get_data()` when back-transforming data from predictors 
  that used `cos()`, `sin()` or `tan()` transformations.

# insight 0.16.0

## New functions

* `get_datagrid()`, to generate a reference grid, usually used when computing
  adjusted predictions or marginal means from regression models.

## Changes to functions

### `get_predicted()`

* `get_predicted()` was revised. Beside the four core options for the `predict`
  argument, it is now also possible to use any value that is valid for the
  model's `predict()` method's `type` argument.

* `get_predicted()` now supports more models (e.g., from packages like 
  _GLMMadaptive_ or _survival_).

* `get_predicted()` is now more robust when calculating standard errors of
  predictions.

### Other functions

* `get_statistic()` and `find_statistic()` now support *htest* objects.

## General

* Various minor improvements.

# insight 0.15.1

## General

* Improved speed performance, especially for `get_data()`.

## Changes to functions

* `get_data()` for `coxph` models now returns the original factor levels for
  variables transformed with `strata()` inside formulas.

# insight 0.15.0

## Breaking changes

* Data management functions (like `reshape_longer()`, or `data_match()`) have
  been moved to the *datawizard* package.

* `get_data()` no longer returns factor types for numeric variables that have
  been converted to factors on-the-fly within formulas (like `y ~ as.factor(x)`).
  Instead, for each numeric variable that was coerced to factor within a formula
  gets a `factor` attribute (set to `TRUE`), and the returned data frame gets
  a `factors` attribute including all names of affected variables.

## New supported model classes

* Support for `bfsl` (*bfsl*)

## New functions

* New `standardize_column_order()` function can be used to standardize the
  column order in output dataframes.

## General

* Improved speed performance for some functions.

* Improved handling of table captions and footers in `export_table()`. See also
  the new vignette on exporting data frames into human readable tables here:
  https://easystats.github.io/insight/articles/export.html

* Revised `width` argument in `export_table()`, which now allows to set
  different column widths across table columns. See examples in
  `?export_table`.

* `export_table()` gets a `table_width` argument to split wide tables into
  two parts.

* `get_varcov()` for `MixMod` (package *GLMMadaptive*) was revised, and now
  allows to return a robust variance-covariance matrix.

* Added more `get_df()` methods.

## Bug fixes

* Fixed issues with manual sigma computation to handle dispersion models in
  `get_sigma()`.

* Fixed issue in `find_formula()` for `BayesFactor::lmBF()` with multiple random
  effects.

* Fixed issue in `get_parameters.BFBayesFactor()` with wrong sign of difference
  estimate for t-tests.

* Argument `width` in `format_value()` was ignored when formatting integer
  values and `protect_integers` was set to `TRUE`.

# insight 0.14.5

## New functions

* `find_transformation()` and `get_transformation()` to find or get any function
  that was used to transform the response variable in a regression model.

## General

* Improved support for models of class `sampleSelection`.

* Improved documentation.

* `get_modelmatrix()` now supports: `rms::lrm`

* `get_predicted()` supports: `MASS::polr`, `MASS::rlm`, `rms::lrm`, `fixest`,
  `bife::bife`, `ordinal::clm`.

* `get_predicted()` standard errors are often much faster to compute.

* `get_predicted()` supports models with "grouped" or "level" outcomes (e.g.,
  multinomial logit).

* `get_predicted()` handles factors better.

* Improved documentation

## Changes to functions

* `check_if_installed()` gains a `quietly` argument, if neither stopping nor a
  warning message for non-installed packages is requested.

* `get_predicted()`'s `predict` argument now accepts these values: "link",
  "expectation", "prediction", "classification", or NULL.

* `get_predicted()` accepts `predict=NULL`, which allows users to push a `type`
  argument through the `...` ellipsis, forward to the `predict()` method of the
  modelling package.

## Bug fixes

* Fixed issue with parameter names from *emmeans* objects in
  `get_parameters()`.

* Fixed issues with unknown arguments in `get_predicted()`.

# insight 0.14.4

## Bug fixes

* Fixed issues due to latest *brms* update.

# insight 0.14.3

## New supported model classes

* `systemfit` (*systemfit*)

## General

* Minor improvements for functions that support printing outputs.

## Changes to functions

* `get_predicted()` gains a new option, `predict = "response"` for binomial
  models.

* Improved stability of `get_variance()` when computing random-slope-intercept
  correlation with categorical random slopes.

* Improved `get_priors()` for *brms* models.

## Bug fixes

* Fixed issue in `get_data()` for *brms* models with auxiliary parameters.

* Fixed issue in `find_formula()` for *brms* models with auxiliary parameters.

* Fixed issue where `get_data()` for *htest* objects did not always preserve
  factors.

* Fixed issue in `format_table()` for ci-levels with longer fractional part.

# insight 0.14.2

## Changes to functions

* `check_if_installed()` gains a `minimum_version` argument, to check if an
  installed package is not older than the specified version number.

* The `package` argument in `check_if_installed()` is now vectorized, so you can
  check for multiple packages in one function call.

* Value formatting functions (like `format_value()` or `format_ci()`) can now
  round to significant digits using `digits = "signif"`.

## Bug fixes

* Fixed issue in `model_info()` with `stan_polr()` models.

* Fixed issue in `find_parameters()` for *brms* when model contained parameters
  for the priors on sigma.

* Fixed issue in `n_obs()` for `stats4::mle()` models.

* Fixed failing tests due to latest *fixest* update.

* Fixed issues due to latest *epiR* update.

# insight 0.14.1

## New functions

* Added several data management and preparation functions: `data_to_long()`,
  `data_match()`, `data_relocate()`, `data_restoretype()`, `force_numeric()`.

## New supported model classes

* Support for `pgmm` (*plm*)

## Changes to functions

* Improved handling of auxiliary parameters for *stanreg* models.

## Bug fixes

* Stability improvements to `get_predicted()`.

* Fixed issues when accessing information from an `afex_aov` model with an empty
  `aov` slot (in anticipation for `{afex}` v.1.0.0).

* Fixed issue in `model_info()` for *stanreg* object with non-standard
  model-family specification.

# insight 0.14.0

## General

* Better support for accessing auxiliary parameters (via `get_sigma()` and
  `get_auxiliary()`, as well as `get_parameters(component = "all")`) for
  `brmsfit` models.

## New functions

* `get_modelmatrix()` as a robust alternative to `model.matrix()` for different
  model classes.

* `format_message()` to format warnings and messages by adjusting the maximum
  line_length, possibly to the width of the console window.

* `format_string()` to shorten a string to a maximum length.

* `check_if_installed()` to see if the needed package is installed.

## New supported model classes

* Support for `mvord` (*mvord*), `SemiParBIV` (*GJRM*), `selection`
  (*sampleSelection*)

## Changes to functions

* `find_formula()` now warns when data name is present in formula, since this
  can result in unexpected behaviour in other package functions.

* `model_info()` returns `is_bernoulli = TRUE` for Bernoulli models.

* Add `get_statistic()` for *lavaan* models.

* `get_df()` supports more models/objects.

* `get_sigma()` supports more models/objects.

* `get_sigma()` and `get_deviance()` for `lrm` models (package *rms*) now only
  return one value, sigma or deviance for the model with intercept and
  predictors.

* `get_deviance()` now works for `glmerMod`, `MixMod` and `glmmTMB` models.

* The behaviour and documentation of the `effects` and `component` arguments, in
  particular for `brmsfit` models, were revised to be more consistent.

* `export_table()` now correctly prints the footer if the input was a list of
  data frames.

## Bug fixes

* Fixed issue (warning) in `get_loglikelihood()` for binomial models with
  non-numeric response variables.

* `find_statistic()` correctly distinguishes t- and z-statistic for *emmGrid*
  objects.

* Fixed issue in `model_info()` for `BGGM` and `mgcv::gam()`.

* Fixed issue in `find_formula()` for `gamlss` models with `random()` function
  in formula.

* Fixed issue with `find_parameters()` for *brmsfit* models when auxiliary
  parameters are directly modelled.

* Fixed issue with `get_parameters()` and `find_parameters()` for multi-group
  *blavaan* models.

* Fixed issue in `ellipsis_info()` when detecting nested models with
  poly-terms.

* Fixed issue in `find_response()` for *brmsfit* models that used the
  `resp_thres()` function in the model formula.

* Fixed issue in `get_predicted_ci()` for models with rank-deficient model
  matrix.

* Argument `zap_small` in `format_value()` did not work properly over vectorized
  vectors.

# insight 0.13.2

## General

* `get_predicted()` has be revamped with a new API and a stable output form (a
  vector). In the course of this revision, a new function `get_predicted_ci()`
  to calculate uncertainty intervals for model predictions.

* Improved support for `orm` (*rms*).

## New supported model classes

* Support for `svy_vglm` (*svyVGAM*), `mjoint` (*joineRML*), `mhurdle`
  (*mhurdle*), `sarlm` (*spatialreg*), `model_fit` (*tidymodels*)

## New functions

* `is_gam_model()` as a small helper to check if a model is a generalized
  additive model with smooth terms.

## Changes to functions

* Added `iterations` argument to `get_predicted()` to control the number of
  draws returned for Bayesian models.

* `model_info()` now returns `$is_gam` if model is generalized additive model
  with smooth terms.

* `format_table()` and `export_table()` now check for valid input (e.g.,
  non-empty data frame) and give an informative message.

* Improved support for `MixMod` (*GLMMadaptive*) in `get_variance()`.

* Improved `print_parameters()`, to allow more flexibility and better cope with
  different output formats.

* `get_parameters()`, `find_parameters()` and `clean_parameters()` for *emmGrid*
  and *emm_list* objects were revised and now better match the actual parameter
  names (also for contrasts).

## Bug fixes

* Fixed issue in `get_variance()` for models without intercept.

* Fixed labelling issue in `get_parameters()` and `clean_parameters()` for
  `blavaan` models.

* `clean_parameters()` for *MCMCglmm* objects did not include random
  parameters.

* Fixed minor issue with unintended sub-titles for `print_html()`.

* Fixed issue in `get_prior()` for `rstanarm::R2()` priors.

# insight 0.13.1

## General

* Improved handling for GAMs.

## New supported model classes

* Support for `elm`, `eglm` (*eflm*)

## Changes to functions

* `get_residuals(..., weighted = TRUE)` doesn't throw warnings if weights are 1
  (no weights specified).

* `n_parameters()` gains a `only_estimable` argument, to remove non-estimable
  parameters from counting the number of parameters for models with
  rank-deficient model matrix.

* `format_ci()` also gains a `zap_small` argument.

## Bug fixed

* Fix or disable failing tests on Mac OS.

* Fixed issues in `get_variance()` with non-correlated random-slope-intercepts
  for *lme4* models.

# insight 0.13.0

## General

* Roll-back R dependency to R >= 3.4.

## New supported model classes

* Support for `crr` (*cmprsk*), `ergm` (*ergm*), `btergm` (*btergm*), `Rchoice`
  (*Rchoice*), `garch` (*tseries*)

## Changes to functions

* Slightly improved handling of different CI-columns in `format_table()`.

* `model_info()` now returns `$is_leventest` if model is an object returned by
  `car::leveneTest()`.

* `get_parameters()` supports `htest` objects.

## Bug fixes

* `get_varcov()` did not properly remove `NA` from rank-deficient models.

* Fixed issue/warning in `get_data()` for some *htest* objects, where the
  environment was not properly evaluated.

* Fixed issue in `format_table()` with p-value formatting, when input data frame
  contained a column named `"p"`, which was not numeric.

* (Hopefully) fixed issue with failing CRAN checks.

# insight 0.12.0

## Breaking changes

* `format_table()` is an alias for `parameters_table()`, and is no longer
  referring to `export_table()`.

## New supported model classes

* Support for `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivFixed`
  (*ivFixed*), `ivprobit` (*ivprobit*), `riskRegression` (*riskRegression*).
  `summary.lm`, `lmodel2` (*lmodel2*), improved support for `bamlss`
  (*bamlss*).

## New functions

* Added `get_deviance()` function that returns the model deviance as a robust
  alternative to `stats::deviance()`.

* Added `model_name()` function that returns the model's "name".

* Added `format()` method for `find_formula()` output to flatten it.

* Added `null_as_ones = TRUE` argument to `get_weights()` to return vector of 1s
  instead of `NULL`.

* Added `get_intercept()` as a helper function to easily retrieve the value at
  the intercept.

* Added `get_df()` as a robust alternative to `stats::df.residuals()`.

* Added `get_predicted()` as a robust alternative to `stats::fitted()`.

* Added `get_loglikelihood()` (and its alias `loglikelihood()`) function as a
  robust alternative to `stats::logLik()`.

* Added `get_residuals()` as a robust alternative extract model residuals.

* Added `ellipsis_info()` to specify the nature of ellipsis (`...`) inputs.

* Added `is_nested_models()` to check if multiple regression models are nested
  (decreasing or increasing).

* Added generic `print_html()`, to allow other packages to create tables in HTML
  format (via `export_table()`) when not printing the output to console.

* Added `is_mixed_model()`, to safely check if a model is a mixed effects model.
  This function also works for multivariate response models.

* `n_parameters()` was moved from *parameters* to *insight*.

## Changes to functions

* `find_formula()`, `find_predictor()`, `find_random()` and related functions
  now also return names of random effects from generalized additive mixed models
  (`gamm`, `gamm4`, `stan_gamm4`).

* Added support for more BFBayesFactor objects.

* `model_info()` now returns `$is_xtab` for `chisq.test()` and
  `BayesFactor::contingencyTableBF()`. Furthermore, the `$family` element for
  those objects is set to `"categorical"`.

* `n_obs()` now handles number of observations from models with binomial family
  correctly when these have matrix-columns as response variable.

## Bug fixes

* Fixed issue in `find_statistic()` for *fixest* models, which did not return
  the correct value `"t-statistic"` for `feols()`.

* Fixes inconsistencies in `get_priors()` for (linear) `BFBayesFactor` models.

# insight 0.11.1

## General

* Warnings that formerly were printed using `print_color()` now use `warning()`,
  to better suppress warning messages if required.

## New functions

* `find_smooth()`, to return in particular smooth terms used in a model.

## Changes to functions

* `get_variance()` and `get_variance_random()` gain a `tolerance`-argument, to
  set the tolerance level for singularity checks when computing random effect
  variances.

* `parameters_table()` formats more objects from the *easystats* packages, like
  ROPE-range or `p_rope()`.

* `find_statistic()` now supports models of class *scam*.

* `get_data()` now also supports `htest`-object, where possible.

## Bug fixes

* Fix CRAN check issues.

* `find_formula()` for `stan_gamm4()` now correctly includes random effects.

# insight 0.11.0

## Breaking changes

* `model_info()` now also detects models from `oneway.test()`, `binom.test()`
  `chisq.test()`, `mcnemar.test()` and `prop.test()`. Furthermore,
  `model_info()` better deals with objects from `BFBayesFactor`, and censored
  regression models no longer return `TRUE` for `$is_linear`.

* `format_table()` is going to be renamed in a future update. Please use its
  alias `export_table()`.

## New supported model classes

* Support for `scam` (*scam*), `meta_random` and `meta_fixed` (*metaBMA*), `Glm`
  (*rms*), `ridgelm` (*MASS*), `mediate` (*mediation*). Partial support for
  `mcmc.list` (e.g. *bayesGARCH*)

## New function

* `parameters_table()`, which was moved from package *parameters* to *insight*.
  Note that this function is going to be renamed into `format_table()` in a
  future update.

* `find_offset()`, to find the name of offset-terms.

* Added generics for `display()` and `print_md()`, to allow other packages to
  create tables in other formats when not printing the output to console.

## Changes to functions

* `standardize_names()` tries to be as loyal to the *broom*-naming conventions
  as possible.

* The function of the `brackets`-argument in `format_ci()` was changed. It is
  now also possible to provide a length-two character vector, to define own
  brackets that encompass the CI-values.

* Related to the change in `format_ci()`, the function of the
  `brackets`-argument in `parameters_table()` was changed accordingly.
  Furthermore, `parameters_table()` gains a `preserve_attributes`-argument, to
  preserve any attributes from the input data frame.

* `export_table()` gains several new arguments that allows to create tables in
  markdown-format.

* `print_parameters()` gains a `keep_parameter_column`-argument, to keep
  (default) both the `"Cleaned_Parameter"` and `"Parameter"` columns, or - if
  `FALSE` - use `"Cleaned_Parameter"` as new `"Parameter"` column.

## Bug fixes

### `get_data()`

* Fixed issue in `get_data()` for `MixMod` objects, which were caused due to
  internal changes in *GLMMadaptive*.

* `get_data()` for zero-inflated models from *pscl* did not include the
  offset-term in cases where the offset was defined as argument, not inside the
  model formula.

* Fixed issue in `get_data()` for `coxph` models with survival-objects with
  `event`-argument as response.

* Fixed edge case in `get_data()` for column name of response values that were
  log-transformed using `log(x+1)`.

### Other bug fixes

* Fixed issue with `survreg` models that included `strata()` in their formula.

* Fixed warning in CRAN checks for forthcoming R-devel.

# insight 0.10.0

## New function

* `get_sigma()` to return the residual standard deviation.

* `standardize_names()`, which was moved from package *parameters* to
  *insight*.

## New supported model classes

* Support for `maov` (*stats*), `HLfit` (*spaMM*), preliminary support for
  `margins` (*margins*), `merModList` (*merTools*).

## General

* Better support for (weighted) multivariate response models of class `mlm` for
  functions like `get_varcov()` or `clean_parameters()`.

* Make `find_formula()` work with t-tests from *BayesFactor*.

* Improved handling for *mira* objects.

## Changes to functions

* `format_bf()` gains a `na_reference` argument, to set the "reference" for
  Bayes factor values that are `NA`, and an `exact` argument for returning
  scientific formatted extreme values.

* `format_value()` gains a `zap_small` argument, to prevent scientific printing
  of numbers if these have more decimal places than indicated by `digits`.

* `get_weights()` now also returns `NULL` when all weights were 1.

* `get_parameters()` for *BFBayesFactor* objects gets a `verbose` argument.

* `get_parameters()` for *emmGrid* and *emm_list* objects gets a `summary`
  argument, to either return the full posterior samples or the summarized
  centrality indices for Bayesian models.

* `find_formula()` for `MuMIn::model.avg()` now tries to retrieve the random
  effects part of a formula, when present.

* `get_weights()` gains a `na_rm` argument to remove possible missing values.

## Bug fixes

* Fix issues with one-sample Bayesian t-tests (
  https://github.com/easystats/parameters/issues/297 ).

* Fix issue in `format_value()` that printed `"100%"` as `"1e+02%"`.

* Removed unnecessary white-spaces in `format_ci()` when upper or lower interval
  was larger than 1e+5.

* `has_intercept()` did not work correctly when intercept was removed from
  formula using `-1`.

* `find_terms()` now shows removal of intercept formula using `-1` as term
  `"-1"`.

* Fix issues with `get_statistic()` for *vgam* models.

# insight 0.9.6

## Changes to functions

* `get_data()` now works for models from `afex_aov()`.

* `get_parameters()` returns a more informative message for `BFBayesFactor`
  objects when not the first model is indexed.

* `clean_names()` now also removes `exp()`-pattern.

* `clean_names()` for character-objects now works with "interaction patterns"
  (like `clean_names("scale(a):scale(b)")`).

* `format_bf()` gains a `protect_ratio` argument, to print numbers smaller than
  1 as ratios.

## Bug fixes

* Fix issues in CRAN checks.

* `get_priors()` now works for more complex `BFBayesFactor` objects that have
  multiple custom priors.

# insight 0.9.5

## Breaking changes

* `get_data()` did not always "back-transform" log-transformed or scaled
  variables to return the original values. Now this bug has been fixed, and
  `get_data()` should return all variables on the original scale (as if these
  variables were not transformed), as stated in the docs.

## Bug fixes

* `get_data()` now returns the correct original data for "empty" polynomials
  (i.e. `poly(x, 1)`).

* Fix CRAN check issues due to latest _estimatr_ update.

# insight 0.9.1

## New supported model classes

* Support for `mipo` (*mice*), `lqmm` and `lqm` (*lqmm*). Preliminary support
  for `semLME` (*smicd*), `mle` (*stats4*) and `mle2` (*bbmle*).

## Changes to functions

* `model_info()` returns `$is_meta = TRUE` for *brms*-meta-analysis models.

* Make `find_statistic()` work with `mgcv::bam()`.

* `get_variance()` now also support `truncated_nbinom2()` family from
  *glmmTMB*.

## Bug fixes

* Fixed issue with correctly detecting sigma-parameters in `find_parameters()`
  for multiple-response `brmsfit`-models.

* Fixed issue with `find_formula()` for models from `stan_nlmer()`.

* Fixed issues with `find_terms()` when response variable included a namespace,
  like `survival::Surv()`.

* Fixed issues with `get_priors()` for _stanreg_ models, probably caused by the
  latest update to *rstanarm 2.21.2*.

* Fixed issues in `get_variance()` for *brmsfit* models.

* Fixed some issues around `crq` objects (package *quantreg*).

# insight 0.9.0

## New supported model classes

* `BGGM` (*BGGM*), `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm`
  (*glmm*), improved support for `manova` (*stats*)

## New functions

* Value formatting functions `format_bf()`, `format_pd()`, `format_p()`,
  `format_rope()` and `format_number()` were moved from package *parameters* to
  *insight*.

## Changes to functions

* `get_variance()` now also returns the correlation among random slopes.

* `get_variance()` now also (partially) supports `brmsfit` models.

* `get_parameters()` for models that return (posterior or simulated) samples of
  model parameters gains a `summary`-argument, which - if `TRUE` - returns a
  point-estimate (mean of samples) instead of the full samples.

* `format_p()` returns `"> .999"` for p-values equal to or greater than 0.999.

## Bug fixes

* Fixed issue in `find_formula()` that did not properly work for models with
  random effects in formula (in *lme4* notation), when random effects were in
  between fixed effects parts.

* `get_variance()` did not return variance components for random effects for
  null-models with random slopes.

* Fixed issue with `get_variance()` for `lme`-models with categorical random
  slope.

* Fixed issue that occurred since R 4.0.0 in `find_weights()` when function call
  had no `weights`-argument.

* Fixed issue in `get_data()` for models with `cbind()`-response variables and
  matrix-like variables in the model frame (e.g. when using `poly()`).

* Fixed issues with `PROreg::BBmm()`, due to changes in latest package update.