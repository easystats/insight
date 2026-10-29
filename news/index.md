# Changelog

## insight 1.6.1

CRAN release: 2026-10-28

### New supported models

- Support for models of class `nlme` from
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html).
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  returns the same parts as for
  [`lme4::nlmer()`](https://rdrr.io/pkg/lme4/man/nlmer.html) models:
  `conditional`, `nonlinear` and `random`. `conditional` has the
  response and the covariates of the fixed-effect parameter formulas.
  `nonlinear` has the nonlinear model, and `random` has the random part.
  As for [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) models,
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  also returns a `correlation` part. Functions that use the formula,
  like
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  [`get_response()`](https://easystats.github.io/insight/reference/get_response.md),
  [`get_random()`](https://easystats.github.io/insight/reference/get_random.md)
  and
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md),
  now work for these models.
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  returns standard errors and confidence intervals for predictions,
  computed with the delta method from the Jacobian of
  [`predict()`](https://rdrr.io/r/stats/predict.html) with respect to
  the fixed effects. Prediction intervals are not yet supported.
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  returns `NULL` with a message, because the random effects are on the
  scale of the nonlinear parameters.

### Changes

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md),
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  and
  [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  no longer return the nonlinear parameters as variables for models from
  [`stats::nls()`](https://rdrr.io/r/stats/nls.html),
  [`lme4::nlmer()`](https://rdrr.io/pkg/lme4/man/nlmer.html) and
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html). For the
  nonlinear formula,
  [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  now returns the same as
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  so transformations like `log(conc)` are no longer returned there.
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for `nls` models now returns the coefficients, not the variables
  ([\#350](https://github.com/easystats/insight/issues/350)).

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  and
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  no longer return the grouping factor of a correlation structure, like
  `Subject` in `corAR1(form = ~ age | Subject)`, as fixed predictor for
  models from *nlme* and
  [`MASS::glmmPQL()`](https://rdrr.io/pkg/MASS/man/glmmPQL.html). It is
  still returned for `effects = "all"`.
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  no longer includes variables that are only used in the correlation
  structure, unless requested in `by`.

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models from
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html),
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html),
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html) and
  [`MASS::glmmPQL()`](https://rdrr.io/pkg/MASS/man/glmmPQL.html) now
  reads the `correlation` part from the fitted correlation structure,
  not from the model call. Before, a structure object that was created
  inside a function gave an error or no `correlation` part. A `form`
  that was not a named argument gave no `correlation` part. A global
  structure object that was changed after the fit gave the changed
  formula. A structure without `form` now returns the grouping that the
  model used, like `~1 | Subject`.
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  and
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  with `effects = "all"` then also return that grouping factor as a
  `correlation` variable. If that formula has no variable, like `~1` for
  `gls()` models, there is no `correlation` part.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *lavaan* models, and for *blavaan* models with `summary = TRUE`,
  now returns standardized parameters only for `standardize = TRUE`, as
  it does for the posterior draws of *blavaan* models. Before, values
  such as `1` or `"TRUE"` also returned standardized parameters, and
  values such as `"yes"`, `NA` or `NULL` gave an error.

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  now works for models from
  [`stats::arima()`](https://rdrr.io/r/stats/arima.html)
  ([\#1274](https://github.com/easystats/insight/issues/1274)). It
  returns the columns of the `xreg` regressors, named like the
  coefficients. For a model with an intercept, the first column is the
  intercept. [`stats::arima()`](https://rdrr.io/r/stats/arima.html) does
  not store `xreg`, so it is evaluated from the model call. The objects
  in `xreg` must be visible from where
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  is called, or from the global environment, and must be unchanged.
  Models from
  [`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
  use their stored regressors. A `data` argument gives an error, because
  new data is not supported.

### Bug fixes

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for multivariate models from package *brms* now also returns the
  predictors of distributional parameters, such as `sigma ~ z`, when the
  data are taken from the model frame. This is the case for models
  fitted with `brm_multiple()`. Before, these columns were missing.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now passes arguments in `...` to
  [`predict()`](https://rdrr.io/r/stats/predict.html) for models that
  use the default method, like *betareg*. Before, these arguments were
  ignored, e.g. `at` for `type = "quantile"`. If
  [`predict()`](https://rdrr.io/r/stats/predict.html) fails with these
  arguments, or they change the shape of the predictions (like
  `se.fit`), predictions are computed without them.

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  for `coxph` models now returns `FALSE` with a message that convergence
  cannot be assessed in two more cases. In the first case, the call does
  not write the formula as a `~` expression, such as `coxph(f, ...)`. A
  control argument of this call uses a variable. Before,
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  evaluated these arguments in the environment of the formula. For a
  model fitted inside a function, an object with the same name in that
  environment gave other control values than the fit used. Models of
  this kind that were assessed correctly before, for example in
  top-level code, now also return `FALSE`. The second case is a
  right-censored single-state model with a `strata()` term whose strata
  variable now puts observations in other strata. Before,
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  flagged coefficients as infinite by mistake. The second case does not
  apply to models fitted with `model = TRUE` or `x = TRUE`.

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  for models from package *VGAM* now returns the standard deviation
  parameter of families that have one, such as `uninormal()`, `tobit()`
  or `binormal()`. Before, it returned a wrong value. A model with more
  than one standard deviation parameter returns one value for each. If
  the standard deviation is modeled with predictors,
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  returns `NULL`.

- [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  for [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) models
  now returns the log-likelihood of the mixed model part (`$lme` or
  `$mer`), which includes the random effects. Before, it used only the
  `$gam` part. For `gamm()` models fitted by PQL, it now returns `NA`
  with a warning, because the log-likelihood of the PQL working model is
  not the one of the GAMM.

- [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  for [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) and
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) models fitted
  by REML now returns the ML log-likelihood with `estimator = "ML"`.
  Before, `estimator` was ignored.

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  for `coxph` models now returns `FALSE` with a message that convergence
  cannot be assessed in three more cases. The first case is a model
  without events. Before, a right-censored model without events gave
  `TRUE`. The second case is a right-censored model whose predictor data
  changed after the fit. Before, the result depended on the change:
  `TRUE`, coefficients flagged as infinite, or a message that the score
  vector was not computed. Models fitted with `model = TRUE` are not
  affected. The third case is a model object with a missing number of
  iterations or missing variances. Before, these gave an error or
  `TRUE`. A model without events and without predictors now returns
  `TRUE`, as other null models. Before, it returned `FALSE`. With
  `verbose = FALSE`,
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  now also gives no message for models that it does not support.

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  for `coxph` models now assesses models from
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  fitted with `method = "efron"` or `method = "breslow"`. Before, it
  returned `NULL`. Models from `clogit()` with predictors and the
  default `method = "exact"` now return `FALSE` with a message that
  convergence cannot be assessed. Single-state counting-process models
  fitted with `y = FALSE` are now assessed. The check for changed
  predictor data now also runs for multi-state models with
  right-censored data. If transitions share a baseline hazard or the
  model was fitted with `x = TRUE`, it does not run. It also runs for
  models with factor predictors or a `strata()` term, which skipped it
  before. A change to the `strata()` variable of a multi-state model is
  still not detected. If the predictor data of these models changed
  after the fit, they now return `FALSE` with a message that convergence
  cannot be assessed. Before, the result depended on the change, and
  coefficients were flagged as infinite by mistake. A model object with
  zero iterations now also returns `FALSE` with this message. If the
  data of the model can be read, the message no longer suggests to refit
  the model with `model = TRUE`.

- [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  and
  [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  now work for brms custom families with brms-only links like `softplus`
  or `squareplus`.

- `export_table(format = "html")` now applies `align` to the columns
  that the table shows, in the order shown. `align = "left"`, `"right"`
  or `"center"` aligns all columns. Before,
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  read these words as one character per column, and it gave an error for
  tables with fewer columns than letters. A string with one character
  per column skips the characters of the group columns (like `Component`
  or the `by` columns). Before, this worked only without `rowname_col`,
  and a group column with one value gave an error. A string with fewer
  characters now applies to the shown columns only. Columns that
  `column_groups` moves under a spanner take the character of their
  shown position. Extra characters are ignored.

- If one group column has one value, or a `by` column is not in the
  data, `export_table(format = "html")` now makes row groups from the
  other group columns with two or more values. Before, it made no row
  groups in these cases. With `row_groups`, it still makes no row groups
  in these cases.

- `export_table(format = "html")` for a list of data frames with an
  `Effects` column now labels the row groups with the table captions
  only, like `"Fixed Effects"`. Before, the labels repeated the
  `Effects` value, like `"fixed - Fixed Effects"`. If the `Effects`
  column has one value for each caption,
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  removes it. Otherwise, it is kept. For lists with an `Effects` or
  `Component` column, the first caption is no longer also used as the
  table title.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  no longer returns `NULL` for models from
  [`MASS::glmmPQL()`](https://rdrr.io/pkg/MASS/man/glmmPQL.html).
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  failed for these models with the error `object 'zz' not found`,
  because the model terms have the working response of the fitting
  algorithm.

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  now works for models from
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) and
  [`MASS::glmmPQL()`](https://rdrr.io/pkg/MASS/man/glmmPQL.html).
  Before, it failed, so
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  returned a distribution-specific variance of 0 for `glmmPQL` models of
  families such as Poisson.

## insight 1.6.0

CRAN release: 2026-10-13

### New supported models

- Support for models of class `glmmTMB` fitted with the new
  [`ordinal()`](https://rdrr.io/pkg/glmmTMB/man/nbinom2.html) family
  (*glmmTMB* \>= 1.1.15). As for
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html), the
  thresholds are returned as conditional parameters ahead of the slopes,
  with standard errors and statistics obtained via the delta method (the
  thresholds are estimated on an internal, softmax-parameterized scale),
  while the intercept, which is fixed to zero by *glmmTMB*, is dropped.
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  returns per-category probabilities for `predict = "expectation"` and
  the most likely category for `predict = "classification"`, and
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  uses the distribution-specific residual variance of the latent scale,
  as for [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html).

### Changes

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  now supports `CmdStanMCMC` models from *cmdstanr*, with the same
  checks and thresholds as for other Stan models. The values come from
  the `$diagnostic_summary()` and `$summary()` methods of the model.
  Other *cmdstanr* models, for example from `$variational()` or
  `$optimize()`, return `FALSE` with a message. `stanfit` models from
  [`rstan::read_stan_csv()`](https://mc-stan.org/rstan/reference/stan_csv.html)
  store the maximum treedepth outside `control`. For these models,
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  counts treedepth hits against this stored maximum, not against the
  default of 10.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *lavaan* and *blavaan* models gets a `type` argument (in `...`).
  If `standardize = TRUE` and `type` is given, it is passed to
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  or
  [`blavaan::standardizedPosterior()`](https://blavaan.org/reference/standardizedPosterior.html),
  for example `type = "std.lv"` or `type = "std.nox"`. The default is
  still `"std.all"`.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) models now
  returns `cor.slopes`, the correlations between random slopes, as for
  *lme4* models. Before, it was missing. This works for nested and
  non-nested grouping factors with general covariance blocks (`pdSymm`
  and the default `pdLogChol`). Blocks of class `pdDiag` or `pdIdent` do
  not estimate correlations, so they add no entry to `cor.slopes`.

- `export_table(format = "html")` passes arguments of
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) from `...` to
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html), for example
  `id` or `rowname_col`
  ([\#631](https://github.com/easystats/insight/issues/631)).

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  now supports models fitted with Stan (`stanfit`, `brmsfit` and
  `stanreg`). Before, it did not work for these models. It checks
  divergent transitions, transitions at the maximum treedepth, E-BFMI,
  R-hat, and bulk and tail effective sample size, with the thresholds of
  the warnings that *rstan* gives after sampling. E-BFMI is computed as
  in
  [`rstan::check_hmc_diagnostics()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html).
  The values of the checks are returned in the `diagnostics` attribute.
  If convergence cannot be assessed, for example for models fitted with
  variational inference or with
  [`brms::brm_multiple()`](https://paulbuerkner.com/brms/reference/brm_multiple.html),
  `FALSE` is returned with a message
  ([\#619](https://github.com/easystats/insight/issues/619)).

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  now supports `coxph` models from *survival*. Before, it did not work
  for these models. It checks whether the model ran out of iterations
  and whether a coefficient is possibly infinite, as *survival* does
  when it fits the model. If a check fails, it returns `FALSE`. The
  attribute `diagnostics` shows the result of each check and the
  coefficients that are possibly infinite. For penalized models, the
  checks do not apply. For models with `ties = "exact"` or `tt()` terms,
  and in the other cases that the help page lists,
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  cannot recompute them. In these cases, it returns `FALSE`. Objects of
  other classes that inherit from `coxph`, such as
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  and
  [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html),
  are not supported
  ([\#1090](https://github.com/easystats/insight/issues/1090)).

### Bug fixes

- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  now work for multivariate *brms* models with addition terms in the
  response, such as `resp_weights()` or `trials()`. Before,
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  returned empty elements with wrong response names, and
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  gave an error (easystats/bayestestR#700).

- If you use the hidden `variable` argument with `summary = TRUE`,
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *brms* models no longer fails with the error
  `object 'parms' not found`. The summary then has the columns
  `Parameter` and `Estimate`.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for zero-inflated *glmmTMB* models no longer fails for
  population-level predictions (`include_random = FALSE`, or a model
  without random effects) when the data object of the model call was
  changed or removed after the fit. Also,
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for zero-inflated *glmmTMB* and *pscl* models now accepts new data in
  the `newdata` argument. Before, it gave the error
  `formal argument "newdata" matched by multiple actual arguments`.

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  now works when the model was fitted inside a function and its call
  uses a local object other than the data, for example `family = fam`.
  Before, it returned `NULL` for mixed models and gave an error for
  other models (easystats/performance#145).

- [`find_random_slopes()`](https://easystats.github.io/insight/reference/find_random_slopes.md)
  no longer reports a grouping factor or a *brms* correlation id as a
  random slope because its name is part of a slope name. An example is
  `p` in `(0 + Intercept | p | g)`. For *brms* models, the explicit
  `Intercept` is no longer a random slope. Before,
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  warned that random slopes were not present as fixed effects for these
  models.

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  with new `data` for *nlme* (`lme`, `gls`), `clmm` and *brms* models
  returns one row per data row. A row with a missing value or a factor
  level that the model data does not have holds `NA` values. Before, it
  was dropped. For *brms* models, an ordered factor given as a character
  vector in `data` gets the polynomial contrasts of the model.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  with new `data` for `lme` and `gls` models returns `NA` for rows with
  a missing value or a factor level that the model data does not have in
  a fixed-effect predictor, with a warning. If no row can be predicted,
  it returns `NULL` with a warning. Before, such a row made
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  return the predictions for the model data. If `data` has no grouping
  column, `lme` models give population-level predictions, as for *lme4*
  models.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for `lme` and `gls` models uses the `ci` argument, with or without new
  `data`. As for other models, the default `ci = NULL` returns no
  intervals or standard errors. Before, the interval level did not
  follow `ci`. For `lme` models, an explicit `level` argument is passed
  to [`predict()`](https://rdrr.io/r/stats/predict.html).

- The confidence intervals of
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for `lme` models, with or without new `data`, use one degree of
  freedom value for all rows, the smallest of the values per
  coefficient. Before, the values per coefficient were recycled against
  the predictions.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  with new `data` for `clmm` models returns `NULL` with a warning.
  Before, it returned the fitted values of the model data, transformed
  with the inverse link. If `data` has the rows of the model data and
  all of its predictor columns with the same values, the fitted values
  are still returned.

- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for *brms* models again returns the standard deviations of smooth
  terms (`sds_*`) in the `smooth_terms` element. It also returns the
  simplex parameters of monotonic effects (`simo_*`) in the `simplex`
  element. For multivariate models, each response gets only its own
  parameters (easystats/parameters#1191)

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `merModList` objects (package *merTools*) gets a `verbose`
  argument. If `verbose = FALSE`, it gives no warning that the data
  cannot be accessed.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) models with
  a random-effect block that has no intercept, for example
  `random = ~ 0 + day | Dog / Side`:

  - For nested grouping factors,
    [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
    returned `NULL`.

  - Without nested grouping factors, `var.slope` left out the first
    slope of the block, and `cor.slope_intercept` returned the
    correlation of two slopes as a slope-intercept correlation.

  `var.slope` of [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)
  models now leaves out the term named `(Intercept)`, not the first term
  of a block. So a block whose intercept is not its first term no longer
  reports the intercept as a slope. Such a block, for example from
  [`nlme::pdBlocked()`](https://rdrr.io/pkg/nlme/man/pdBlocked.html),
  now also has its intercept variance in `var.intercept`.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *lme4* models with uncorrelated random effects, for example
  `(1 + Days || Subject)`, now includes the random-slope variance in
  `var.random`. Before, `var.random` included only the random-intercept
  variance. For nested grouping factors, for example
  `(1 + Days || grp / subgrp)`, `var.random` was `NULL`. This also
  changes
  [`performance::r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.html)
  and
  [`performance::icc()`](https://easystats.github.io/performance/reference/icc.html)
  for these models
  ([\#304](https://github.com/easystats/insight/issues/304),
  easystats/performance#428).

- `export_table(format = "html")` for a list of data frames now shows
  the caption of each data frame as the label of its row group, and the
  footer of each data frame as a source note. Before, a list `footer` or
  a mix of captioned and uncaptioned tables gave an error. Of the
  footers stored as attributes, only the first one was kept, and a list
  `caption` became one title. If any data frame in the list has a
  `Component`, `Effects`, `Group` or `Response` column, the old output
  stays, but a list `footer` no longer gives an error
  ([\#631](https://github.com/easystats/insight/issues/631)).

- `export_table(format = "html")` for a single data frame with a list
  footer (a multi-line, colored footer) gave an error. The parts of the
  footer are now joined into one source note, as in text output.

- `export_table(format = "tt")` for a list of data frames now shows the
  caption of each data frame as the label of its row group, and the
  footer of each data frame as a note. Before, the captions were shown
  as an extra `group` column, the first caption became the title, and
  only the footer of the first data frame, or only `footer` if given,
  was kept. A list `caption`, or a mix of `table_title` and
  `table_caption` attributes, gave an error. An unnamed `column_names`
  vector names the shown columns. Before, it also needed a name for the
  `group` column. The old behavior stays if a data frame has one column,
  no rows, row groups (`indent_rows`) or a `Component`, `Effects`,
  `Group` or `Response` column, if the data frames have different column
  names, or if `by`, `row_groups` or `column_groups` is given. For these
  lists, each element of a list `footer` still gives one note. Each
  footer changes as for a single data frame. The color is not shown as a
  note, new lines at the start and end are removed, and an empty footer
  gives no note.

- `export_table(format = "tt")` for a single data frame now shows a
  footer as one note. A character vector gives its first string, because
  the second string is the color, and a list gives its strings joined.
  Before, each string was a note, so the color of a colored footer was
  shown as a second note, and a list footer with colors (a multi-line,
  colored footer) gave an error.

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md),
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  and
  [`get_variance_residual()`](https://easystats.github.io/insight/reference/get_variance.md)
  now support *brms* models where `sigma` has an intercept-only formula,
  for example `bf(y ~ x, sigma ~ 1)`. Sigma is the mean of the posterior
  draws of `b_sigma_Intercept`, back-transformed with the link function
  of `sigma`. Before,
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  returned `NULL` and
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  returned a residual variance of 1. If `sigma` varies, for example with
  `sigma ~ (1 | g)`,
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  now returns no residual variance with a message, as it already did for
  `sigma ~ x`
  ([\#967](https://github.com/easystats/insight/issues/967)).

- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md),
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  and
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  now work for *blavaan* models with multiple groups and equality
  constraints, for example `group.equal = "loadings"`. Before,
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  failed with the error “replacement has length zero”.
  `get_parameters(standardize = TRUE)` now gives the correct names for
  the posterior draws of all *blavaan* models. Before, the names were
  shifted, because the standardized draws also include the fixed
  parameters.
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  now also works for *blavaan* models with user-defined parameters
  (`:=`) and returns their posterior draws. Before, it failed with the
  error “Arguments must be mcmc objects”. `get_data(source = "mf")` now
  works for *lavaan* and *blavaan* models with multiple groups and
  returns the grouping variable as a column. Together, these fixes let
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  work for multiple-group *blavaan* models (easystats/parameters#735).

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  for *fixest* models now supports `vcov = "HC0"`. Before, it gave an
  error. The result is the same as
  `fixest::vcov_hetero(ssc = fixest::ssc(K.adj = FALSE))`, the
  heteroskedasticity-consistent matrix without the small-sample
  adjustment. For `"HC"`, `"HC4"`, `"HC4m"` and `"HC5"`, the error now
  names the supported types
  ([\#1251](https://github.com/easystats/insight/issues/1251)).

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  and
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  no longer return the matrix of the *brms* autocorrelation terms
  `sar()`, `car()` and `fcor()`, for example `W` in `y ~ x + sar(W)`.
  This matrix is an object in `data2`, not a variable in the data. The
  grouping variable of `car()` is still returned.

- If *performance* is not installed,
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  now gives an error that asks you to install it. Before, it returned
  `NULL` without a message
  ([\#928](https://github.com/easystats/insight/issues/928)).

- `n_obs(disaggregate = TRUE)` now returns the total number of trials
  for *brms* models with a `trials()` term in the response, for example
  `y | trials(size) ~ x`. Before, it ignored `disaggregate` and returned
  the number of data rows
  ([\#581](https://github.com/easystats/insight/issues/581)).

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  and
  [`get_variance_distribution()`](https://easystats.github.io/insight/reference/get_variance.md)
  returned a wrong distribution-specific variance for beta-binomial
  models with a probit or cloglog link. The cause was the
  back-transformation of the null-model mean, which always used the
  inverse logit. The mean now uses the inverse link of the model.

- [`get_residuals()`](https://easystats.github.io/insight/reference/get_residuals.md)
  for *gamlss* models now returns response residuals (observed minus
  expected values) for `type = "response"`. Before, it returned the
  normalized quantile residuals without a warning. The
  [`mean()`](https://rdrr.io/r/base/mean.html) function of the family
  gives the expected values. Thus, the result is also correct for
  families where `mu` is not the mean, for example zero-inflated
  families. For families without a
  [`mean()`](https://rdrr.io/r/base/mean.html) function, the residuals
  use `mu`, with a warning
  ([\#327](https://github.com/easystats/insight/issues/327)).

- For a list of data frames, `caption = ""` (or `title = ""`) now makes
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  remove the captions of all tables. This includes captions stored as
  attributes. `subtitle = ""` and `footer = ""` do the same for
  subtitles and footers. In a list of captions or footers, `""` removes
  the caption or footer of that table. Before this fix,
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  still printed the attributes
  ([\#930](https://github.com/easystats/insight/issues/930)).

- For univariate non-linear *brms* models (`nl = TRUE`),
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  now returns the coefficients of the non-linear parameters of `mu` as
  conditional parameters, and their group-level terms as random
  parameters. Before, they were returned as auxiliary parameters.
  [`find_auxiliary()`](https://easystats.github.io/insight/reference/find_auxiliary.md)
  no longer returns the names of these non-linear parameters.
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  labels their coefficients like
  [`summary()`](https://rdrr.io/r/base/summary.html) from *brms*, for
  example `ult_Intercept`. The `effects`, `component` and `parameters`
  arguments of
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  now select the coefficients of non-linear parameters.
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md),
  `n_parameters(component = "conditional")` and
  `find_parameters(component = "location")` now include these
  coefficients, where they included none before. This also applies to
  non-linear parameters that are nested with `nlf()`
  ([\#1076](https://github.com/easystats/insight/issues/1076)).

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *brms* models now returns `NULL` when no parameter matches the
  selection of `effects`, `component` and `parameters`. Before,
  depending on the selection, it returned the posterior draws of all
  parameters, an empty data frame, or an error
  ([\#1076](https://github.com/easystats/insight/issues/1076)).

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  for `clmm` models now uses the contrasts stored in the model, so
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  works for `clmm` models fitted with, for example,
  `contrasts = list(x = "contr.sum")`. For `clmm` models, a `data`
  argument is now used. For `clmm` and `brmsfit` models, factor
  variables in `data` get the levels of the model data, and the
  contrasts of the model are used. Thus, new data with only some of the
  levels gives the correct columns. A `contrasts.arg` list is now
  accepted for both model classes.

- If variables used in the model are missing from the data in the
  environment, but are columns of the model frame,
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now warns and takes the data from the model frame. This applies, for
  example, to `lm`, `glm`, `lmer`, `glmmTMB` and `gam` models. Variables
  are missing, for example, if you overwrite the data object after the
  model fit. Before this fix,
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  silently dropped these variables. Then
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md),
  and
  [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  for `glm` models, failed with “object not found” errors. Variables
  used only inside a transformation, like `a` in `log(a)`, are not
  detected ([\#1210](https://github.com/easystats/insight/issues/1210)).

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  for `lme` and `gls` models now uses the contrasts stored in the model
  instead of treatment contrasts. You set these contrasts with
  `contrasts = list(x = contr.sum)` in `lme()`, or with
  `options(contrasts = ...)` when you fit the model. If you provide
  `data`, its factor and character variables get the levels of the model
  data. Thus, new data with only some of the levels gives the correct
  columns.

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  for `lme` and `gls` models with new `data` now keeps the basis of
  terms like [`poly()`](https://rdrr.io/r/stats/poly.html) from the
  model data. Thus, the columns match the fitted coefficients. If
  another package, like *MuMIn*, provides its own
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) method
  for `lme` objects,
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  now still uses `data` and the model contrasts.

- `format_table` now correctly formats labels consistently when
  parameter names occurred multiple times.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `coxph` models no longer drops variable and value labels when
  `source = "mf"`.

- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for *brms* models no longer drops parameters whose names start with
  the name of a distributional parameter, e.g. the coefficient
  `b_condition2` in a model with a (custom) distributional parameter
  `c`.

- [`find_auxiliary()`](https://easystats.github.io/insight/reference/find_auxiliary.md)
  no longer returns `"sigma"` for *brms* models that have no residual
  standard deviation, but auxiliary parameters whose names merely
  contain `"sigma"` (e.g. `"sigmadrift"` or `"sigmabias"` from custom
  families).

- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  no longer assigns auxiliary parameters whose names contain `"sigma"`
  (like `"sigmabias"`) to the `"sigma"` component. These are now
  returned as their own component, which also fixes the related grouping
  in
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  returned wrong values for `lme` models with nested random effects, for
  example `random = ~ 1 | Dog / Side`. It returned the standard
  deviations of the random effects instead of their variances, and it
  gave them the wrong group names. This also gave wrong results for
  [`performance::icc()`](https://easystats.github.io/performance/reference/icc.html)
  and
  [`performance::r2()`](https://easystats.github.io/performance/reference/r2.html).
  For groups with three or more correlated random terms, the variances
  and covariances of the random effects were also wrong.

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  now returns the correct random effects formula for
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) and
  [`MASS::glmmPQL()`](https://rdrr.io/pkg/MASS/man/glmmPQL.html) models
  with a named list or a `pdMat` object in the `random` argument.
  Examples are `random = list(g = ~1)` and `random = pdDiag(~x)`. Before
  this fix,
  [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  returned `NULL` for these models, and
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  failed. If the `random` argument names an object that was defined
  inside a function,
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  also no longer fails.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for `coxph` models with `predict = "survival"` now computes confidence
  intervals on the cumulative-hazard scale and back-transforms them, as
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  does by default. Before this fix, the intervals were symmetric around
  the survival probability and were not always between 0 and 1.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for `coxph` models with `predict = "expectation"` or
  `predict = "risk"` now returns standard errors and no longer warns
  that the delta method could not be applied.

## insight 1.5.4

CRAN release: 2026-09-05

### Bug fixes

- Fixed a bug where
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  incorrectly extracted degrees of freedom (df) instead of the t value
  for `rlmerMod` models (*robustlmm*) after Satterthwaite degrees of
  freedom were cached (e.g., following `emmeans` calls).

- Fix failing CRAN checks.

## insight 1.5.3

CRAN release: 2026-08-25

### Changes

- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  now supports the `df_per_obs` argument for models of class `mmrm`,
  returning one degree of freedom per row of `data` instead of one per
  coefficient, using the method chosen when fitting the model.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  and
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now recognize the `ddm()`, `lba()` and `rdm()` custom *brms* families
  from package *cogmod* as reaction-time and choice models
  (`is_rtchoice`), alongside the already supported `lnr()`. Their
  predictions are hence split into the `"rt"` and `"response"`
  components, instead of being returned as a single interleaved vector.

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gains a `weighted` argument, to create a smaller representation of
  large data grids, where multiple unique combinations of predictors are
  included only once, and a new `Weight` column indicates how often each
  combination appears in the original data. The related `n_bins`
  argument indicates how numeric values are represented in the reduced
  data grid.

- [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  gains a `separator` argument, to customize the separator for lower and
  upper CI values.

- [`find_response()`](https://easystats.github.io/insight/reference/find_response.md),
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md),
  and
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  now also work for objects from package *marginaleffects*.

- Preliminary support in
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for objects from package *marginaleffects*.

- `get_variances()` now passes `...` down to
  [`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html), which might
  be useful for Bayesian models (e.g. to return robust estimates using
  `robust = TRUE`).

- Added a documentation page for available `options`.

- Updated test-files for `htest` objects and fixed deprecated names.

### Bug fixes

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  no longer fails for models whose offset is written inline in the
  formula with a nested-parenthesis expression followed by further terms
  (e.g. `y ~ x + offset(log(exposure)) + factor(year)`). The offset term
  is now extracted from the formula’s language tree instead of by a
  paren-blind regex.

## insight 1.5.2

CRAN release: 2026-06-28

### Changes

- Major performance improvement for `compact_list` on very large, nested
  list objects.

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  now explicitly works for `lavaan` objects.

### Bug fixes

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  now correctly handles *brms* models fitted with `0 + Intercept`
  formulas.

- [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  did not return degrees of freedom for Student’s t-distribution for
  *rstanarm* models.

- Fixed issue in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *brms* intercept-only models.

## insight 1.5.1

CRAN release: 2026-05-21

### Changes

- New support for `CmdStanFit` models from the *cmdstanr* package -
  [`find_algorithm()`](https://easystats.github.io/insight/reference/find_algorithm.md),
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md),
  and
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  now work with these models.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now correctly detects t- and z-tests from package *BSDA*.

- [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md)
  gains a `digits` argument.

- More informative error message when the response variable in
  [`get_response()`](https://easystats.github.io/insight/reference/get_response.md)
  is not present in the data.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  now also formats columns for tail and bulk ESS from Bayesian models.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now supports objects of class `mira` from package *mice*.

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  now supports the option `"fpc"` to apply finite population correction
  (Lai et al. 2018).

- Added a new function,
  [`vcovFPC()`](https://easystats.github.io/insight/reference/vcovFPC.md),
  to calculate finite-population-adjusted variance-covariance matrices.

### Bug fixes

- [`has_intercept()`](https://easystats.github.io/insight/reference/has_intercept.md)
  now correctly detects whether models from packages *fixest* and *lfe*
  have intercepts or not.

- The `vcov` argument in
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  was ignored when `vcov` was of class `"dpoMatrix"` and did not return
  `TRUE` to [`is.matrix()`](https://rdrr.io/r/base/matrix.html).

## insight 1.5.0

CRAN release: 2026-04-14

### Changes

- New function,
  [`get_simulated()`](https://easystats.github.io/insight/reference/get_simulated.md),
  to return simulated values from the response of regression models,
  which is comparable to posterior predictions from a model.

- Improved performance of
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for models from package *mgcv* that include random effects.

- Improved performance of
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  for models from package *mgcv* that include random effects.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  supports models from package *nestedLogit*.

- Added support for `aft`, `stpm2`, and `pstpm2` models from package
  *rstpm2*.
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md),
  and
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now work with these flexible parametric survival models.

### Bug fixes

- Fixed issue with duplicated
  [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  and missing
  [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  for `phyloglm` models.

## insight 1.4.6

CRAN release: 2026-02-04

### Breaking Changes

- Revised weighted residuals (`get_residuals(weighted = TRUE)`) to be in
  line with R 4.6.0, which now returns weighted working residuals for
  `glm` objects.

## insight 1.4.5

CRAN release: 2026-01-26

### Changes

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md),
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  and
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now work with tidymodels workflow objects
  ([\#1161](https://github.com/easystats/insight/issues/1161)).

- Improve performance for
  [`is_empty_object()`](https://easystats.github.io/insight/reference/is_empty_object.md)
  for large data frames (which, in turn, should improve performance of
  many other functions relying on this helper-function, including from
  other packages).

### Bug fixes

- Disabled test for the *feisr* package, which currently seems to be
  broken.

- Fixes issue where
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  failed on random effects with tokens in `by`.

## insight 1.4.4

CRAN release: 2025-12-06

### Bug fixes

- Fixed issue with
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md)
  due to changes in *lme4* package.

- Fixed issue in `get_data(source = "mf")` for models from package
  *fixest* when `lean` was set to `TRUE` in the model call.

## insight 1.4.3

CRAN release: 2025-11-24

### Changes

- Added support for models from package *lcmm* (`lcmm`, `externVar`).

- Support for objects from the *modelbased* package. This includes
  methods for
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md),
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md),
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md),
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md),
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md),
  [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md),
  and
  [`get_call()`](https://easystats.github.io/insight/reference/get_call.md).

- Better formatting for p-values-columns from equivalence tests from
  other packages.

- [`format_value()`](https://easystats.github.io/insight/reference/format_value.md),
  [`format_number()`](https://easystats.github.io/insight/reference/format_number.md),
  and
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gain a `big_mark` argument to format numbers with thousands separators
  (e.g., `1,234,567.89` or `1 234 567.89`). This makes large numbers
  more readable in tables and formatted output. When `big_mark` is
  provided, scientific notation is suppressed for large numbers to show
  the full value with separators.

- Improved support for models from package *survey*. This includes a
  dedicated `source` argument for methods such as
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md),
  [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md),
  [`find_weights()`](https://easystats.github.io/insight/reference/find_weights.md),
  or
  [`get_response()`](https://easystats.github.io/insight/reference/get_response.md),
  to specify whether the data should be extracted from the model frame
  of the survey design object (`"mf"`), which is usually equivalent to
  the original data, or from the data that can be extracted from the
  model-object in the environment (`"environment"`) , which usually
  includes processed variables (like the `"(weights)"` variable for
  weights).

- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  now supports Kenward-Roger and Satterthwaite degrees of freedom for
  models from package *glmmTMB*. This requires package version 1.1.3.

### Bug fixes

- Fix CRAN check failures.

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for models from package *survey*.

- Fixed issue in
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  when the `data` argument was provided and contained columns with only
  missing values.

## insight 1.4.2

CRAN release: 2025-09-02

### Bug fixes

- Fix CRAN failures for Mac OS R-old-release.

## insight 1.4.1

CRAN release: 2025-08-29

### Changes

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  now supports robust variance-covariance matrices for models of class
  `glmmTMB` (package *glmmTMB*). The argument `vcov` can be used to
  specify the robust variance-covariance matrix function. Furthermore,
  the `component` argument gains the `"full"` option, to return the full
  variance-covariance matrix, including the random effects (theta
  parameters).

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  now also formats ROPE columns for superiority and inferiority.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  protects integer columns for non-specific column types.

- The `numerics` argument in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gains two new options, `"integer"` and `"mode"`, to either return the
  rounded mean or the most frequent value of a numeric vector.

- Modified code base to address changes in the *marginaleffects* package
  from version 0.29.0 onwards.

### Bug fixes

- Fixed issue with
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  with *mhurdle* models.

## insight 1.4.0

CRAN release: 2025-08-18

### Breaking changes

- `apply_table_theme()` was removed, since it was an experimental
  feature that is no longer used in any package.

### Changes

- [`display()`](https://easystats.github.io/insight/reference/display.md),
  [`print_md()`](https://easystats.github.io/insight/reference/display.md)
  and
  [`print_html()`](https://easystats.github.io/insight/reference/display.md)
  get a `.table` method.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now supports
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), and returns
  the expected frequencies.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gains better support for the *tinytable* package. Use `format = "tt"`
  to export tables into the tinytable-format. This can also be used with
  grouped tables, i.e. `by = "group"`.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gains arguments `row_groups` and `column_groups`, to group rows and
  columns in the exported table. Column groups currently only work for
  `format = "tt"`.

- If arguments `title`, `subtitle` and `footer` in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  are set to an empty string `""`, no titles/subtitles/footers are
  printed, even if present as attributes.

- Added a `.lavaan` method for
  [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md).

- The formerly internal function to extract various information about
  mixed models is now exported as
  [`get_mixed_info()`](https://easystats.github.io/insight/reference/get_mixed_info.md).

### Bug fixes

- Fixed issue with models of class `selection` with multiple response
  variables.

- Fixed issue in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  for factors with `=` in their levels.

- Fixed issue in
  [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  for multivariate response models of class `brms` with special response
  options.

- Fixed issue in several functions for certain betareg-models that
  contained a `"mu"` component instead of `"mean"`.

- Fixed CRAN check issues on M1 Macs.

## insight 1.3.1

CRAN release: 2025-06-30

### Changes

- New function
  [`get_model()`](https://easystats.github.io/insight/reference/get_model.md)
  to extract the model object from an arbitrary object, if the model
  object is stored as attribute of the parent object.

- The `range` argument in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gets a new option, `"pretty"`, to create a range of pretty values.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now supports models of class `glmtoolbox::glmee`.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  supports predicting the class membership for models from package
  *brms* with `mixture()` family, using `predict = "classificaton"`.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  supports predicting the outcome by class membership for models from
  package *brms* with `mixture()` family, using `predict = "link"`.

- [`get_residuals()`](https://easystats.github.io/insight/reference/get_residuals.md)
  gets a method for objects from
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html),
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) and
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html).

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  returns `$is_mixture` to identify finite mixture models.

- Better support for models of class `sdmTMB`.

- Improve efficiency of
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  for more complex *brms* models.

### Bug fixes

- Fixed issue in
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  for models from package *afex*.

- Fixed issue in
  [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  for *brms* models with `mm()` in formula.

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for *brms* models with `mmc()` in formula.

- Fixed issue in
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  for objects of class `aov`.

## insight 1.3.0

CRAN release: 2025-05-20

### Breaking Changes

- The default option `"all"` for the `effects` argument of
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for models from package *brms* and *rstanarm* has a new behaviour and
  only returns fixed effects and random effects variance components, but
  no longer the group level estimates. Use `effects = "full"` to return
  all parameters. This change is mainly to be more flexible and gain
  more efficiency for models with many parameters and / or many
  posterior draws.

### New functions

- [`is_bayesian_model()`](https://easystats.github.io/insight/reference/is_bayesian_model.md)
  as a convenient shortcut to check whether a model is Bayesian or not.

### Changes

- Revised wording for alerts from
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md).

- The `effects` argument of
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for models from package *brms* and *rstanarm* get two new options,
  `"grouplevel"` and `"random_variances"`, to return only random effects
  variance components, or group level effects. This is more efficient
  especially for models with many samples and many parameters.
  Additionally, a `variable` argument can be passed to
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md),
  which is in turn passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), to
  extract parameters more efficiently.

- The `by` argument in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  now also splits tables when format is not `"html"`.

### Bug fixes

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models of class `barts` (package *dbarts*), when formula was
  abbreviated using `y ~ .`.

## insight 1.2.0

CRAN release: 2025-04-22

### Breaking Changes

- The handling of `brms` models in
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md),
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  or
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  (and other functions) should now systematically take the many possible
  distributional parameters into account, identifying the different
  types of them, and assigning them to own values in the `Component`
  columns. *insight* should now be flexible enough to also cope with
  user-defined variables that have been modelled as distributional
  parameters.

- To be consistent with the naming pattern from package `brms`, all
  elements related to distributional parameters now returns exactly that
  “dpar” name. This means that, for instance,
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  no longer returns an element named `$zero_inflated`, but instead
  `$zi`. This only applies to models from `brms`! All other packages are
  not affected by this breaking change.

### Changes

- [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  and
  [`find_random_slopes()`](https://easystats.github.io/insight/reference/find_random_slopes.md)
  now also extract random effects names from auxiliary components.

- [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  and
  [`find_random_slopes()`](https://easystats.github.io/insight/reference/find_random_slopes.md)
  now include random effects from the dispersion component for models
  from package *glmmTMB*.

- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  for brms-models now assigns auxiliary parameters to their related
  `Component` and overwrites former assignments to `"conditional"`.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  now includes more effect sizes when formatting column names.

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  now allows named vectors for arguments `length` and `range`, to match
  values with target variables defined in `by`.

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gets a `protect_integer` argument, to allow a spread of values from
  minimum to maximum of length `length`, also for integer values.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gets an argument `column_names`, to change the column names of the
  exported table.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  gains a `response` argument for classes that can be multivariate
  response models that return multiple lists of model information
  (currently, Stan models from *rstanarm* and *brms*). If not `NULL`,
  only the information for one of the response variables is returned.

- Creating a range of values in a
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  using `by` was now simplified and works like regular R syntax,
  e.g. `by = "mpg = 20:50"`.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for models of class `brmsfit` now supports Wiener models or similar so
  called “decision models”, that simultaneously model, e.g., reaction
  times and (discrete) choices.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  no longer throws warnings for models of class `brmsfit` when
  distributional are predicted.

- [`find_offset()`](https://easystats.github.io/insight/reference/find_offset.md)
  gets an `as_term` argument, which returns the offset as term,
  including possible transformations.

- Token-option `"quartiles2"` was removed and option `"quartiles"` now
  produces the three quartile values (lower hinge, median, upper hinge).
  The former option of `"quartiles"` that included minimum and maximum
  was identical to the already existing `"fivenum"` option.

- New function
  [`find_auxiliary()`](https://easystats.github.io/insight/reference/find_auxiliary.md),
  which is a small helper to extract all distributional parameters that
  were used in models from package *brms*.

- [`display()`](https://easystats.github.io/insight/reference/display.md),
  [`print_md()`](https://easystats.github.io/insight/reference/display.md)
  and
  [`print_html()`](https://easystats.github.io/insight/reference/display.md)
  get methods for matrix- and array objects.

- Cleaning / revising package documentation.

### Bug fixes

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  now correctly calculates the null-model based on the data that was
  used to fit the model (model frame), which can lead to different
  results when the original data contained missing values.

- Fixed issue for
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  with multivariate response models.

## insight 1.1.0

CRAN release: 2025-03-01

### Breaking Changes

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  no longer creates fractional parts when creating a range of values for
  numeric variables that are integers.

### Changes

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  now includes a dummy column for model weights (values `NA`), to work
  with models that used weights.

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gets a `digits` argument, to round numeric representative values.

- Argument `ci_digits` defaults to `digits` in
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md).

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  gets a `select` argument, which can be used to select columns and
  column layout in a glue-like style.

- [`find_response()`](https://easystats.github.io/insight/reference/find_response.md)
  now also works for *tidymodels* workflows.

- [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  and
  [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  now also detect log-transformation with logarithmic base.

### Bug fixes

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  [`find_response()`](https://easystats.github.io/insight/reference/find_response.md)
  and
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  for multinomial `gam` models from package *mgcv*.

## insight 1.0.2

CRAN release: 2025-02-06

### Changes

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gives a more informative error message when a variable specified in
  `by` was not found in the data.

- The `by` argument in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gets a new token-option, `"[sample <number>]"`, to draw a random
  sample of values.

### Bug fixes

- Option `"terciles"` and `"terciles2"` in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  were swapped, i.e. `"terciles"` was doing what was documented for
  `"terciles2"` and vice versa. This has been fixed.

- `include_random` in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  now works for nested random effects, i.e. for more than one group
  level factor in the random effects.

- Fixed issue in
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  for models of class `brmsfit` that included monotonic effects.

## insight 1.0.1

CRAN release: 2025-01-10

### General

- Support for models of class `oohbchoice` (package *DCchoice*).

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  gets a `stars_only` argument, to format p-value columns to contain
  only significance stars.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for brms-models with categorical family now includes the data of the
  data grid in the returned predictions, for better orientation.

### Bug fixes

- Fixed issues due to latest *mice* updates.

- Fixed typo in `get_parameters.glmmadmb()`, which was erroneously
  renamed into `get_parameters.glmmTMBadmb()`.

- Fixed issues in
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  and
  [`has_intercept()`](https://easystats.github.io/insight/reference/has_intercept.md)
  for *brms* models with `0 + Intercept` formula notation.

- Fixed issues in
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  for models of class `fixest` from negative-binomial families.

- Fixed issue with [`as.numeric()`](https://rdrr.io/r/base/numeric.html)
  method for
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md).

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  now only returns valid levels when `include_random = TRUE` and
  group-level factor in random effects is numeric.

## insight 1.0.0

CRAN release: 2024-11-26

### Breaking changes

- All deprecated arguments have been removed.

- The `table_width` argument in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  now defaults to `"auto"`.

### General

- [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  can now deal with any power-transformation, and also returns results
  for divisions (scaled response) and Box-Cox transformations.

- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  and
  [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  now also detects use of divisions, like `x/3` or Box-Cox
  transformations (like `(x^lambda - 1) / lambda`).

- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  and
  [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  get a `include_all` argument, to check all model terms for
  transformations.

- [`get_dispersion()`](https://easystats.github.io/insight/reference/get_auxiliary.md)
  is now an exported function.

- Updated
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  (and related documentation) to support new covariance matrix
  estimation methods from the **sandwich** package.

- New function
  [`validate_argument()`](https://easystats.github.io/insight/reference/validate_argument.md)
  as a replacement for
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) with more
  informative error message.

- The function to calculate the corrections for likelihood-values when
  the response-variable is transformed is now exported as
  `get_likelihood_adjustment()`.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  can now split tables into more than three tables when `table_width` is
  used (formerly, the maximum number of split tables was three).

- Changed (improved) formatting for parameter tables in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md),
  when `format = "html"`. Rows are indented, and group headers are
  emphasized in italic.

- [`formula_ok()`](https://easystats.github.io/insight/reference/find_formula.md)
  now also checks for syntactically invalid variable names. Furthermore,
  argument `checks` now allows to specify for which possibly problematic
  formula notation should be checked.

- [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  gains a `decimal_point` argument, to change the decimal point in
  output conversion.

- [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md)
  with `stars = TRUE` uses the `°` symbol for inferiority (evidence
  *against* the comparison).

- Added support for `coxph.panel` models.

- Added support for models of class `asym` (package *panelr*).

- Overhaul of documentation for the package-functions.

### Bug fix

- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  now uses the correct labels for the random effects variances
  (`"SD/Cor"` has changed to `"Var/Cov"`).

- When
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  could not properly evaluate the subset of a data set, it now returns
  an informative warning and no longer errors.

- Fixed inaccuracy in
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  for models of class *brmsfit*.

- Fixed issues in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for models of class *brmsfit* when the sigma-parameter was directly
  modeled.

- Fixed issue in
  [`compact_character()`](https://easystats.github.io/insight/reference/compact_character.md)
  and
  [`compact_list()`](https://easystats.github.io/insight/reference/compact_list.md)
  for date-variables.

- Fixed edge case in
  [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  for simple log-transformation of the response variable.

- Fixed issue for `model_info.averaging()`.

## insight 0.20.5

CRAN release: 2024-10-02

### General

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  can now be used to extract the “grid” information from
  [emmeans](https://rvlenth.github.io/emmeans/) and
  [marginaleffects](https://marginaleffects.com/) outputs.

- Arguments `na.rm` and `na_rm` are deprecated throughout the package’s
  functions. Instead, use `remove_na`.

### Bug fixes

- Fixed rendering issue of the example in
  [`?insight::display`](https://easystats.github.io/insight/reference/display.md).

- Fixed issues due to recent changes in the *glmmTMB* package.

## insight 0.20.4

CRAN release: 2024-09-01

### New supported models

- Support for models of classes `glm_weighit`, `multinom_weightit` and
  `ordinal_weightit` (package *WeightIt*).

### Changes

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  and
  [`formula_ok()`](https://easystats.github.io/insight/reference/find_formula.md)
  now warn when indexed data frames, such as `df[, 5]`, are used as
  response variable in the formula, as this can lead to unexpected
  results.

- Minor improvements to
  [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  and
  [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md).

### Bug fixes

- Fixed regression from latest fix related to
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *brms* models.

- Fixed issue in
  [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  and
  [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  for models of class *cglm* with `"identity"` link, which was not
  correctly recognized due to a typo.

## insight 0.20.3

CRAN release: 2024-08-16

### Changes

- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  now supports more model classes.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  gives an informative error if no mixed model is provided.

### Bug fixes

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  and
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  for models from package *brms* with custom formulas.

- Fixed issues in
  [`find_response()`](https://easystats.github.io/insight/reference/find_response.md)
  for *brms* models with `mi()` function in the response variable.

- Fixed issue in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  that could lead to recursive calls for *brms* models, resulting in
  “infinite” resampling of the model.

- Fixed issue in
  [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  that erroneously tried to guess the minimum required package version
  based on the SUGGEST field of the *insight* package, instead of the
  package that was calling the function.

- Fixed issue in
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  for models from package *brms* with special functions in the formula
  (like `mo()`).

- Fixed issue in `ellipses_info()` when this function was called from
  [`do.call()`](https://rdrr.io/r/base/do.call.html).

- Fixed issue with formatting unicode-symbols, where a wrong
  unicode-character was used for “Omega”. Furthermore, Omega2 and Eta2
  are now correctly converted.

## insight 0.20.2

CRAN release: 2024-07-13

### New supported models

- Support for models of class `glmgee` (package *glmtoolbox*).

- Support for models of class `svy2lme` (package *svylme*).

### General

- Massive overhaul of
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md).
  The function should be now more accurate for different distributional
  families, in particular for mixed regression models with Beta family.

- Improved accuracy of singularity-checks in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md).

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  gets a few new arguments:

  - `null_model`, to provide a null-model to be used for the calculation
    of random effect variances. If `NULL`, the null-model is computed
    internally. This argument is optional, but may be useful to save
    time, or when the null-model cannot be calculated internally.

  - `approximation`, indicating the approximation method for the
    distribution-specific (observation level, or residual) variance.

  - `model_component`, for models that can have a zero-inflation
    component, specify for which component variances should be returned.
    By default, both the conditional and the zero-inflation component
    are taken into account.

- [`format_alert()`](https://easystats.github.io/insight/reference/format_message.md)
  and
  [`format_warning()`](https://easystats.github.io/insight/reference/format_message.md)
  get an `immediate` argument, to output warnings immediately.

- [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  and
  [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  now better cope with inverse transformations of the response value,
  such as `1/y`.

- [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  now returns more transformations for power-transformed response
  variables.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  for `MixMod` objects from package *GLMMadaptive* now recognize
  zero-inflation and hurdle models for custom families.

### Bug fixes

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  now correctly handles zero-inflated models from package *glmmTMB*.

- Fixed issue in
  [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  for models from package *GLMMadaptive*.

- Fixed issues in
  [`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.md)
  and
  [`link_function()`](https://easystats.github.io/insight/reference/link_function.md)
  for models of class `gamlss` from `LOGNO()` family.

## insight 0.20.1

CRAN release: 2024-06-11

### Bug fixes

- Fixed possible memory allocation issues when the deprecated argument
  `at` was used in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

## insight 0.20.0

CRAN release: 2024-06-04

### Breaking

- Arguments named `group`, `at`, `group_by` and `split_by` will be
  deprecated in future releases of *easystats* packages. Please use `by`
  instead. This affects following functions in *insight*:

  - [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  - [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  - [`print_parameters()`](https://easystats.github.io/insight/reference/print_parameters.md)

### Bug fixes

- Fixed errors in CRAN checks.

## insight 0.19.11

CRAN release: 2024-05-12

### General

- More informative error message for
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  when the requested `vcov`-function failed.

### Bug fixes

- Fixed issue with
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `coxme` models when `source` was set to `"modelframe"`.

## insight 0.19.10

CRAN release: 2024-03-22

### Bug fixes

- Functions like
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  or
  [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  now support multi-membership formulas for models from *brms*.

- Updated tests to work with the latest changes in *glmmTMB 1.1.9*.

## insight 0.19.9

CRAN release: 2024-03-15

### New supported models

- Support for models of class `serp` (package *serp*).

### General

- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  now also recognizes column `s.value` from objects of package
  *marginaleffects*.

### Bug fixes

- Fixed issue in
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  for models with splines (`s()`), where number of dimensions was
  indicated with a variable, not a number.

- [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  now works for factors and character vectors again.

- Fixed issues with latest release of *tinytable*.

- Fixed issues with latest release of *PROreg*.

## insight 0.19.8

CRAN release: 2024-01-31

### General

- Removed deprecated arguments in `get_data.mmrm()`.

- Improved support for models of class `rqs` (package *quantreg*).

- Revised test to address forthcoming changes in the *pscl* package.

### Bug fixes

- Fixed issue in
  [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  for glm-models with binary outcome, where levels were defined in
  reversed order.

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models of class `glmmPQL` (package *MASS*).

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models of class `gam` (package *mgcv*) for the `"gaulss"` family.

- Fixed issue in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *glmmTMB* models with `family = "ordbeta"`.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  with correctly detecting multivariate vgam/vglm models.

## insight 0.19.7

CRAN release: 2023-11-26

### General

- Support for objects of class `ggcomparisons` from
  `ggeffects::hypothesis_test()`.

- [`brms::gr()`](https://paulbuerkner.com/brms/reference/gr.html) is now
  supported, meaning that functions like
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  or
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  now also work for models with group-specific random effects.

- Fix CRAN check issues due to the last *fixest* update.

### Changes to functions

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  for models of class `pgmm` (package *plm*) now also supported robust
  variance-covariance matrices (i.e. argument `vcov`).

### Bug fixes

- Fixed issue in
  [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  for survival models with `strata()`, containing more that one
  variable.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md),
  where in some cases logistic regression models were erroneously
  considered as `"bernoulli"` models.

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models of class `gamlss` when the `random()` function was used
  with namespace in the formula (i.e. `... + gamlss::random()`).

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now detects models with zero-inflation part from package *glmmTMB*
  when models have truncated-families but no `ziformula`.

## insight 0.19.6

CRAN release: 2023-10-12

### General

- Improved documentation for
  [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md).

### Changes to functions

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now recognized ordered beta families.

- `find_formula` and `get_response` for `nestedLogit` models gain a
  `dichotomies` argument, to return values for the dichotomies used to
  fit the model.

### Bug fixes

- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  better detects power-transformation of the response variable.

- Corrected return value from `find_statistic` for
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  models.

- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  did not return the `"clean_parameters"` class attributes for some
  object. This caused issued in upstream packages.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md),
  which did not correctly detect “Bernoulli” property for some models
  classes (like `glmmTMB` or `glmerMod`).

## insight 0.19.5

CRAN release: 2023-09-13

### Bug fixes

- Fixed critical issue with
  [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  for old R releases.

## insight 0.19.4

CRAN release: 2023-09-10

### Changes to functions

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now accepts `predict = "link"` for gaussian models with log-link
  (i.e. `glm(..., family = gaussian("log"))`), to return predictions on
  the link scale.

- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  now automatically checks the package DESCRIPTION file to determine the
  correct minimum version required.

### Bug fixes

- Fixed issue with invalid multibyte strings in
  [`trim_ws()`](https://easystats.github.io/insight/reference/trim_ws.md).

- Fixed issue in
  [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  for models from package *fixest*.

## insight 0.19.3

CRAN release: 2023-06-29

### Breaking changes

- [`standardize_column_order()`](https://easystats.github.io/insight/reference/standardize_column_order.md)
  has changed the position when re-ordering Bayes factors, ROPEs and ESS
  / Rhat (mainly relevant for Bayesian models).

### Changes to functions

- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  and
  [`standardize_column_order()`](https://easystats.github.io/insight/reference/standardize_column_order.md)
  now also recognize the `"response.level"` column name.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for *lavaan* models is now more stable at retrieving model data when
  this is not available in the environment.

- [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  gets an `as_term_labels` argument, to extract model terms from the
  formula’s `"term.labels"` attribute. This is closer to the behaviour
  of [`stats::terms()`](https://rdrr.io/r/stats/terms.html), but may be
  insufficient, e.g. for mixed models.

### Bug fixes

- [`get_random()`](https://easystats.github.io/insight/reference/get_random.md)
  now returns the same observations as
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  and correctly removes missing values from the data before returning
  it.

- [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for marginal effects ignores the `"s.value"` column (which was added
  in a recent update).

- Fixed issue in
  [`get_response()`](https://easystats.github.io/insight/reference/get_response.md)
  for *brms* models with [`trunc()`](https://rdrr.io/r/base/Round.html)
  function in the response variable.

## insight 0.19.2

CRAN release: 2023-05-23

### Breaking changes

- The minimum needed R version has been bumped to `3.6`.

- [`download_model()`](https://easystats.github.io/insight/reference/download_model.md)
  no longer errors when a model object could not be downloaded, but
  instead returns `NULL`. This prevents test failures, and allows to
  skip tests when the return value of
  [`download_model()`](https://easystats.github.io/insight/reference/download_model.md)
  is `NULL`.

### General

- Improved support for `mclogit` models (package *mclogit*) and `mipo`
  objects (package *mice*) for models with ordinal or categorical
  response.

### New supported models

- `phylolm` and `phyloglm` (package *phylolm*), `nestedLogit` (package
  *nestedLogit*).

### Bug fixes

- Fixed issue in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *glmmTMB* models with rank deficient coefficients.

- Fixed issues in
  [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md)
  for `glm` models without weights and `na.action` not set to default in
  the model call.

- [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  now also removes the
  [`relevel()`](https://rdrr.io/r/stats/relevel.html) pattern.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  for models of class `gamlss`.

- Fixed problems preventing
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  from locating data defined in non-global environments.

- Fixed issue in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for variables of class numeric matrix created by
  [`scale()`](https://rdrr.io/r/base/scale.html), which were correctly
  handled only when
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  failed to find the data in the appropriate environment.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  for `gee` models from `binomial` families.

## insight 0.19.1

CRAN release: 2023-03-18

### New supported models

- `hglm` (package *hglm*).

### Changes to functions

- Minor improvements to
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for [`t.test()`](https://rdrr.io/r/stats/t.test.html).

- [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  gets a `lead_zero` argument, to keep or drop the leading zero of a
  formatted value, as well as arguments `style_positive` and
  `style_negative` to style positive or negative numbers.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  now also formats columns named `SGPV` (second generation p-values) as
  p-values.

- Functions for models of class `clm` (like
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md),
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  etc.) now also include variables that were defined as `scale` or
  `nominal` component.

### Bug fixes

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for results from
  [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html).

- Fixed issue in
  [`find_weights()`](https://easystats.github.io/insight/reference/find_weights.md)
  for models of class `lme` and `gls`.

- Fixed issue in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  for models with multiple weight variables.

## insight 0.19.0

CRAN release: 2023-01-30

### New supported models

- `mmrm` (package *mmrm*), `flac` and `flic` (*logistf*)

### Breaking changes

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  was revised and now always tries to recover the data that was used to
  fit a model from the environment. If this fails, it falls back to
  recovering data from the model frame (the former default behaviour).
  Furthermore, the `source` argument can be used to explicitly force the
  old behaviour: `source = "mf"` will try to recover data from the model
  frame first, then possibly falling back to look in the environment.

### New functions

- [`n_grouplevels()`](https://easystats.github.io/insight/reference/n_grouplevels.md),
  to return random effect groups and number of group levels for mixed
  models.

### Changes to functions

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  preserves all factor levels for factors that are hold constant at
  their reference level. This is required to work together with
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  when calculating standard errors for
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md).

### Bug fixes

- Fixed but in
  [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  handling of incomplete factors which sometimes had downstream
  implications for numerical results in the uncertainty estimates
  produced by
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md).

- Fixed minor issues for HTML tables in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  when model parameters were grouped.

- Fixed issue with incorrect back-transforming in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for models with log-transformed variables.

- Fixes issue in
  [`compact_list()`](https://easystats.github.io/insight/reference/compact_list.md).

- [`has_single_value()`](https://easystats.github.io/insight/reference/trim_ws.md)
  now returns `FALSE` when the object only has `NA` and `na.rm = TRUE`.

- Fixed issue in
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for gam-models without smooth terms, or with only smooth terms and
  removed intercept.

## insight 0.18.8

CRAN release: 2022-11-24

### Bug fixes

- Fixed test due to changes in the *performance* package.

## insight 0.18.7

CRAN release: 2022-11-18

### General

- Minor revisions to `get_predicted.glmmTMB()` due to changes in
  behaviour of `predict.glmmTMB()` for truncated-family models since
  *glmmTMB* 1.1.5.

- New function
  [`has_single_value()`](https://easystats.github.io/insight/reference/trim_ws.md)
  that is equivalent to `length(unique()) == 1` (or `n_unique() == 1`)
  but faster.

### Changes to functions

- `ellipses_info()` now includes an attribute `$is_binomial`, which is
  `TRUE` for each model from binomial family.

### Bug fixes

- Fixed behaviour of the `at` argument in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

- Fixed issue for accessing model data in
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  for some edge cases.

## insight 0.18.6

CRAN release: 2022-10-23

### New supported models

- Support the *logitr* package:
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md),
  [`find_variables()`](https://easystats.github.io/insight/reference/find_variables.md)
  and more.

### Bug fixes

- Better detection of unicode-support, to avoid failures when building
  vignettes.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now correctly handles variables of class numeric matrix created by
  [`scale()`](https://rdrr.io/r/base/scale.html), which fixes a bug in
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  (easystats/performance#432).

- Fixed issue with `iterations` argument in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  with *brms* models.

## insight 0.18.5

CRAN release: 2022-10-12

### Breaking

- `get_df(type = "satterthwaite")` for `lmerMod` objects now return
  degrees of freedom per parameter, and no longer per observation. Use
  `df_per_obs TRUE` to return degrees of freedom per observation.

### New functions

- [`safe_deparse_symbol()`](https://easystats.github.io/insight/reference/trim_ws.md)
  to only deparses a substituted expressions when possible,which
  increases performance in case many calls to `deparse(substitute())`.

### Changes to functions

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  gets a `use_symbols` argument. If `TRUE`, column names that refer to
  particular effectsizes (like Phi, Omega or Epsilon) include the
  related unicode-character instead of the written name. This only works
  on Windows for R \>= 4.2, and on OS X or Linux for R \>= 4.0.

- The `stars` argument in
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  can now also be a character vector, naming the columns that should
  include stars for significant values. This is especially useful for
  Bayesian models, where we might have multiple columns with significant
  values, e.g. `"BF"` for the Bayes factor or `"pd"` for the probability
  of direction.

- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  gets more `type` options to return different type of degrees of
  freedom (namely, `"wald"` and `"normal"`, and for mixed models,
  `"ml1"`, `"betwithin"`, `"satterthwaite"` and `"kenward-roger"`).

- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  now recognized more classes from package *marginaleffects*.

- Minor improvements to
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for models with nonlinear formula.

- Minor speed improvements.

### Bug fixes

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for models of class `plm`, which accidentally converted factors into
  character vectors.

- Fixed issue with column alignment in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  when the data frame to print contained unicode-characters longer than
  1 byte.

- Correctly extract predictors for `fixest::i(f1, i.f2)` interactions
  ([\#649](https://github.com/easystats/insight/issues/649) by
  [@grantmcdermott](https://github.com/grantmcdermott)).

## insight 0.18.4

CRAN release: 2022-09-20

### Changes to functions

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now includes information for `htest` objects from
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) and
  [`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html) (will
  return `$is_variancetest = TRUE`).

### Bug fixes

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  which did not correctly backtransform to original data when terms had
  log-transformations such as `log(1 + x)` or `log(x + 1)`.

- Fixed CRAN check issues.

## insight 0.18.3

CRAN release: 2022-09-18

### New functions

- [`format_alert()`](https://easystats.github.io/insight/reference/format_message.md),
  [`format_warning()`](https://easystats.github.io/insight/reference/format_message.md)
  and
  [`format_error()`](https://easystats.github.io/insight/reference/format_message.md),
  as convenient wrappers around
  [`message()`](https://rdrr.io/r/base/message.html),
  [`warning()`](https://rdrr.io/r/base/warning.html) or
  [`stop()`](https://rdrr.io/r/base/stop.html) in combination with
  [`format_message()`](https://easystats.github.io/insight/reference/format_message.md).
  You can use these funcionts to format messages, warnings or errors.

### Changes to functions

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for models of class `clm` now includes confidence intervals of
  predictions.

- [`format_message()`](https://easystats.github.io/insight/reference/format_message.md)
  gets some additional formatting features. See ‘Details’ in
  [`?format_message`](https://easystats.github.io/insight/reference/format_message.md)
  for more information and some current limitations.

- [`format_message()`](https://easystats.github.io/insight/reference/format_message.md)
  gets an `indent` argument, to specify indention string for subsequent
  lines.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  now merges IC and IC weights columns into one column (e.g., former
  columns `"AIC"` and `"AIC_wt"` will now be printed as one column,
  named `"AIC (weights)"`). Furthermore, an `ic_digits` argument was
  added to control the number of significant digits for the IC values.

- [`print_color()`](https://easystats.github.io/insight/reference/print_color.md)
  and
  [`color_text()`](https://easystats.github.io/insight/reference/print_color.md)
  now support bright variants of colors and background colors.

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  gets more options for `at` and `range`, to provide more control how to
  generate the reference grid.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for models of class `geeglm` and `fixest`now more reliably retrieves
  the model data.

### New supported models

- Support for models of class `mblogit` and `mclogit`.

### Bug fixes

- Fixed issues with wrong attribute `adjusted_for` in
  [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md).

- Fixed issue (resp. implemented workaround) in `get_data.iv_robust()`,
  which failed due to a bug in the *estimatr* package.

- Fixed issue where
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  failed when data contains factors with only one or incomplete levels.

- Fixed issue in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for models of class `mlm`.

- Fixed issue where
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  failed to compute confidence intervals of predictions when model
  contained matrix-alike response columns, e.g. a response variable
  created with [`cbind()`](https://rdrr.io/r/base/cbind.html).

## insight 0.18.2

CRAN release: 2022-08-10

### New functions

- [`format_percent()`](https://easystats.github.io/insight/reference/format_value.md)
  as short-cut for `format_value(as_percent = TRUE)`.

- [`is_converged()`](https://easystats.github.io/insight/reference/is_converged.md),
  to check whether a mixed model has converged or not.

### Changes to functions

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  gains an `exact` argument, to either report exact or rounded Bayes
  factors.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  gets a method for models of class `gamlss` (and thereby,
  [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  now also works for those model classes).

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now better handles models of class `polr`, `multinom` and `rlm`.

### Bug fixes

- Fixed test failures.

- Minor fixes to address changes in other packages.

## insight 0.18.0

CRAN release: 2022-07-05

### Breaking changes

- The `ci` argument in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now defaults to `NULL`. One reason was to make the function faster if
  confidence intervals are not required, which was the case for many
  downstream usages of that function. Please set `ci` explicitly to
  compute confidence intervals for predictions.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  no longer returns logical types for numeric variables that have been
  converted to logicals on-the-fly within formulas (like
  `y ~ as.logical(x)`). Instead, for each numeric variable that was
  coerced to logical within a formula gets a `logical` attribute (set to
  `TRUE`), and the returned data frame gets a `logicals` attribute
  including all names of affected variables.

- `parameters_table()`, the alias for
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md),
  was removed.

### Changes to functions

- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  and
  [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  now also work for models where the response was transformed using
  [`log2()`](https://rdrr.io/r/base/Log.html) or
  [`log10()`](https://rdrr.io/r/base/Log.html).

### Bug fixes

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  for models from package *VGAM* returned wrong sigma-parameter.

- [`find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.md)
  for models from package *fixest* that contained interaction terms in
  the endogenous formula part did not correctly return all instruments.

- Fixed formatting of HTML table footers in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).

- Several fixes to
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  for models from
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- The `component` argument in
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for `stanmvreg` models did not accept the `"location"` value.

- [`null_model()`](https://easystats.github.io/insight/reference/null_model.md)
  did not consider offset-terms if these were specified inside formulas.

- Argument `allow.new.levels` was not passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html) for
  `get_predicted.glmmTMB()`.

- [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  now works correctly when several variables are specified in `s()`
  ([\#573](https://github.com/easystats/insight/issues/573),
  [@etiennebacher](https://github.com/etiennebacher)).

## insight 0.17.1

CRAN release: 2022-05-13

### New supported model classes

- `deltaMethod` (*car*), `marginaleffects`, `marginaleffects.summary`
  (*marginaleffects*)

### General

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now supports models of class `iv_robust` and `ivreg`.

- For
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md),
  when both `type` and `predict` are given, `type` will overwrite
  `predict`. Note that this will print a message, because `predict` is
  the preferred argument.

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  gains `vcov` and `vcov_args` arguments, to specify the
  variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors).

- `get_loglikehood()` improved handling of models from package
  *estimator*.

### Bug fixes

- Fixed bug in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for model objects whose data needs to be recovered from the
  environment, and where the data name was a reserved word (e.g., named
  like an R function).

- The matrix returned by
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  for models of class *bife* now returns row and column names.

- [`find_offset()`](https://easystats.github.io/insight/reference/find_offset.md)
  did not find offset-terms for `merMod` objects when the offset was
  specified as `offset` argument in the function call.

## insight 0.17.0

CRAN release: 2022-03-29

### Breaking changes

- Arguments `vcov_estimation` and `vcov_type` in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md),
  `get_predicted_se()` and
  [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
  are replaced by `vcov` and `vcov_args`, to have a more simplified and
  common interface to control robust covariance matrix estimation.

### General

- Improved performance for various functions, in particular
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  and
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md).

### New functions

- To check for names:
  [`object_has_names()`](https://easystats.github.io/insight/reference/object_has_names.md)
  and
  [`object_has_rownames()`](https://easystats.github.io/insight/reference/object_has_names.md)

- To work with lists:
  [`is_empty_object()`](https://easystats.github.io/insight/reference/is_empty_object.md)
  and
  [`compact_list()`](https://easystats.github.io/insight/reference/compact_list.md)

- To work with strings:
  [`compact_character()`](https://easystats.github.io/insight/reference/compact_character.md)

- Further utility functions are
  [`safe_deparse()`](https://easystats.github.io/insight/reference/trim_ws.md),
  [`trim_ws()`](https://easystats.github.io/insight/reference/trim_ws.md)
  and
  [`n_unique()`](https://easystats.github.io/insight/reference/trim_ws.md).

### Changes to functions

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  now better checks for invalid values of caption and footer for tables
  in HTML format, and silently removes, e.g., ansi-colour codes that
  only work for text-format.

- `get_data.coxph()` returns the original data frame instead of data
  with type coercion.

- [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  gets a `check_response` argument, to check if a model has a
  transformed response variable (like
  [`log()`](https://rdrr.io/r/base/Log.html) or
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html) transformation), and
  if so, returns a corrected log-likelihood.

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  now supports *BayesFactor* models.

- [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  and
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  now support more model classes.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  was improved for multinomial models from *brms*.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  was improved to cover more edge cases of (more complex) random effect
  structures.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now includes variables in the returned data frame that were used in
  the `subset` argument of regression functions (like
  [`lm()`](https://rdrr.io/r/stats/lm.html)).

- In some edge cases, where
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  is unable to retrieve the data that was used to fit the model, now a
  more informative error is printed.

- `ellipses_info()` now also accepts a list of model objects, is more
  stable and returns more information about the provided models (like if
  all fixed or random effects are the same across models, if all models
  are mixed models or null-models, etc.)

- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  now works interactively and lets the user prompt whether to
  automatically update or install packages.

### Bug fixes

- Fixed incorrect column name conversion in
  [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  for certain columns returned by
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html).

- Fixed issue with correctly detecting Tweedie-models in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md).

- Fixed issue with
  [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md)
  for *brms* models with monotonic factors.

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  when argument `correlation` was defined outside of `lme()` and `gls()`
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#525](https://github.com/easystats/insight/issues/525)).

- Fixed issue with
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  when back-transforming data from predictors that used
  [`cos()`](https://rdrr.io/r/base/Trig.html),
  [`sin()`](https://rdrr.io/r/base/Trig.html) or
  [`tan()`](https://rdrr.io/r/base/Trig.html) transformations.

## insight 0.16.0

CRAN release: 2022-02-16

### New functions

- [`get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.md),
  to generate a reference grid, usually used when computing adjusted
  predictions or marginal means from regression models.

### Changes to functions

#### `get_predicted()`

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  was revised. Beside the four core options for the `predict` argument,
  it is now also possible to use any value that is valid for the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) method’s `type`
  argument.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  now supports more models (e.g., from packages like *GLMMadaptive* or
  *survival*).

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  is now more robust when calculating standard errors of predictions.

#### Other functions

- [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  and
  [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  now support *htest* objects.

### General

- Various minor improvements.

## insight 0.15.1

### General

- Improved speed performance, especially for
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md).

### Changes to functions

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `coxph` models now returns the original factor levels for
  variables transformed with `strata()` inside formulas.

## insight 0.15.0

CRAN release: 2022-01-07

### Breaking changes

- Data management functions (like
  [`reshape_longer()`](https://easystats.github.io/datawizard/reference/data_to_long.html),
  or
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.html))
  have been moved to the *datawizard* package.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  no longer returns factor types for numeric variables that have been
  converted to factors on-the-fly within formulas (like
  `y ~ as.factor(x)`). Instead, for each numeric variable that was
  coerced to factor within a formula gets a `factor` attribute (set to
  `TRUE`), and the returned data frame gets a `factors` attribute
  including all names of affected variables.

### New supported model classes

- Support for `bfsl` (*bfsl*)

### New functions

- New
  [`standardize_column_order()`](https://easystats.github.io/insight/reference/standardize_column_order.md)
  function can be used to standardize the column order in output
  dataframes.

### General

- Improved speed performance for some functions.

- Improved handling of table captions and footers in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).
  See also the new vignette on exporting data frames into human readable
  tables here:
  <https://easystats.github.io/insight/articles/export.html>

- Revised `width` argument in
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md),
  which now allows to set different column widths across table columns.
  See examples in
  [`?export_table`](https://easystats.github.io/insight/reference/export_table.md).

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gets a `table_width` argument to split wide tables into two parts.

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  for `MixMod` (package *GLMMadaptive*) was revised, and now allows to
  return a robust variance-covariance matrix.

- Added more
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  methods.

### Bug fixes

- Fixed issues with manual sigma computation to handle dispersion models
  in
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md).

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for
  [`BayesFactor::lmBF()`](https://rdrr.io/pkg/BayesFactor/man/lmBF.html)
  with multiple random effects.

- Fixed issue in
  [`get_parameters.BFBayesFactor()`](https://easystats.github.io/insight/reference/get_parameters.BGGM.md)
  with wrong sign of difference estimate for t-tests.

- Argument `width` in
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  was ignored when formatting integer values and `protect_integers` was
  set to `TRUE`.

## insight 0.14.5

CRAN release: 2021-10-16

### New functions

- [`find_transformation()`](https://easystats.github.io/insight/reference/find_transformation.md)
  and
  [`get_transformation()`](https://easystats.github.io/insight/reference/get_transformation.md)
  to find or get any function that was used to transform the response
  variable in a regression model.

### General

- Improved support for models of class `sampleSelection`.

- Improved documentation.

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  now supports: [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html)

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  supports: [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html),
  [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html), `fixest`,
  `bife::bife`,
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html).

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  standard errors are often much faster to compute.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  supports models with “grouped” or “level” outcomes (e.g., multinomial
  logit).

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  handles factors better.

- Improved documentation

### Changes to functions

- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  gains a `quietly` argument, if neither stopping nor a warning message
  for non-installed packages is requested.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)’s
  `predict` argument now accepts these values: “link”, “expectation”,
  “prediction”, “classification”, or NULL.

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  accepts `predict=NULL`, which allows users to push a `type` argument
  through the `...` ellipsis, forward to the
  [`predict()`](https://rdrr.io/r/stats/predict.html) method of the
  modelling package.

### Bug fixes

- Fixed issue with parameter names from *emmeans* objects in
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md).

- Fixed issues with unknown arguments in
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md).

## insight 0.14.4

CRAN release: 2021-09-02

### Bug fixes

- Fixed issues due to latest *brms* update.

## insight 0.14.3

CRAN release: 2021-08-17

### New supported model classes

- `systemfit` (*systemfit*)

### General

- Minor improvements for functions that support printing outputs.

### Changes to functions

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  gains a new option, `predict = "response"` for binomial models.

- Improved stability of
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  when computing random-slope-intercept correlation with categorical
  random slopes.

- Improved
  [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  for *brms* models.

### Bug fixes

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for *brms* models with auxiliary parameters.

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for *brms* models with auxiliary parameters.

- Fixed issue where
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for *htest* objects did not always preserve factors.

- Fixed issue in
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  for ci-levels with longer fractional part.

## insight 0.14.2

CRAN release: 2021-06-22

### Changes to functions

- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  gains a `minimum_version` argument, to check if an installed package
  is not older than the specified version number.

- The `package` argument in
  [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  is now vectorized, so you can check for multiple packages in one
  function call.

- Value formatting functions (like
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  or
  [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md))
  can now round to significant digits using `digits = "signif"`.

### Bug fixes

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  with
  [`stan_polr()`](https://mc-stan.org/rstanarm/reference/stan_polr.html)
  models.

- Fixed issue in
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for *brms* when model contained parameters for the priors on sigma.

- Fixed issue in
  [`n_obs()`](https://easystats.github.io/insight/reference/n_obs.md)
  for [`stats4::mle()`](https://rdrr.io/r/stats4/mle.html) models.

- Fixed failing tests due to latest *fixest* update.

- Fixed issues due to latest *epiR* update.

## insight 0.14.1

CRAN release: 2021-05-28

### New functions

- Added several data management and preparation functions:
  [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.html),
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.html),
  [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.html),
  [`data_restoretype()`](https://easystats.github.io/datawizard/reference/data_restoretype.html),
  `force_numeric()`.

### New supported model classes

- Support for `pgmm` (*plm*)

### Changes to functions

- Improved handling of auxiliary parameters for *stanreg* models.

### Bug fixes

- Stability improvements to
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md).

- Fixed issues when accessing information from an `afex_aov` model with
  an empty `aov` slot (in anticipation for
  [afex](https://afex.singmann.science/) v.1.0.0).

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  for *stanreg* object with non-standard model-family specification.

## insight 0.14.0

CRAN release: 2021-05-07

### General

- Better support for accessing auxiliary parameters (via
  [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  and
  [`get_auxiliary()`](https://easystats.github.io/insight/reference/get_auxiliary.md),
  as well as `get_parameters(component = "all")`) for `brmsfit` models.

### New functions

- [`get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.md)
  as a robust alternative to
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) for
  different model classes.

- [`format_message()`](https://easystats.github.io/insight/reference/format_message.md)
  to format warnings and messages by adjusting the maximum line_length,
  possibly to the width of the console window.

- [`format_string()`](https://easystats.github.io/insight/reference/format_string.md)
  to shorten a string to a maximum length.

- [`check_if_installed()`](https://easystats.github.io/insight/reference/check_if_installed.md)
  to see if the needed package is installed.

### New supported model classes

- Support for `mvord` (*mvord*), `SemiParBIV` (*GJRM*), `selection`
  (*sampleSelection*)

### Changes to functions

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  now warns when data name is present in formula, since this can result
  in unexpected behaviour in other package functions.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  returns `is_bernoulli = TRUE` for Bernoulli models.

- Add
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  for *lavaan* models.

- [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  supports more models/objects.

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  supports more models/objects.

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  and
  [`get_deviance()`](https://easystats.github.io/insight/reference/get_deviance.md)
  for `lrm` models (package *rms*) now only return one value, sigma or
  deviance for the model with intercept and predictors.

- [`get_deviance()`](https://easystats.github.io/insight/reference/get_deviance.md)
  now works for `glmerMod`, `MixMod` and `glmmTMB` models.

- The behaviour and documentation of the `effects` and `component`
  arguments, in particular for `brmsfit` models, were revised to be more
  consistent.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  now correctly prints the footer if the input was a list of data
  frames.

### Bug fixes

- Fixed issue (warning) in
  [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  for binomial models with non-numeric response variables.

- [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  correctly distinguishes t- and z-statistic for *emmGrid* objects.

- Fixed issue in
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  for `BGGM` and [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for `gamlss` models with `random()` function in formula.

- Fixed issue with
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for *brmsfit* models when auxiliary parameters are directly modelled.

- Fixed issue with
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  and
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for multi-group *blavaan* models.

- Fixed issue in
  [`ellipsis_info()`](https://easystats.github.io/insight/reference/ellipsis_info.md)
  when detecting nested models with poly-terms.

- Fixed issue in
  [`find_response()`](https://easystats.github.io/insight/reference/find_response.md)
  for *brmsfit* models that used the `resp_thres()` function in the
  model formula.

- Fixed issue in
  [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
  for models with rank-deficient model matrix.

- Argument `zap_small` in
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  did not work properly over vectorized vectors.

## insight 0.13.2

CRAN release: 2021-04-01

### General

- [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  has be revamped with a new API and a stable output form (a vector). In
  the course of this revision, a new function
  [`get_predicted_ci()`](https://easystats.github.io/insight/reference/get_predicted_ci.md)
  to calculate uncertainty intervals for model predictions.

- Improved support for `orm` (*rms*).

### New supported model classes

- Support for `svy_vglm` (*svyVGAM*), `mjoint` (*joineRML*), `mhurdle`
  (*mhurdle*), `sarlm` (*spatialreg*), `model_fit` (*tidymodels*)

### New functions

- [`is_gam_model()`](https://easystats.github.io/insight/reference/is_gam_model.md)
  as a small helper to check if a model is a generalized additive model
  with smooth terms.

### Changes to functions

- Added `iterations` argument to
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  to control the number of draws returned for Bayesian models.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now returns `$is_gam` if model is generalized additive model with
  smooth terms.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  and
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  now check for valid input (e.g., non-empty data frame) and give an
  informative message.

- Improved support for `MixMod` (*GLMMadaptive*) in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md).

- Improved
  [`print_parameters()`](https://easystats.github.io/insight/reference/print_parameters.md),
  to allow more flexibility and better cope with different output
  formats.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md),
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  and
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  for *emmGrid* and *emm_list* objects were revised and now better match
  the actual parameter names (also for contrasts).

### Bug fixes

- Fixed issue in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for models without intercept.

- Fixed labelling issue in
  [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  and
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  for `blavaan` models.

- [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md)
  for *MCMCglmm* objects did not include random parameters.

- Fixed minor issue with unintended sub-titles for
  [`print_html()`](https://easystats.github.io/insight/reference/display.md).

- Fixed issue in `get_prior()` for
  [`rstanarm::R2()`](https://mc-stan.org/rstanarm/reference/priors.html)
  priors.

## insight 0.13.1

CRAN release: 2021-02-22

### General

- Improved handling for GAMs.

### New supported model classes

- Support for `elm`, `eglm` (*eflm*)

### Changes to functions

- `get_residuals(..., weighted = TRUE)` doesn’t throw warnings if
  weights are 1 (no weights specified).

- [`n_parameters()`](https://easystats.github.io/insight/reference/n_parameters.md)
  gains a `only_estimable` argument, to remove non-estimable parameters
  from counting the number of parameters for models with rank-deficient
  model matrix.

- [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  also gains a `zap_small` argument.

### Bug fixed

- Fix or disable failing tests on Mac OS.

- Fixed issues in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  with non-correlated random-slope-intercepts for *lme4* models.

## insight 0.13.0

CRAN release: 2021-02-15

### General

- Roll-back R dependency to R \>= 3.4.

### New supported model classes

- Support for `crr` (*cmprsk*), `ergm` (*ergm*), `btergm` (*btergm*),
  `Rchoice` (*Rchoice*), `garch` (*tseries*)

### Changes to functions

- Slightly improved handling of different CI-columns in
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md).

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now returns `$is_leventest` if model is an object returned by
  [`car::leveneTest()`](https://rdrr.io/pkg/car/man/leveneTest.html).

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  supports `htest` objects.

### Bug fixes

- [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  did not properly remove `NA` from rank-deficient models.

- Fixed issue/warning in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for some *htest* objects, where the environment was not properly
  evaluated.

- Fixed issue in
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  with p-value formatting, when input data frame contained a column
  named `"p"`, which was not numeric.

- (Hopefully) fixed issue with failing CRAN checks.

## insight 0.12.0

CRAN release: 2021-01-14

### Breaking changes

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  is an alias for `parameters_table()`, and is no longer referring to
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).

### New supported model classes

- Support for `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivFixed`
  (*ivFixed*), `ivprobit` (*ivprobit*), `riskRegression`
  (*riskRegression*). `summary.lm`, `lmodel2` (*lmodel2*), improved
  support for `bamlss` (*bamlss*).

### New functions

- Added
  [`get_deviance()`](https://easystats.github.io/insight/reference/get_deviance.md)
  function that returns the model deviance as a robust alternative to
  [`stats::deviance()`](https://rdrr.io/r/stats/deviance.html).

- Added
  [`model_name()`](https://easystats.github.io/insight/reference/model_name.md)
  function that returns the model’s “name”.

- Added [`format()`](https://rdrr.io/r/base/format.html) method for
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  output to flatten it.

- Added `null_as_ones = TRUE` argument to
  [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md)
  to return vector of 1s instead of `NULL`.

- Added
  [`get_intercept()`](https://easystats.github.io/insight/reference/get_intercept.md)
  as a helper function to easily retrieve the value at the intercept.

- Added
  [`get_df()`](https://easystats.github.io/insight/reference/get_df.md)
  as a robust alternative to `stats::df.residuals()`.

- Added
  [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.md)
  as a robust alternative to
  [`stats::fitted()`](https://rdrr.io/r/stats/fitted.values.html).

- Added
  [`get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md)
  (and its alias
  [`loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.md))
  function as a robust alternative to
  [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html).

- Added
  [`get_residuals()`](https://easystats.github.io/insight/reference/get_residuals.md)
  as a robust alternative extract model residuals.

- Added
  [`ellipsis_info()`](https://easystats.github.io/insight/reference/ellipsis_info.md)
  to specify the nature of ellipsis (`...`) inputs.

- Added
  [`is_nested_models()`](https://easystats.github.io/insight/reference/is_nested_models.md)
  to check if multiple regression models are nested (decreasing or
  increasing).

- Added generic
  [`print_html()`](https://easystats.github.io/insight/reference/display.md),
  to allow other packages to create tables in HTML format (via
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md))
  when not printing the output to console.

- Added
  [`is_mixed_model()`](https://easystats.github.io/insight/reference/is_mixed_model.md),
  to safely check if a model is a mixed effects model. This function
  also works for multivariate response models.

- [`n_parameters()`](https://easystats.github.io/insight/reference/n_parameters.md)
  was moved from *parameters* to *insight*.

### Changes to functions

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md),
  `find_predictor()`,
  [`find_random()`](https://easystats.github.io/insight/reference/find_random.md)
  and related functions now also return names of random effects from
  generalized additive mixed models (`gamm`, `gamm4`, `stan_gamm4`).

- Added support for more BFBayesFactor objects.

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now returns `$is_xtab` for
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) and
  [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html).
  Furthermore, the `$family` element for those objects is set to
  `"categorical"`.

- [`n_obs()`](https://easystats.github.io/insight/reference/n_obs.md)
  now handles number of observations from models with binomial family
  correctly when these have matrix-columns as response variable.

### Bug fixes

- Fixed issue in
  [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  for *fixest* models, which did not return the correct value
  `"t-statistic"` for `feols()`.

- Fixes inconsistencies in
  [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  for (linear) `BFBayesFactor` models.

## insight 0.11.1

CRAN release: 2020-12-08

### General

- Warnings that formerly were printed using
  [`print_color()`](https://easystats.github.io/insight/reference/print_color.md)
  now use [`warning()`](https://rdrr.io/r/base/warning.html), to better
  suppress warning messages if required.

### New functions

- [`find_smooth()`](https://easystats.github.io/insight/reference/find_smooth.md),
  to return in particular smooth terms used in a model.

### Changes to functions

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  and
  [`get_variance_random()`](https://easystats.github.io/insight/reference/get_variance.md)
  gain a `tolerance`-argument, to set the tolerance level for
  singularity checks when computing random effect variances.

- `parameters_table()` formats more objects from the *easystats*
  packages, like ROPE-range or
  [`p_rope()`](https://easystats.github.io/bayestestR/reference/rope.html).

- [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  now supports models of class *scam*.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now also supports `htest`-object, where possible.

### Bug fixes

- Fix CRAN check issues.

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for
  [`stan_gamm4()`](https://mc-stan.org/rstanarm/reference/stan_gamm4.html)
  now correctly includes random effects.

## insight 0.11.0

CRAN release: 2020-11-24

### Breaking changes

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  now also detects models from
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html),
  [`binom.test()`](https://rdrr.io/r/stats/binom.test.html)
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
  [`mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html) and
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html). Furthermore,
  [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  better deals with objects from `BFBayesFactor`, and censored
  regression models no longer return `TRUE` for `$is_linear`.

- [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  is going to be renamed in a future update. Please use its alias
  [`export_table()`](https://easystats.github.io/insight/reference/export_table.md).

### New supported model classes

- Support for `scam` (*scam*), `meta_random` and `meta_fixed`
  (*metaBMA*), `Glm` (*rms*), `ridgelm` (*MASS*), `mediate`
  (*mediation*). Partial support for `mcmc.list` (e.g. *bayesGARCH*)

### New function

- `parameters_table()`, which was moved from package *parameters* to
  *insight*. Note that this function is going to be renamed into
  [`format_table()`](https://easystats.github.io/insight/reference/format_table.md)
  in a future update.

- [`find_offset()`](https://easystats.github.io/insight/reference/find_offset.md),
  to find the name of offset-terms.

- Added generics for
  [`display()`](https://easystats.github.io/insight/reference/display.md)
  and
  [`print_md()`](https://easystats.github.io/insight/reference/display.md),
  to allow other packages to create tables in other formats when not
  printing the output to console.

### Changes to functions

- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md)
  tries to be as loyal to the *broom*-naming conventions as possible.

- The function of the `brackets`-argument in
  [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  was changed. It is now also possible to provide a length-two character
  vector, to define own brackets that encompass the CI-values.

- Related to the change in
  [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md),
  the function of the `brackets`-argument in `parameters_table()` was
  changed accordingly. Furthermore, `parameters_table()` gains a
  `preserve_attributes`-argument, to preserve any attributes from the
  input data frame.

- [`export_table()`](https://easystats.github.io/insight/reference/export_table.md)
  gains several new arguments that allows to create tables in
  markdown-format.

- [`print_parameters()`](https://easystats.github.io/insight/reference/print_parameters.md)
  gains a `keep_parameter_column`-argument, to keep (default) both the
  `"Cleaned_Parameter"` and `"Parameter"` columns, or - if `FALSE` - use
  `"Cleaned_Parameter"` as new `"Parameter"` column.

### Bug fixes

#### `get_data()`

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `MixMod` objects, which were caused due to internal changes in
  *GLMMadaptive*.

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for zero-inflated models from *pscl* did not include the offset-term
  in cases where the offset was defined as argument, not inside the
  model formula.

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for `coxph` models with survival-objects with `event`-argument as
  response.

- Fixed edge case in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for column name of response values that were log-transformed using
  `log(x+1)`.

#### Other bug fixes

- Fixed issue with `survreg` models that included `strata()` in their
  formula.

- Fixed warning in CRAN checks for forthcoming R-devel.

## insight 0.10.0

CRAN release: 2020-10-22

### New function

- [`get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.md)
  to return the residual standard deviation.

- [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.md),
  which was moved from package *parameters* to *insight*.

### New supported model classes

- Support for `maov` (*stats*), `HLfit` (*spaMM*), preliminary support
  for `margins` (*margins*), `merModList` (*merTools*).

### General

- Better support for (weighted) multivariate response models of class
  `mlm` for functions like
  [`get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.md)
  or
  [`clean_parameters()`](https://easystats.github.io/insight/reference/clean_parameters.md).

- Make
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  work with t-tests from *BayesFactor*.

- Improved handling for *mira* objects.

### Changes to functions

- [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md)
  gains a `na_reference` argument, to set the “reference” for Bayes
  factor values that are `NA`, and an `exact` argument for returning
  scientific formatted extreme values.

- [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  gains a `zap_small` argument, to prevent scientific printing of
  numbers if these have more decimal places than indicated by `digits`.

- [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md)
  now also returns `NULL` when all weights were 1.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *BFBayesFactor* objects gets a `verbose` argument.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for *emmGrid* and *emm_list* objects gets a `summary` argument, to
  either return the full posterior samples or the summarized centrality
  indices for Bayesian models.

- [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for
  [`MuMIn::model.avg()`](https://rdrr.io/pkg/MuMIn/man/model.avg.html)
  now tries to retrieve the random effects part of a formula, when
  present.

- [`get_weights()`](https://easystats.github.io/insight/reference/get_weights.md)
  gains a `na_rm` argument to remove possible missing values.

### Bug fixes

- Fix issues with one-sample Bayesian t-tests (
  <https://github.com/easystats/parameters/issues/297> ).

- Fix issue in
  [`format_value()`](https://easystats.github.io/insight/reference/format_value.md)
  that printed `"100%"` as `"1e+02%"`.

- Removed unnecessary white-spaces in
  [`format_ci()`](https://easystats.github.io/insight/reference/format_ci.md)
  when upper or lower interval was larger than 1e+5.

- [`has_intercept()`](https://easystats.github.io/insight/reference/has_intercept.md)
  did not work correctly when intercept was removed from formula using
  `-1`.

- [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  now shows removal of intercept formula using `-1` as term `"-1"`.

- Fix issues with
  [`get_statistic()`](https://easystats.github.io/insight/reference/get_statistic.md)
  for *vgam* models.

## insight 0.9.6

CRAN release: 2020-09-20

### Changes to functions

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now works for models from `afex_aov()`.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  returns a more informative message for `BFBayesFactor` objects when
  not the first model is indexed.

- [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  now also removes [`exp()`](https://rdrr.io/r/base/Log.html)-pattern.

- [`clean_names()`](https://easystats.github.io/insight/reference/clean_names.md)
  for character-objects now works with “interaction patterns” (like
  `clean_names("scale(a):scale(b)")`).

- [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md)
  gains a `protect_ratio` argument, to print numbers smaller than 1 as
  ratios.

### Bug fixes

- Fix issues in CRAN checks.

- [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  now works for more complex `BFBayesFactor` objects that have multiple
  custom priors.

## insight 0.9.5

CRAN release: 2020-09-07

### Breaking changes

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  did not always “back-transform” log-transformed or scaled variables to
  return the original values. Now this bug has been fixed, and
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  should return all variables on the original scale (as if these
  variables were not transformed), as stated in the docs.

### Bug fixes

- [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  now returns the correct original data for “empty” polynomials
  (i.e. `poly(x, 1)`).

- Fix CRAN check issues due to latest *estimatr* update.

## insight 0.9.1

CRAN release: 2020-08-26

### New supported model classes

- Support for `mipo` (*mice*), `lqmm` and `lqm` (*lqmm*). Preliminary
  support for `semLME` (*smicd*), `mle` (*stats4*) and `mle2` (*bbmle*).

### Changes to functions

- [`model_info()`](https://easystats.github.io/insight/reference/model_info.md)
  returns `$is_meta = TRUE` for *brms*-meta-analysis models.

- Make
  [`find_statistic()`](https://easystats.github.io/insight/reference/find_statistic.md)
  work with [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  now also support
  [`truncated_nbinom2()`](https://rdrr.io/pkg/glmmTMB/man/nbinom2.html)
  family from *glmmTMB*.

### Bug fixes

- Fixed issue with correctly detecting sigma-parameters in
  [`find_parameters()`](https://easystats.github.io/insight/reference/find_parameters.md)
  for multiple-response `brmsfit`-models.

- Fixed issue with
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  for models from
  [`stan_nlmer()`](https://mc-stan.org/rstanarm/reference/stan_nlmer.html).

- Fixed issues with
  [`find_terms()`](https://easystats.github.io/insight/reference/find_terms.md)
  when response variable included a namespace, like
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

- Fixed issues with
  [`get_priors()`](https://easystats.github.io/insight/reference/get_priors.md)
  for *stanreg* models, probably caused by the latest update to
  *rstanarm 2.21.2*.

- Fixed issues in
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for *brmsfit* models.

- Fixed some issues around `crq` objects (package *quantreg*).

## insight 0.9.0

CRAN release: 2020-07-20

### New supported model classes

- `BGGM` (*BGGM*), `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm`
  (*glmm*), improved support for `manova` (*stats*)

### New functions

- Value formatting functions
  [`format_bf()`](https://easystats.github.io/insight/reference/format_bf.md),
  [`format_pd()`](https://easystats.github.io/insight/reference/format_pd.md),
  [`format_p()`](https://easystats.github.io/insight/reference/format_p.md),
  [`format_rope()`](https://easystats.github.io/insight/reference/format_rope.md)
  and
  [`format_number()`](https://easystats.github.io/insight/reference/format_number.md)
  were moved from package *parameters* to *insight*.

### Changes to functions

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  now also returns the correlation among random slopes.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  now also (partially) supports `brmsfit` models.

- [`get_parameters()`](https://easystats.github.io/insight/reference/get_parameters.md)
  for models that return (posterior or simulated) samples of model
  parameters gains a `summary`-argument, which - if `TRUE` - returns a
  point-estimate (mean of samples) instead of the full samples.

- [`format_p()`](https://easystats.github.io/insight/reference/format_p.md)
  returns `"> .999"` for p-values equal to or greater than 0.999.

### Bug fixes

- Fixed issue in
  [`find_formula()`](https://easystats.github.io/insight/reference/find_formula.md)
  that did not properly work for models with random effects in formula
  (in *lme4* notation), when random effects were in between fixed
  effects parts.

- [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  did not return variance components for random effects for null-models
  with random slopes.

- Fixed issue with
  [`get_variance()`](https://easystats.github.io/insight/reference/get_variance.md)
  for `lme`-models with categorical random slope.

- Fixed issue that occurred since R 4.0.0 in
  [`find_weights()`](https://easystats.github.io/insight/reference/find_weights.md)
  when function call had no `weights`-argument.

- Fixed issue in
  [`get_data()`](https://easystats.github.io/insight/reference/get_data.md)
  for models with
  [`cbind()`](https://rdrr.io/r/base/cbind.html)-response variables and
  matrix-like variables in the model frame (e.g. when using
  [`poly()`](https://rdrr.io/r/stats/poly.html)).

- Fixed issues with
  [`PROreg::BBmm()`](https://rdrr.io/pkg/PROreg/man/BBmm.html), due to
  changes in latest package update.
