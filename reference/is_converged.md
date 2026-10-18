# Convergence test for mixed effects, Stan and Cox models

`is_converged()` provides an alternative convergence test for
`merMod`-objects. For models fitted with Stan (`stanfit`, `brmsfit`,
`stanreg` and `CmdStanMCMC`), it checks the diagnostics of the sampler.
For `coxph` models, it recomputes the checks of the *survival* package.

## Usage

``` r
is_converged(x, tolerance = 0.001, ...)

# S3 method for class 'merMod'
is_converged(x, tolerance = 0.001, verbose = TRUE, ...)

# S3 method for class 'stanfit'
is_converged(x, tolerance = 0.001, verbose = TRUE, ...)

# S3 method for class 'coxph'
is_converged(x, tolerance = 0.001, verbose = TRUE, ...)
```

## Arguments

- x:

  A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan`,
  `_glm`, `stanfit`, `brmsfit`, `stanreg`, `CmdStanMCMC` or `coxph`.

- tolerance:

  Indicates up to which value the convergence result is accepted. The
  smaller `tolerance` is, the stricter the test will be. Not used for
  Stan and `coxph` models.

- ...:

  Currently not used.

- verbose:

  Toggle messages and warnings.

## Value

`TRUE` if convergence is fine and `FALSE` if convergence is suspicious
or cannot be assessed. For `merMod` models, the convergence value is
returned as attribute `gradient`. If the model is singular, convergence
is determined by the optimizer's convergence code. For non-singular
models where derivatives are unavailable, `FALSE` is returned and a
message is printed to indicate that convergence cannot be assessed
through the usual gradient-based checks.

For Stan models, the attribute `diagnostics` is a data frame with the
value, the threshold and the result of each check. For Stan models whose
convergence cannot be assessed, `FALSE` is returned without this
attribute, and a message gives the reason: models without MCMC draws
from the NUTS sampler (for example, models fitted with variational
inference or optimization), models without draws after warmup, and
models fitted with
[`brms::brm_multiple()`](https://paulbuerkner.com/brms/reference/brm_multiple.html),
whose chains come from different data sets (such as imputed ones).

For `coxph` models, the attribute `diagnostics` is a data frame with the
result of each check. If convergence cannot be assessed, `FALSE` is
returned without this attribute, and, if `verbose = TRUE`, a message
gives the reason.

## Stan models

For models fitted with Stan, `is_converged()` returns `FALSE` if at
least one of the checks below fails. The checks and thresholds are those
of the warnings that *rstan* gives after sampling. For `stanfit`,
`brmsfit` and `stanreg` models, the values are computed with functions
from *rstan*:

- Divergent transitions after warmup
  ([`rstan::get_num_divergent()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html)):
  the check fails if there is at least one.

- Transitions after warmup that reach the maximum treedepth stored in
  the model
  ([`rstan::get_sampler_params()`](https://mc-stan.org/rstan/reference/stanfit-class.html)):
  the check fails if there is at least one. The maximum is read from the
  `control` settings. Models from
  [`rstan::read_stan_csv()`](https://mc-stan.org/rstan/reference/stan_csv.html)
  store it outside `control`, and `is_converged()` reads it from there.
  For these models,
  [`rstan::get_num_max_treedepth()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html)
  counts against the default maximum of 10. If the model stores no
  maximum, 10 is used.

- E-BFMI
  ([`rstan::get_bfmi()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html)):
  the check fails if at least one chain has a value below 0.2. This is
  the E-BFMI of
  [`rstan::check_hmc_diagnostics()`](https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html),
  which can differ from the warning that *rstan* prints after sampling.

- R-hat
  ([`rstan::Rhat()`](https://mc-stan.org/rstan/reference/Rhat.html)):
  the check fails if the largest value over all parameters is above
  1.05.

- Bulk and tail effective sample size
  ([`rstan::ess_bulk()`](https://mc-stan.org/rstan/reference/Rhat.html)
  and
  [`rstan::ess_tail()`](https://mc-stan.org/rstan/reference/Rhat.html)):
  each check fails if the smallest value over all parameters is below
  100 times the number of chains with draws after warmup.

For `CmdStanMCMC` models from *cmdstanr*, the same checks and thresholds
are used. Divergent transitions, transitions at the maximum treedepth
and E-BFMI come from the `$diagnostic_summary()` method of the model,
and R-hat and effective sample size from its `$summary()` method. The
E-BFMI warning of *cmdstanr* uses a threshold of 0.3, not 0.2.

Missing values (for example, R-hat of a constant parameter) are ignored.
Stan also prints messages about rejected proposals ("exception thrown")
during sampling. These messages are not checked, because the model
object does not store them.

## Convergence and log-likelihood

Convergence problems typically arise when the model hasn't converged to
a solution where the log-likelihood has a true maximum. This may result
in unreliable and overly complex (or non-estimable) estimates and
standard errors.

## Inspect model convergence

**lme4** performs a convergence-check (see
[`?lme4::convergence`](https://rdrr.io/pkg/lme4/man/convergence.html)),
however, as discussed [here](https://github.com/lme4/lme4/issues/120)
and suggested by one of the lme4-authors in [this
comment](https://github.com/lme4/lme4/issues/120#issuecomment-39920269),
this check can be too strict. `is_converged()` (and its wrapper
function,
[`performance::check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.html))
thus provides an alternative convergence test for `merMod`-objects.

## Resolving convergence issues

Convergence issues are not easy to diagnose. The help page on
[`?lme4::convergence`](https://rdrr.io/pkg/lme4/man/convergence.html)
provides most of the current advice about how to resolve convergence
issues. In general, convergence issues may be addressed by one or more
of the following strategies: 1. Rescale continuous predictors; 2. try a
different optimizer; 3. increase the number of iterations; or, if
everything else fails, 4. simplify the model. Another clue might be
large parameter values, e.g. estimates (on the scale of the linear
predictor) larger than 10 in (non-identity link) generalized linear
model *might* indicate complete separation, which can be addressed by
regularization, e.g. penalized regression or Bayesian regression with
appropriate priors on the fixed effects.

## Cox proportional hazards models

*survival* warns about convergence problems when a `coxph` model is
fitted, but does not store the warnings in the model object. For `coxph`
models, `is_converged()` therefore computes the two checks of *survival*
again, with the `iter.max`, `eps` and `toler.inf` values of the model
call:

- Iterations: the check fails if the model did not converge within
  `iter.max` iterations ("Ran out of iterations and did not converge").

- Infinite coefficient: the check fails for a coefficient if the
  log-likelihood converged before the coefficient did ("Loglik converged
  before variable ...; coefficient may be infinite", or "beta may be
  infinite" for counting-process data). This happens, for example, if a
  factor level has no events. As in *survival*, this check runs only if
  the first check passed.

`is_converged()` returns `FALSE` if a check fails. The attribute
`diagnostics` is a data frame with the value, the threshold and the
result of each check, with one row for each coefficient for the second
check. The `tolerance` argument is not used for `coxph` models.

Convergence cannot be assessed, and `FALSE` is returned, for penalized
models (with `frailty()`, `ridge()` or `pspline()` terms), for models
with `ties = "exact"`, for models with `tt()` terms, for models with
`iter.max` of 1 or less, for right-censored and multi-state models
fitted with `y = FALSE`, for models without events, for model objects
without a call, and if the control arguments of the model call or the
score residuals cannot be computed. Convergence also cannot be assessed
if the model call does not write the formula as a `~` expression, for
example `coxph(f, data = d, iter.max = n)`, and a control argument
refers to a variable. The control arguments are evaluated in the
environment of the formula, which can differ from the environment in
which the model was fitted, for example inside a function. This also
applies to models fitted in top-level code, where the two environments
are usually the same. Single-state counting-process models fitted with
`y = FALSE` are assessed, because their checks use only the stored score
vector and convergence flag. A model without events and without
predictors returns `TRUE`, as other null models.

Models from
[`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html) are
assessed as other `coxph` models. The default method of `clogit()` is
`"exact"`, so convergence can only be assessed for models fitted with
`method = "efron"` or `method = "breslow"` (or `"approximate"`). Objects
of other classes that inherit from `coxph`, for example from
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html),
are not supported: `NULL` is returned with a message (no message if
`verbose = FALSE`).

For models with right-censored data, the score residuals are computed
from the data of the model call, unless the model was fitted with
`model = TRUE`, or with `x = TRUE` for single-state models. If these
data were removed after the model was fitted, convergence cannot be
assessed. If the predictor data were changed, so that the linear
predictor computed from them differs from the one of the model,
convergence cannot be assessed either. For single-state models with a
`strata()` term, the log-likelihood and the martingale residuals at the
coefficients of the model are also computed again from these data and
the stored response and weights. If they differ from those of the model,
for example because observations were moved to other strata, or if they
cannot be computed, convergence cannot be assessed. Strata that are
relabeled without a change of grouping are not detected, but they leave
the result the same. For multi-state models, the linear predictor is
compared within each transition. A change that adds the same constant to
all values of a predictor is not detected, but it leaves the score
residuals the same. The check is not done for multi-state models in
which transitions share a baseline hazard, or that were fitted with
`x = TRUE`. Other changes that stay undetected can give a wrong result:
a change of several predictors that leaves the linear predictor the
same, changes to a `strata()` or `istate` variable of a multi-state
model, and changes to the data of models without the check. In these
cases, refit the model, or fit it with `model = TRUE`, which avoids
them.

## Convergence versus Singularity

Note the different meaning between singularity and convergence:
singularity indicates an issue with the "true" best estimate, i.e.
whether the maximum likelihood estimation for the variance-covariance
matrix of the random effects is positive definite or only semi-definite.
Convergence is a question of whether we can assume that the numerical
optimization has worked correctly or not. A convergence failure means
the optimizer (the algorithm) could not find a stable solution (*Bates
et. al 2015*).

For singular models (see
[`?lme4::isSingular`](https://rdrr.io/pkg/lme4/man/isSingular.html)),
convergence is determined based on the optimizer's convergence code. If
the optimizer reports successful convergence (convergence code 0) for a
singular model, `is_converged()` returns `TRUE`. For non-singular
models, in cases where the gradient and Hessian are not available,
`is_converged()` returns `FALSE` and prints a message to indicate that
convergence cannot be assessed through the usual gradient-based checks.
Note that
[`performance::check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.html)
is a wrapper around `insight::is_converged()`.

## References

Bates, D., Mächler, M., Bolker, B., and Walker, S. (2015). Fitting
Linear Mixed-Effects Models Using lme4. Journal of Statistical Software,
67(1), 1-48.
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01)

## Examples

``` r
library(lme4)
data(cbpp)
set.seed(1)
cbpp$x <- rnorm(nrow(cbpp))
cbpp$x2 <- runif(nrow(cbpp))

model <- glmer(
  cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
  data = cbpp,
  family = binomial()
)
#> boundary (singular) fit: see help('isSingular')

is_converged(model)
#> [1] TRUE
#> attr(,"gradient")
#> [1] NA
# \donttest{
library(glmmTMB)
model <- glmmTMB(
  Sepal.Length ~ poly(Petal.Width, 4) * poly(Petal.Length, 4) +
    (1 + poly(Petal.Width, 4) | Species),
  data = iris
)
#> Warning: Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
#> Warning: Model convergence problem; false convergence (8). See vignette('troubleshooting'), help('diagnose')

is_converged(model)
#> [1] FALSE
# }
# \donttest{
# a model fitted with brms
model <- download_model("brms_1")
result <- is_converged(model)
result
#> [1] TRUE
#> attr(,"diagnostics")
#>    Diagnostic        Value Threshold Passed
#> 1 Divergences    0.0000000      0.00   TRUE
#> 2   Treedepth    0.0000000      0.00   TRUE
#> 3      E-BFMI    0.8455105      0.20   TRUE
#> 4        Rhat    1.0021166      1.05   TRUE
#> 5    ESS_bulk 1631.5978432    400.00   TRUE
#> 6    ESS_tail 1918.0354686    400.00   TRUE
attributes(result)$diagnostics
#>    Diagnostic        Value Threshold Passed
#> 1 Divergences    0.0000000      0.00   TRUE
#> 2   Treedepth    0.0000000      0.00   TRUE
#> 3      E-BFMI    0.8455105      0.20   TRUE
#> 4        Rhat    1.0021166      1.05   TRUE
#> 5    ESS_bulk 1631.5978432    400.00   TRUE
#> 6    ESS_tail 1918.0354686    400.00   TRUE
# }
```
