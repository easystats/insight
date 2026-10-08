# Convergence test for mixed effects and Cox models

`is_converged()` provides an alternative convergence test for
`merMod`-objects. For `coxph` models, it recomputes the checks of the
*survival* package.

## Usage

``` r
is_converged(x, tolerance = 0.001, ...)

# S3 method for class 'merMod'
is_converged(x, tolerance = 0.001, verbose = TRUE, ...)

# S3 method for class 'coxph'
is_converged(x, tolerance = 0.001, verbose = TRUE, ...)
```

## Arguments

- x:

  A model object from class `merMod`, `glmmTMB`, `glm`, `lavaan`, `_glm`
  or `coxph`.

- tolerance:

  Indicates up to which value the convergence result is accepted. The
  smaller `tolerance` is, the stricter the test will be. Not used for
  `coxph` models.

- ...:

  Currently not used.

- verbose:

  Toggle messages and warnings.

## Value

`TRUE` if convergence is fine and `FALSE` if convergence is suspicious.
Additionally, the convergence value is returned as attribute. For
`merMod` models, if the model is singular, convergence is determined by
the optimizer's convergence code. For non-singular models where
derivatives are unavailable, `FALSE` is returned and a message is
printed to indicate that convergence cannot be assessed through the
usual gradient-based checks. For `coxph` models, the attribute
`diagnostics` is a data frame with the result of each check. If
convergence cannot be assessed, `FALSE` is returned without this
attribute, and, if `verbose = TRUE`, a message gives the reason.

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
`iter.max` of 1 or less, for models fitted with `y = FALSE`, for model
objects without a call, and if the control arguments of the model call
or the score residuals cannot be computed. Objects of other classes that
inherit from `coxph`, for example from
[`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html) or
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html),
are not supported: `NULL` is returned with a message.

For models with right-censored data, the score residuals are computed
from the data of the model call, unless the model was fitted with
`model = TRUE` or `x = TRUE`. If these data were removed after the model
was fitted, convergence cannot be assessed. If they were changed, the
result can be wrong. In both cases, refit the model with `model = TRUE`.

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
```
