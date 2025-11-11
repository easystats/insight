# Get variance components from random effects models

This function extracts the different variance components of a mixed
model and returns the result as list. Functions like
`get_variance_residual(x)` or `get_variance_fixed(x)` are shortcuts for
`get_variance(x, component = "residual")` etc.

## Usage

``` r
get_variance(x, ...)

# S3 method for class 'merMod'
get_variance(
  x,
  component = "all",
  tolerance = 1e-08,
  null_model = NULL,
  approximation = "lognormal",
  verbose = TRUE,
  ...
)

# S3 method for class 'glmmTMB'
get_variance(
  x,
  component = "all",
  model_component = NULL,
  tolerance = 1e-08,
  null_model = NULL,
  approximation = "lognormal",
  verbose = TRUE,
  ...
)

get_variance_residual(x, verbose = TRUE, ...)

get_variance_fixed(x, verbose = TRUE, ...)

get_variance_random(x, verbose = TRUE, tolerance = 1e-08, ...)

get_variance_distribution(x, verbose = TRUE, ...)

get_variance_dispersion(x, verbose = TRUE, ...)

get_variance_intercept(x, verbose = TRUE, ...)

get_variance_slope(x, verbose = TRUE, ...)

get_correlation_slope_intercept(x, verbose = TRUE, ...)

get_correlation_slopes(x, verbose = TRUE, ...)
```

## Arguments

- x:

  A mixed effects model.

- ...:

  Currently not used.

- component:

  Character value, indicating the variance component that should be
  returned. By default, all variance components are returned. Valid
  options are `"all"`, `"fixed"`, `"random"`, `"residual"`,
  `"distribution"`, `"dispersion"`, `"intercept"`, `"slope"`, `"rho01"`,
  and `"rho00"`, which are equivalent to calling the dedicated functions
  like `get_variance_residual()` etc. The distribution-specific
  (`"distribution"`) and residual (`"residual"`) variance are the most
  computational intensive components, and hence may take a few seconds
  to calculate.

- tolerance:

  Tolerance for singularity check of random effects, to decide whether
  to compute random effect variances or not. Indicates up to which value
  the convergence result is accepted. The larger tolerance is, the
  stricter the test will be. See
  [`performance::check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.html).

- null_model:

  Optional, a null-model to be used for the calculation of random effect
  variances. If `NULL`, the null-model is computed internally.

- approximation:

  Character string, indicating the approximation method for the
  distribution-specific (observation level, or residual) variance. Only
  applies to non-Gaussian models. Can be `"lognormal"` (default),
  `"delta"` or `"trigamma"`. For binomial models, the default is the
  *theoretical* distribution specific variance, however, it can also be
  `"observation_level"`. See *Nakagawa et al. 2017*, in particular
  supplement 2, for details.

- verbose:

  Toggle off warnings.

- model_component:

  For models that can have a zero-inflation component, specify for which
  component variances should be returned. If `NULL` or `"full"` (the
  default), both the conditional and the zero-inflation component are
  taken into account. If `"conditional"`, only the conditional component
  is considered.

## Value

A list with following elements:

- `var.fixed`, variance attributable to the fixed effects

- `var.random`, (mean) variance of random effects

- `var.residual`, residual variance (sum of dispersion and
  distribution-specific/observation level variance)

- `var.distribution`, distribution-specific (or observation level)
  variance

- `var.dispersion`, variance due to additive dispersion

- `var.intercept`, the random-intercept-variance, or
  between-subject-variance (τ₀₀)

- `var.slope`, the random-slope-variance (τ₁₁)

- `cor.slope_intercept`, the random-slope-intercept-correlation (ρ₀₁)

- `cor.slopes`, the correlation between random slopes (ρ₀₀)

## Details

This function returns different variance components from mixed models,
which are needed, for instance, to calculate r-squared measures or the
intraclass-correlation coefficient (ICC).

## Fixed effects variance

The fixed effects variance, σ²_(f), is the variance of the
matrix-multiplication β∗X (parameter vector by model matrix).

## Random effects variance

The random effect variance, σ²_(i), represents the *mean* random effect
variance of the model. Since this variance reflects the "average" random
effects variance for mixed models, it is also appropriate for models
with more complex random effects structures, like random slopes or
nested random effects. Details can be found in *Johnson 2014*, in
particular equation 10. For simple random-intercept models, the random
effects variance equals the random-intercept variance.

## Distribution-specific (observation level) variance

The distribution-specific variance, σ²_(d), is the conditional variance
of the response given the predictors , `Var[y|x]`, which depends on the
model family.

- **Gaussian:** For Gaussian models, it is σ² (i.e. `sigma(model)^2`).

- **Bernoulli:** For models with binary outcome, it is π²/3 for
  logit-link, `1` for probit-link, and π²/6 for cloglog-links.

- **Binomial:** For other binomial models, the distribution-specific
  variance for Bernoulli models is used, divided by a weighting factor
  based on the number of trials and successes.

- **Gamma:** Models from Gamma-families use μ² (as obtained from
  `family$variance()`).

- For all other models, the distribution-specific variance is by default
  based on lognormal approximation, log(1 + var(x) / μ²) (see *Nakagawa
  et al. 2017*). Other approximation methods can be specified with the
  `approximation` argument.

- **Zero-inflation models:** The expected variance of a zero-inflated
  model is computed according to *Zuur et al. 2012, p277*.

## Variance for the additive overdispersion term

The variance for the additive overdispersion term, σ²_(*e*), represents
"the excess variation relative to what is expected from a certain
distribution" (*Nakagawa et al. 2017*). In (most? many?) cases, this
will be `0`.

## Residual variance

The residual variance, σ²_(ε), is simply σ²_(d) + σ²_(*e*). It is also
called *within-subject variance*.

## Random intercept variance

The random intercept variance, or *between-subject* variance (τ₀₀), is
obtained from [`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html).
It indicates how much groups or subjects differ from each other, while
the residual variance σ²_(ε) indicates the *within-subject variance*.

## Random slope variance

The random slope variance (τ₁₁) is obtained from
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). This measure
is only available for mixed models with random slopes.

## Random slope-intercept correlation

The random slope-intercept correlation (ρ₀₁) is obtained from
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). This measure
is only available for mixed models with random intercepts and slopes.

## Supported models and model families

This function supports models of class `merMod` (including models from
**blme**), `clmm`, `cpglmm`, `glmmadmb`, `glmmTMB`, `MixMod`, `lme`,
`mixed`, `rlmerMod`, `stanreg`, `brmsfit` or `wbm`. Support for objects
of class `MixMod` (**GLMMadaptive**), `lme` (**nlme**) or `brmsfit`
(**brms**) is not fully implemented or tested, and therefore may not
work for all models of the aforementioned classes.

The results are validated against the solutions provided by *Nakagawa et
al. (2017)*, in particular examples shown in the Supplement 2 of the
paper. Other model families are validated against results from the
**MuMIn** package. This means that the returned variance components
should be accurate and reliable for following mixed models or model
families:

- Bernoulli (logistic) regression

- Binomial regression (with other than binary outcomes)

- Poisson and Quasi-Poisson regression

- Negative binomial regression (including nbinom1, nbinom2 and nbinom12
  families)

- Gaussian regression (linear models)

- Gamma regression

- Tweedie regression

- Beta regression

- Ordered beta regression

Following model families are not yet validated, but should work:

- Zero-inflated and hurdle models

- Beta-binomial regression

- Compound Poisson regression

- Generalized Poisson regression

- Log-normal regression

- Skew-normal regression

Extracting variance components for models with zero-inflation part is
not straightforward, because it is not definitely clear how the
distribution-specific variance should be calculated. Therefore, it is
recommended to carefully inspect the results, and probably validate
against other models, e.g. Bayesian models (although results may be only
roughly comparable).

Log-normal regressions (e.g. `lognormal()` family in **glmmTMB** or
`gaussian("log")`) often have a very low fixed effects variance (if they
were calculated as suggested by *Nakagawa et al. 2017*). This results in
very low ICC or r-squared values, which may not be meaningful (see
[`performance::icc()`](https://easystats.github.io/performance/reference/icc.html)
or
[`performance::r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.html)).

## References

- Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM
  to random slopes models. Methods in Ecology and Evolution, 5(9),
  944–946.
  [doi:10.1111/2041-210X.12225](https://doi.org/10.1111/2041-210X.12225)

- Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The
  coefficient of determination R2 and intra-class correlation
  coefficient from generalized linear mixed-effects models revisited and
  expanded. Journal of The Royal Society Interface, 14(134), 20170213.
  [doi:10.1098/rsif.2017.0213](https://doi.org/10.1098/rsif.2017.0213)

- Zuur, A. F., Savel'ev, A. A., & Ieno, E. N. (2012). Zero inflated
  models and generalized linear mixed models with R. Newburgh, United
  Kingdom: Highland Statistics.

## Examples

``` r
# \donttest{
library(lme4)
data(sleepstudy)
m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

get_variance(m)
#> $var.fixed
#> [1] 908.9534
#> 
#> $var.random
#> [1] 1698.084
#> 
#> $var.residual
#> [1] 654.94
#> 
#> $var.distribution
#> [1] 654.94
#> 
#> $var.dispersion
#> [1] 0
#> 
#> $var.intercept
#>  Subject 
#> 612.1002 
#> 
#> $var.slope
#> Subject.Days 
#>     35.07171 
#> 
#> $cor.slope_intercept
#>    Subject 
#> 0.06555124 
#> 
get_variance_fixed(m)
#> var.fixed 
#>  908.9534 
get_variance_residual(m)
#> var.residual 
#>       654.94 
# }
```
