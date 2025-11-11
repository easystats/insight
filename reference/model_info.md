# Access information from model objects

Retrieve information from model objects.

## Usage

``` r
model_info(x, ...)

# Default S3 method
model_info(x, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
model_info(x, response = NULL, ...)
```

## Arguments

- x:

  A fitted model.

- ...:

  Currently not used.

- verbose:

  Toggle off warnings.

- response:

  If `x` is a multivariate response model, `model_info()` returns a list
  of information for each response variable. Set `response` to the
  number of a specific response variable, or provide the name of the
  response variable in `response`, to return the information for only
  one response.

## Value

A list with information about the model, like family, link-function etc.
(see 'Details').

## Details

`model_info()` returns a list with information about the model for many
different model objects. Following information is returned, where all
values starting with `is_` are logicals.

**Common families and distributions:**

- `is_bernoulli`: special case of binomial models: family is Bernoulli

- `is_beta`: family is beta

- `is_betabinomial`: family is beta-binomial

- `is_binomial`: family is binomial (but not negative binomial)

- `is_categorical`: family is categorical link

- `is_censored`: model is a censored model (has a censored response,
  including survival models)

- `is_count`: model is a count model (i.e. family is either poisson or
  negative binomial)

- `is_cumulative`: family is ordinal or cumulative link

- `is_dirichlet`: family is dirichlet

- `is_exponential`: family is exponential (e.g. Gamma or Weibull)

- `is_linear`: family is gaussian

- `is_logit`: model has logit link

- `is_multinomial`: family is multinomial or categorical link

- `is_negbin`: family is negative binomial

- `is_orderedbeta`: family is ordered beta

- `is_ordinal`: family is ordinal or cumulative link

- `is_poisson`: family is poisson

- `is_probit`: model has probit link

- `is_tweedie`: family is tweedie

**Special model types**:

- `is_anova`: model is an Anova object

- `is_bayesian`: model is a Bayesian model

- `is_dispersion`: model has dispersion component (not only dispersion
  *parameter*)

- `is_gam`: model is a generalized additive model

- `is_meta`: model is a meta-analysis object

- `is_mixed`: model is a mixed effects model (with random effects)

- `is_mixture`: model is a finite mixture model (currently only
  recognized for package *brms*).

- `is_multivariate`: model is a multivariate response model (currently
  only works for *brmsfit* and *vglm/vgam* objects)

- `is_hurdle`: model has zero-inflation component and is a hurdle-model
  (truncated family distribution)

- `is_rtchoice`: model is a *brms* decision-making (sequential sampling)
  model, which models outcomes that consists of two components (reaction
  times and choice).

- `is_survival`: model is a survival model

- `is_trial`: model response contains additional information about the
  trials

- `is_truncated`: model is a truncated model (has a truncated response)

- `is_wiener`: model is a *brms* decision-making (sequential sampling)
  model with Wiener process (also called drift diffusion model)

- `is_zero_inflated`: model has zero-inflation component

**Hypotheses tests:**

- `is_binomtest`: model is an an object of class `htest`, returned by
  [`binom.test()`](https://rdrr.io/r/stats/binom.test.html)

- `is_chi2test`: model is an an object of class `htest`, returned by
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)

- `is_correlation`: model is an an object of class `htest`, returned by
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html)

- `is_ftest`: model is an an object of class `htest`, and test-statistic
  is an F-statistic.

- `is_levenetest`: model is an an object of class `anova`, returned by
  [`car::leveneTest()`](https://rdrr.io/pkg/car/man/leveneTest.html).

- `is_onewaytest`: model is an an object of class `htest`, returned by
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)

- `is_proptest`: model is an an object of class `htest`, returned by
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html)

- `is_ranktest`: model is an an object of class `htest`, returned by
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) (if Spearman's
  rank correlation), `wilcox.text()` or
  [`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html).

- `is_ttest`: model is an an object of class `htest`, returned by
  [`t.test()`](https://rdrr.io/r/stats/t.test.html)

- `is_variancetest`: model is an an object of class `htest`, returned by
  [`bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html),
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) or
  [`car::leveneTest()`](https://rdrr.io/pkg/car/man/leveneTest.html).

- `is_xtab`: model is an an object of class `htest` or `BFBayesFactor`,
  and test-statistic stems from a contingency table (i.e.
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
  [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html)).

**Other model information:**

- `link_function`: the link-function

- `family`: name of the distributional family of the model. For some
  exceptions (like some `htest` objects), can also be the name of the
  test.

- `n_obs`: number of observations

- `n_grouplevels`: for mixed models, returns names and numbers of random
  effect groups

## Examples

``` r
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
dat <- data.frame(ldose, sex, SF, stringsAsFactors = FALSE)
m <- glm(SF ~ sex * ldose, family = binomial)

# logistic regression
model_info(m)
#> $is_binomial
#> [1] TRUE
#> 
#> $is_bernoulli
#> [1] FALSE
#> 
#> $is_count
#> [1] FALSE
#> 
#> $is_poisson
#> [1] FALSE
#> 
#> $is_negbin
#> [1] FALSE
#> 
#> $is_beta
#> [1] FALSE
#> 
#> $is_betabinomial
#> [1] FALSE
#> 
#> $is_orderedbeta
#> [1] FALSE
#> 
#> $is_dirichlet
#> [1] FALSE
#> 
#> $is_exponential
#> [1] FALSE
#> 
#> $is_logit
#> [1] TRUE
#> 
#> $is_probit
#> [1] FALSE
#> 
#> $is_censored
#> [1] FALSE
#> 
#> $is_truncated
#> [1] FALSE
#> 
#> $is_survival
#> [1] FALSE
#> 
#> $is_linear
#> [1] FALSE
#> 
#> $is_tweedie
#> [1] FALSE
#> 
#> $is_zeroinf
#> [1] FALSE
#> 
#> $is_zero_inflated
#> [1] FALSE
#> 
#> $is_dispersion
#> [1] FALSE
#> 
#> $is_hurdle
#> [1] FALSE
#> 
#> $is_ordinal
#> [1] FALSE
#> 
#> $is_cumulative
#> [1] FALSE
#> 
#> $is_multinomial
#> [1] FALSE
#> 
#> $is_categorical
#> [1] FALSE
#> 
#> $is_mixed
#> [1] FALSE
#> 
#> $is_multivariate
#> [1] FALSE
#> 
#> $is_trial
#> [1] FALSE
#> 
#> $is_bayesian
#> [1] FALSE
#> 
#> $is_gam
#> [1] FALSE
#> 
#> $is_anova
#> [1] FALSE
#> 
#> $is_timeseries
#> [1] FALSE
#> 
#> $is_ttest
#> [1] FALSE
#> 
#> $is_correlation
#> [1] FALSE
#> 
#> $is_onewaytest
#> [1] FALSE
#> 
#> $is_chi2test
#> [1] FALSE
#> 
#> $is_ranktest
#> [1] FALSE
#> 
#> $is_levenetest
#> [1] FALSE
#> 
#> $is_variancetest
#> [1] FALSE
#> 
#> $is_xtab
#> [1] FALSE
#> 
#> $is_proptest
#> [1] FALSE
#> 
#> $is_binomtest
#> [1] FALSE
#> 
#> $is_ftest
#> [1] FALSE
#> 
#> $is_meta
#> [1] FALSE
#> 
#> $is_wiener
#> [1] FALSE
#> 
#> $is_rtchoice
#> [1] FALSE
#> 
#> $is_mixture
#> [1] FALSE
#> 
#> $link_function
#> [1] "logit"
#> 
#> $family
#> [1] "binomial"
#> 
#> $n_obs
#> [1] 12
#> 
#> $n_grouplevels
#> NULL
#> 

# t-test
m <- t.test(1:10, y = c(7:20))
model_info(m)
#> $is_binomial
#> [1] FALSE
#> 
#> $is_bernoulli
#> [1] FALSE
#> 
#> $is_count
#> [1] FALSE
#> 
#> $is_poisson
#> [1] FALSE
#> 
#> $is_negbin
#> [1] FALSE
#> 
#> $is_beta
#> [1] FALSE
#> 
#> $is_betabinomial
#> [1] FALSE
#> 
#> $is_orderedbeta
#> [1] FALSE
#> 
#> $is_dirichlet
#> [1] FALSE
#> 
#> $is_exponential
#> [1] FALSE
#> 
#> $is_logit
#> [1] FALSE
#> 
#> $is_probit
#> [1] FALSE
#> 
#> $is_censored
#> [1] FALSE
#> 
#> $is_truncated
#> [1] FALSE
#> 
#> $is_survival
#> [1] FALSE
#> 
#> $is_linear
#> [1] TRUE
#> 
#> $is_tweedie
#> [1] FALSE
#> 
#> $is_zeroinf
#> [1] FALSE
#> 
#> $is_zero_inflated
#> [1] FALSE
#> 
#> $is_dispersion
#> [1] FALSE
#> 
#> $is_hurdle
#> [1] FALSE
#> 
#> $is_ordinal
#> [1] FALSE
#> 
#> $is_cumulative
#> [1] FALSE
#> 
#> $is_multinomial
#> [1] FALSE
#> 
#> $is_categorical
#> [1] FALSE
#> 
#> $is_mixed
#> [1] FALSE
#> 
#> $is_multivariate
#> [1] FALSE
#> 
#> $is_trial
#> [1] FALSE
#> 
#> $is_bayesian
#> [1] FALSE
#> 
#> $is_gam
#> [1] FALSE
#> 
#> $is_anova
#> [1] FALSE
#> 
#> $is_timeseries
#> [1] FALSE
#> 
#> $is_ttest
#> [1] TRUE
#> 
#> $is_correlation
#> [1] FALSE
#> 
#> $is_onewaytest
#> [1] FALSE
#> 
#> $is_chi2test
#> [1] FALSE
#> 
#> $is_ranktest
#> [1] FALSE
#> 
#> $is_levenetest
#> [1] FALSE
#> 
#> $is_variancetest
#> [1] FALSE
#> 
#> $is_xtab
#> [1] FALSE
#> 
#> $is_proptest
#> [1] FALSE
#> 
#> $is_binomtest
#> [1] FALSE
#> 
#> $is_ftest
#> [1] FALSE
#> 
#> $is_meta
#> [1] FALSE
#> 
#> $is_wiener
#> [1] FALSE
#> 
#> $is_rtchoice
#> [1] FALSE
#> 
#> $is_mixture
#> [1] FALSE
#> 
#> $link_function
#> [1] "identity"
#> 
#> $family
#> [1] "gaussian"
#> 
#> $n_obs
#> NULL
#> 
#> $n_grouplevels
#> NULL
#> 
```
