# Extract degrees of freedom

Estimate or extract residual or model-based degrees of freedom from
regression models.

## Usage

``` r
get_df(x, ...)

# Default S3 method
get_df(x, type = "residual", verbose = TRUE, ...)
```

## Arguments

- x:

  A statistical model.

- ...:

  Currently not used.

- type:

  Type of approximation for the degrees of freedom. Can be one of the
  following:

  - `"residual"` (aka `"analytical"`) returns the residual degrees of
    freedom, which usually is what
    [`stats::df.residual()`](https://rdrr.io/r/stats/df.residual.html)
    returns. If a model object has no method to extract residual degrees
    of freedom, these are calculated as `n-p`, i.e. the number of
    observations minus the number of estimated parameters. If residual
    degrees of freedom cannot be extracted by either approach, returns
    `Inf`.

  - `"wald"` returns residual (aka analytical) degrees of freedom for
    models with t-statistic, `1` for models with Chi-squared statistic,
    and `Inf` for all other models. Also returns `Inf` if residual
    degrees of freedom cannot be extracted.

  - `"normal"` always returns `Inf`.

  - `"model"` returns model-based degrees of freedom, i.e. the number of
    (estimated) parameters.

  - For mixed models, can also be `"ml1"` (or `"m-l-1"`, approximation
    of degrees of freedom based on a "m-l-1" heuristic as suggested by
    *Elff et al. 2019*) or `"between-within"` (or `"betwithin"`).

  - For mixed models of class `merMod`, `type` can also be
    `"satterthwaite"` or `"kenward-roger"` (or `"kenward"`). See
    'Details'.

  Usually, when degrees of freedom are required to calculate p-values or
  confidence intervals, `type = "wald"` is likely to be the best choice
  in most cases.

- verbose:

  Toggle warnings.

## Details

**Degrees of freedom for mixed models**

Inferential statistics (like p-values, confidence intervals and standard
errors) may be biased in mixed models when the number of clusters is
small (even if the sample size of level-1 units is high). In such cases
it is recommended to approximate a more accurate number of degrees of
freedom for such inferential statistics (see *Li and Redden 2015*).

*m-l-1 degrees of freedom*

The *m-l-1* heuristic is an approach that uses a t-distribution with
fewer degrees of freedom. In particular for repeated measure designs
(longitudinal data analysis), the m-l-1 heuristic is likely to be more
accurate than simply using the residual or infinite degrees of freedom,
because `get_df(type = "ml1")` returns different degrees of freedom for
within-cluster and between-cluster effects. Note that the "m-l-1"
heuristic is not applicable (or at least less accurate) for complex
multilevel designs, e.g. with cross-classified clusters. In such cases,
more accurate approaches like the Kenward-Roger approximation is
recommended. However, the "m-l-1" heuristic also applies to generalized
mixed models, while approaches like Kenward-Roger or Satterthwaite are
limited to linear mixed models only.

*Between-within degrees of freedom*

The Between-within denominator degrees of freedom approximation is,
similar to the "m-l-1" heuristic, recommended in particular for
(generalized) linear mixed models with repeated measurements
(longitudinal design). `get_df(type = "betwithin")` implements a
heuristic based on the between-within approach, i.e. this type returns
different degrees of freedom for within-cluster and between-cluster
effects. Note that this implementation does not return exactly the same
results as shown in *Li and Redden 2015*, but similar.

*Satterthwaite and Kenward-Rogers degrees of freedom*

Unlike simpler approximation heuristics like the "m-l-1" rule
(`type = "ml1"`), the Satterthwaite or Kenward-Rogers approximation is
also applicable in more complex multilevel designs. However, the "m-l-1"
or "between-within" heuristics also apply to generalized mixed models,
while approaches like Kenward-Roger or Satterthwaite are limited to
linear mixed models only.

## References

- Kenward, M. G., & Roger, J. H. (1997). Small sample inference for
  fixed effects from restricted maximum likelihood. Biometrics, 983-997.

- Satterthwaite FE (1946) An approximate distribution of estimates of
  variance components. Biometrics Bulletin 2 (6):110–4.

- Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019). Multilevel
  Analysis with Few Clusters: Improving Likelihood-based Methods to
  Provide Unbiased Estimates and Accurate Inference, British Journal of
  Political Science.

- Li, P., Redden, D. T. (2015). Comparing denominator degrees of freedom
  approximations for the generalized linear mixed model in analyzing
  binary outcome in small sample cluster-randomized trials. BMC Medical
  Research Methodology, 15(1), 38

## Examples

``` r
model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
get_df(model) # same as df.residual(model)
#> [1] 144
get_df(model, type = "model") # same as attr(logLik(model), "df")
#> [1] 7
```
