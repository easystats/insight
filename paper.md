---
title: 'insight: Easy Access to Model Information for Various Model Objects'
tags:
  - R
  - model information
  - regression models
authors:
  - name: Daniel Lüdecke
    orcid: 0000-0002-8895-3206
    affiliation: 1
  - name: Dominique Makowski
    orcid: 0000-0001-5375-9967
    affiliation: 2
  - name: Philip D. Waggoner
    orcid: 0000-0002-7825-7573
    affiliation: 3
affiliations:
  - name: University Medical Center Hamburg-Eppendorf
    index: 1
  - name: Nanyang Technological University
    index: 2
  - name: College of William & Mary
    index: 3
date: 22 April 2019
bibliography: paper.bib
---

# Summary

When fitting any statistical model, there are many useful pieces of information that are simultaneously calculated and stored beyond coefficient estimates and general model fit staistics. Although there exist some generic functions to obtain model information and data, many package-specific modeling functions do not provide such methods to allow users to access such valuable information. 

The *insight* package (stable version, v0.2.0) fills this important gap by providing a suite of functions to support almost any model (see a list of the many models supported below in the **Models Supported by *Insight*** section). The goal of *insight*, then, is to provide tools to provide *easy*, *intuitive*, and *consistent* accesss to information contained in model objects. Ultimately, the development of *insight* is in line with the overall philosophy of the [easystats project](https://github.com/easystats), which is to facilitate and streamline the process of doing statistical analysis and reporting the results in the R programming language. 

Built with non-programmers in mind, *insight* offers a broad toolbox for making model and data information easily accessible, revolving around two key prefixes: `get_*` and `find_*`. Generally, the `get_*` prefix extracts values associated with model-specific objects (e.g., parameters or algorithms), while the `find_*` prefix lists model-specific objects (e.g., priors and predictors). In total, the *insight* package includes 16 core functions: `get_data()`, `get_priors()`, `get_variance()`, `get_parameters()`, `get_predictors()`, `get_random()`, `get_response()`, `find_algorithm()`, `find_formula()`, `find_variables()`, `find_terms()`, `find_parameters()`, `find_predictors()`, `find_random()`, `find_response()`, and `model_info()`. In all cases, users must supply at a minimum, the name of the model fit object. In several functions, there are additional arguments that allow for more targeted returns of model information. For example, the `find_terms()` function's `effects` argument allows for the extraction of "fixed effects" terms, "random effects" terms, or by default, "all" terms in the model object. We point users to the package documentation or the complementary package website, https://easystats.github.io/insight/, for a detailed list of the arguments associated with each function as well as the returned values from each function.

For a more intuitive introduction, consider the following examples for three major types of models: ordinary least squares (OLS) regression, linear mixed effects models, and Bayesian models. Importantly, though only a few functions are included below for the sake of space, users are encouraged to inspect the package documentation for an exhaustive list of package functionality with accompanying examples.

## Examples

```{R}
# Load the "insight" package
install.packages("insight")
library(insight)


# Sample model 1: OLS
sample1 <- lm(mpg ~ cyl + wt + hp, 
              data = mtcars)

get_parameters(sample1)

find_algorithm(sample1)

find_formula(sample1)


# Sample model 2: Linear Mixed Effects (via lme4)
sample2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), 
                      data = sleepstudy)

get_parameters(sample2)

find_algorithm(sample2)

find_formula(sample2)


# Sample model 3: Bayesian (via rstanarm)
sample3 <- rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length, 
                              data = iris)

get_priors(sample3)

dplyr::sample_n(get_parameters(sample3), 5) # view 5 random rows

find_algorithm(sample3)

find_formula(sample3)
```

## Models Supported by *Insight*

*insight* works with many different model-objects: **AER** (*ivreg, tobit*), **afex** (*mixed*), **base** (*aov, aovlist, lm, glm*), **betareg** (*betareg*), **blme** (*blmer, bglmer*), **brms** (*brmsfit*), **crch**, **countreg** (*zerontrunc*), **coxme**, **estimatr** (*lm_robust, iv_robust*), **feisr** (*feis*), **gam** (*Gam*), **gamm4** , **gamlss**, **gbm**, **gee**, **geepack** (*geeglm*), **GLMMadaptive** (*MixMod*), **glmmTMB** (*glmmTMB*), **gmnl**, **lfe** (*felm*), **lme4** (*lmer, glmer, nlmer, glmer.nb*), **MASS** (*glmmPQL, polr*), **mgcv** (*gam, gamm*), **nnet** (*multinom*), **nlme** (*lme, gls*), **ordinal** (*clm, clm2, clmm*), **plm**, **pscl** (*zeroinf, hurdle*), **quantreg** (*rq, crq, rqss*), **robust** (*glmRob, lmRob*), **robustbase** (*glmrob, lmrob*), **robustlmm** (*rlmer*), **rstanarm** (*stanreg, stanmvreg*), **survey**, **survival** (*coxph, survreg*), **truncreg** (*truncreg*), **VGAM** (*vgam, vglm*).

## Licensing and Package Access

*Insight* is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/easystats/insight), with a corresponding issue tracker for bug-reporting and feature enhancements. In the spirit of open science and research, we encourage interaction with our package through requests/tips for fixes, support for additional model objects, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.
