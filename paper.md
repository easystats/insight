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
affiliations:
  - name: University Medical Center Hamburg-Eppendorf
    index: 1
  - name: Nanyang Technological University
    index: 2
date: 22 April 2019
bibliography: paper.bib
---

# Summary

Although there are generic functions to get information and data from models, many modelling-functions from different packages do not provide such methods to access these information. The *insight* package aims at closing this gap by providing functions that work for (almost) any model.

## Aim of the Package

The goal of *insight* is to provide tools to help an **easy**, **intuitive** and **consistent** accesss to information contained in various models, like model formulas, model terms, information about random effects, data that was used to fit the model or data from response variables. 

The syntax of `insight` mainly revolves around two types of functions. One is to find the names of the *things* (`find_*`), and the second is to actually get the *things* (`get_`). The *things* can be the following:

## Support for many different Models

*insight* works with many different model-objects: **AER** (*ivreg, tobit*), **afex** (*mixed*), **base** (*aov, aovlist, lm, glm*), **betareg** (*betareg*), **blme** (*blmer, bglmer*), **brms** (*brmsfit*), **crch**, **countreg** (*zerontrunc*), **coxme**, **estimatr** (*lm_robust, iv_robust*), **feisr** (*feis*), **gam** (*Gam*), **gamm4** , **gamlss**, **gbm**, **gee**, **geepack** (*geeglm*), **GLMMadaptive** (*MixMod*), **glmmTMB** (*glmmTMB*), **gmnl**, **lfe** (*felm*), **lme4** (*lmer, glmer, nlmer, glmer.nb*), **MASS** (*glmmPQL, polr*), **mgcv** (*gam, gamm*), **nnet** (*multinom*), **nlme** (*lme, gls*), **ordinal** (*clm, clm2, clmm*), **plm**, **pscl** (*zeroinf, hurdle*), **quantreg** (*rq, crq, rqss*), **robust** (*glmRob, lmRob*), **robustbase** (*glmrob, lmrob*), **robustlmm** (*rlmer*), **rstanarm** (*stanreg, stanmvreg*), **survey**, **survival** (*coxph, survreg*), **truncreg** (*truncreg*), **VGAM** (*vgam, vglm*).

Although a wide range of different model-objects is already covered, the aim is to add support for more objects on request.

## Examples

