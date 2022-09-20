
# insight <img src='man/figures/logo.png' align="right" height="139" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.01412/status.svg)](https://doi.org/10.21105/joss.01412)
[![downloads](https://cranlogs.r-pkg.org/badges/insight)](https://cranlogs.r-pkg.org/)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/insight)](https://cranlogs.r-pkg.org/)
[![status](https://tinyverse.netlify.com/badge/insight)](https://CRAN.R-project.org/package=insight)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

**Gain insight into your models!**

When fitting any statistical model, there are many useful pieces of
information that are simultaneously calculated and stored beyond
coefficient estimates and general model fit statistics. Although there
exist some generic functions to obtain model information and data, many
package-specific modelling functions do not provide such methods to
allow users to access such valuable information.

**insight** is an R-package that fills this important gap by providing a
suite of functions to support almost any model (see a list of the many
models supported below in the **List of Supported Packages and Models**
section). The goal of **insight**, then, is to provide tools to provide
*easy*, *intuitive*, and *consistent* access to information contained in
model objects. These tools aid applied research in virtually any field
who fit, diagnose, and present statistical models by streamlining access
to every aspect of many model objects via consistent syntax and output.

## Installation

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/insight)](https://cran.r-project.org/package=insight)
[![insight status
badge](https://easystats.r-universe.dev/badges/insight)](https://easystats.r-universe.dev)
[![R-CMD-check](https://github.com/easystats/insight/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/easystats/insight/actions)

The *insight* package is available on CRAN, while its latest development
version is available on R-universe (from *rOpenSci*) or GitHub.

| Type        | Source     | Command                                                                   |
|-------------|------------|---------------------------------------------------------------------------|
| Release     | CRAN       | `install.packages("insight")`                                             |
| Development | r-universe | `install.packages("insight", repos = "https://easystats.r-universe.dev")` |
| Development | GitHub     | `remotes::install_github("easystats/insight")`                            |

Once you have downloaded the package, you can then load it using:

``` r
library("insight")
```

> **Tip**
>
> Instead of `library(insight)`, use `library(easystats)`. This will
> make all features of the easystats-ecosystem available.
>
> To stay updated, use `easystats::install_latest()`.

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-insight-orange.svg?colorB=E91E63)](https://easystats.github.io/insight/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-insight-orange.svg?colorB=2196F3)](https://easystats.github.io/insight/reference/index.html)

Built with non-programmers in mind, **insight** offers a broad toolbox
for making model and data information easily accessible. While
**insight** offers many useful functions for working with and
understanding model objects (discussed below), we suggest users start
with `model_info()`, as this function provides a clean and consistent
overview of model objects (e.g., functional form of the model, the model
family, link function, number of observations, variables included in the
specification, etc.). With a clear understanding of the model
introduced, users are able to adapt other functions for more nuanced
exploration of and interaction with virtually any model object.Please
visit <https://easystats.github.io/insight/> for documentation.

### Definition of Model Components

The functions from **insight** address different components of a model.
In an effort to avoid confusion about specific “targets” of each
function, in this section we provide a short explanation of
**insight**’s definitions of regression model components.

#### Data

The dataset used to fit the model.

#### Parameters

Values estimated or learned from data that capture the relationship
between variables. In regression models, these are usually referred to
as *coefficients*.

#### Response and Predictors

-   **response**: the outcome or response variable (dependent variable)
    of a regression model.
-   **predictor**: independent variables of (the *fixed* part of) a
    regression model. For mixed models, variables that are only in the
    *random effects* part (i.e. grouping factors) of the model are not
    returned as predictors by default. However, these can be included
    using additional arguments in the function call, treating predictors
    are “unique”. As such, if a variable appears as a fixed effect and a
    random slope, it is treated as one (the same) predictor.

#### Variables

Any unique variable names that appear in a regression model, e.g.,
response variable, predictors or random effects. A “variable” only
relates to the unique occurence of a term, or the term name. For
instance, the expression `x + poly(x, 2)` has only the variable `x`.

#### Terms

Terms themselves consist of variable and factor names separated by
operators, or involve arithmetic expressions. For instance, the
expression `x + poly(x, 2)` has *one* variable `x`, but *two* terms `x`
and `poly(x, 2)`.

#### Random Effects

-   **random slopes**: variables that are specified as random slopes in
    a mixed effects model.
-   **random or grouping factors**: variables that are specified as
    grouping variables in a mixed effects model.

*Aren’t the predictors, terms and parameters the same thing?*

In some cases, yes. But not in all cases. Find out more by [**clicking
here to access the
documentation**](https://easystats.github.io/insight/articles/insight.html).

### Functions

The package revolves around two key prefixes: `get_*` and `find_*`. The
`get_*` prefix extracts *values* (or *data*) associated with
model-specific objects (e.g., parameters or variables), while the
`find_*` prefix *lists* model-specific objects (e.g., priors or
predictors). These are powerful families of functions allowing for great
flexibility in use, whether at a high, descriptive level (`find_*`) or
narrower level of statistical inspection and reporting (`get_*`).

![](https://raw.githubusercontent.com/easystats/insight/master/paper/figure1_small.png)

In total, the **insight** package includes 16 core functions:
[get_data()](https://easystats.github.io/insight/reference/get_data.html),
[get_priors()](https://easystats.github.io/insight/reference/get_priors.html),
[get_variance()](https://easystats.github.io/insight/reference/get_variance.html),
[get_parameters()](https://easystats.github.io/insight/reference/get_parameters.html),
[get_predictors()](https://easystats.github.io/insight/reference/get_predictors.html),
[get_random()](https://easystats.github.io/insight/reference/get_random.html),
[get_response()](https://easystats.github.io/insight/reference/get_response.html),
[find_algorithm()](https://easystats.github.io/insight/reference/find_algorithm.html),
[find_formula()](https://easystats.github.io/insight/reference/find_formula.html),
[find_variables()](https://easystats.github.io/insight/reference/find_variables.html),
[find_terms()](https://easystats.github.io/insight/reference/find_terms.html),
[find_parameters()](https://easystats.github.io/insight/reference/find_parameters.html),
[find_predictors()](https://easystats.github.io/insight/reference/find_predictors.html),
[find_random()](https://easystats.github.io/insight/reference/find_random.html),
[find_response()](https://easystats.github.io/insight/reference/find_response.html),
and
[model_info()](https://easystats.github.io/insight/reference/model_info.html).
In all cases, users must supply at a minimum, the name of the model fit
object. In several functions, there are additional arguments that allow
for more targeted returns of model information. For example, the
`find_terms()` function’s `effects` argument allows for the extraction
of “fixed effects” terms, “random effects” terms, or by default, “all”
terms in the model object. We point users to the package documentation
or the complementary package website,
<https://easystats.github.io/insight/>, for a detailed list of the
arguments associated with each function as well as the returned values
from each function.

### Examples of Use Cases in R

We now would like to provide examples of use cases of the **insight**
package. These examples probably do not cover typical real-world
problems, but serve as illustration of the core idea of this package:
The unified interface to access model information. **insight** should
help both users and package developers in order to reduce the hassle
with the many exceptions from various modelling packages when accessing
model information.

#### Making Predictions at Specific Values of a Term of Interest

Say, the goal is to make predictions for a certain term, holding
remaining co-variates constant. This is achieved by calling `predict()`
and feeding the `newdata`-argument with the values of the term of
interest as well as the “constant” values for remaining co-variates. The
functions `get_data()` and `find_predictors()` are used to get this
information, which then can be used in the call to `predict()`.

In this example, we fit a simple linear model, but it could be replaced
by (m)any other models, so this approach is “universal” and applies to
many different model objects.

``` r
library(insight)
m <- lm(
  Sepal.Length ~ Species + Petal.Width + Sepal.Width,
  data = iris
)

dat <- get_data(m)
pred <- find_predictors(m, flatten = TRUE)

l <- lapply(pred, function(x) {
  if (is.numeric(dat[[x]])) {
    mean(dat[[x]])
  } else {
    unique(dat[[x]])
  }
})

names(l) <- pred
l <- as.data.frame(l)

cbind(l, predictions = predict(m, newdata = l))
#>      Species Petal.Width Sepal.Width predictions
#> 1     setosa         1.2         3.1         5.1
#> 2 versicolor         1.2         3.1         6.1
#> 3  virginica         1.2         3.1         6.3
```

#### Printing Model Coefficients

The next example should emphasize the possibilities to generalize
functions to many different model objects using **insight**. The aim is
simply to print coefficients in a complete, human readable sentence.

The first approach uses the functions that are available for some, but
obviously not for all models, to access the information about model
coefficients.

``` r
print_params <- function(model) {
  paste0(
    "My parameters are ",
    paste0(row.names(summary(model)$coefficients), collapse = ", "),
    ", thank you for your attention!"
  )
}

m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
print_params(m1)
#> [1] "My parameters are (Intercept), Petal.Width, thank you for your attention!"

# obviously, something is missing in the output
m2 <- mgcv::gam(Sepal.Length ~ Petal.Width + s(Petal.Length), data = iris)
print_params(m2)
#> [1] "My parameters are , thank you for your attention!"
```

As we can see, the function fails for *gam*-models. As the access to
models depends on the type of the model in the R ecosystem, we would
need to create specific functions for all models types. With
**insight**, users can write a function without having to worry about
the model type.

``` r
print_params <- function(model) {
  paste0(
    "My parameters are ",
    paste0(insight::find_parameters(model, flatten = TRUE), collapse = ", "),
    ", thank you for your attention!"
  )
}

m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
print_params(m1)
#> [1] "My parameters are (Intercept), Petal.Width, thank you for your attention!"

m2 <- mgcv::gam(Sepal.Length ~ Petal.Width + s(Petal.Length), data = iris)
print_params(m2)
#> [1] "My parameters are (Intercept), Petal.Width, s(Petal.Length), thank you for your attention!"
```

## Contributing and Support

In case you want to file an issue or contribute in another way to the
package, please follow [this
guide](https://github.com/easystats/insight/blob/master/.github/CONTRIBUTING.md).
For questions about the functionality, you may either contact us via
email or also file an issue.

## List of Supported Models by Class

Currently, 215 model classes are supported.

``` r
supported_models()
#>   [1] "aareg"                   "afex_aov"               
#>   [3] "AKP"                     "Anova.mlm"              
#>   [5] "anova.rms"               "aov"                    
#>   [7] "aovlist"                 "Arima"                  
#>   [9] "averaging"               "bamlss"                 
#>  [11] "bamlss.frame"            "bayesQR"                
#>  [13] "bayesx"                  "BBmm"                   
#>  [15] "BBreg"                   "bcplm"                  
#>  [17] "betamfx"                 "betaor"                 
#>  [19] "betareg"                 "BFBayesFactor"          
#>  [21] "bfsl"                    "BGGM"                   
#>  [23] "bife"                    "bifeAPEs"               
#>  [25] "bigglm"                  "biglm"                  
#>  [27] "blavaan"                 "blrm"                   
#>  [29] "bracl"                   "brglm"                  
#>  [31] "brmsfit"                 "brmultinom"             
#>  [33] "btergm"                  "censReg"                
#>  [35] "cgam"                    "cgamm"                  
#>  [37] "cglm"                    "clm"                    
#>  [39] "clm2"                    "clmm"                   
#>  [41] "clmm2"                   "clogit"                 
#>  [43] "coeftest"                "complmrob"              
#>  [45] "confusionMatrix"         "coxme"                  
#>  [47] "coxph"                   "coxph.penal"            
#>  [49] "coxr"                    "cpglm"                  
#>  [51] "cpglmm"                  "crch"                   
#>  [53] "crq"                     "crqs"                   
#>  [55] "crr"                     "dep.effect"             
#>  [57] "DirichletRegModel"       "draws"                  
#>  [59] "drc"                     "eglm"                   
#>  [61] "elm"                     "epi.2by2"               
#>  [63] "ergm"                    "feglm"                  
#>  [65] "feis"                    "felm"                   
#>  [67] "fitdistr"                "fixest"                 
#>  [69] "flexsurvreg"             "gam"                    
#>  [71] "Gam"                     "gamlss"                 
#>  [73] "gamm"                    "gamm4"                  
#>  [75] "garch"                   "gbm"                    
#>  [77] "gee"                     "geeglm"                 
#>  [79] "glht"                    "glimML"                 
#>  [81] "glm"                     "Glm"                    
#>  [83] "glmm"                    "glmmadmb"               
#>  [85] "glmmPQL"                 "glmmTMB"                
#>  [87] "glmrob"                  "glmRob"                 
#>  [89] "glmx"                    "gls"                    
#>  [91] "gmnl"                    "HLfit"                  
#>  [93] "htest"                   "hurdle"                 
#>  [95] "iv_robust"               "ivFixed"                
#>  [97] "ivprobit"                "ivreg"                  
#>  [99] "lavaan"                  "lm"                     
#> [101] "lm_robust"               "lme"                    
#> [103] "lmerMod"                 "lmerModLmerTest"        
#> [105] "lmodel2"                 "lmrob"                  
#> [107] "lmRob"                   "logistf"                
#> [109] "logitmfx"                "logitor"                
#> [111] "LORgee"                  "lqm"                    
#> [113] "lqmm"                    "lrm"                    
#> [115] "manova"                  "MANOVA"                 
#> [117] "marginaleffects"         "marginaleffects.summary"
#> [119] "margins"                 "maxLik"                 
#> [121] "mblogit"                 "mclogit"                
#> [123] "mcmc"                    "mcmc.list"              
#> [125] "MCMCglmm"                "mcp1"                   
#> [127] "mcp12"                   "mcp2"                   
#> [129] "med1way"                 "mediate"                
#> [131] "merMod"                  "merModList"             
#> [133] "meta_bma"                "meta_fixed"             
#> [135] "meta_random"             "metaplus"               
#> [137] "mhurdle"                 "mipo"                   
#> [139] "mira"                    "mixed"                  
#> [141] "MixMod"                  "mixor"                  
#> [143] "mjoint"                  "mle"                    
#> [145] "mle2"                    "mlm"                    
#> [147] "mlogit"                  "mmclogit"               
#> [149] "mmlogit"                 "model_fit"              
#> [151] "multinom"                "mvord"                  
#> [153] "negbinirr"               "negbinmfx"              
#> [155] "ols"                     "onesampb"               
#> [157] "orm"                     "pgmm"                   
#> [159] "plm"                     "PMCMR"                  
#> [161] "poissonirr"              "poissonmfx"             
#> [163] "polr"                    "probitmfx"              
#> [165] "psm"                     "Rchoice"                
#> [167] "ridgelm"                 "riskRegression"         
#> [169] "rjags"                   "rlm"                    
#> [171] "rlmerMod"                "RM"                     
#> [173] "rma"                     "rma.uni"                
#> [175] "robmixglm"               "robtab"                 
#> [177] "rq"                      "rqs"                    
#> [179] "rqss"                    "rvar"                   
#> [181] "Sarlm"                   "scam"                   
#> [183] "selection"               "sem"                    
#> [185] "SemiParBIV"              "semLm"                  
#> [187] "semLme"                  "slm"                    
#> [189] "speedglm"                "speedlm"                
#> [191] "stanfit"                 "stanmvreg"              
#> [193] "stanreg"                 "summary.lm"             
#> [195] "survfit"                 "survreg"                
#> [197] "svy_vglm"                "svychisq"               
#> [199] "svyglm"                  "svyolr"                 
#> [201] "t1way"                   "tobit"                  
#> [203] "trimcibt"                "truncreg"               
#> [205] "vgam"                    "vglm"                   
#> [207] "wbgee"                   "wblm"                   
#> [209] "wbm"                     "wmcpAKP"                
#> [211] "yuen"                    "yuend"                  
#> [213] "zcpglm"                  "zeroinfl"               
#> [215] "zerotrunc"
```

-   **Didn’t find a model?** [File an
    issue](https://github.com/easystats/insight/issues) and request
    additional model-support in *insight*!

## Citation

If this package helped you, please consider citing as follows:

Lüdecke D, Waggoner P, Makowski D. insight: A Unified Interface to
Access Information from Model Objects in R. Journal of Open Source
Software 2019;4:1412. doi:
[10.21105/joss.01412](https://doi.org/10.21105/joss.01412)

## Code of Conduct

Please note that the insight project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
