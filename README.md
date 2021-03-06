
# insight <img src='man/figures/logo.png' align="right" height="139" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.01412/status.svg)](https://doi.org/10.21105/joss.01412)
[![downloads](https://cranlogs.r-pkg.org/badges/insight)](https://cranlogs.r-pkg.org/)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/insight)](https://cranlogs.r-pkg.org/)

**Gain insight into your models!**

When fitting any statistical model, there are many useful pieces of
information that are simultaneously calculated and stored beyond
coefficient estimates and general model fit statistics. Although there
exist some generic functions to obtain model information and data, many
package-specific modeling functions do not provide such methods to allow
users to access such valuable information.

**insight** is an R-package that fills this important gap by providing a
suite of functions to support almost any model (see a list of the many
models supported below in the **List of Supported Packages and Models**
section). The goal of **insight**, then, is to provide tools to provide
*easy*, *intuitive*, and *consistent* access to information contained in
model objects. These tools aid applied research in virtually any field
who fit, diagnose, and present statistical models by streamlining access
to every aspect of many model objects via consistent syntax and output.

## Installation

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/insight)](https://cran.r-project.org/package=insight)
[![R
check](https://github.com/easystats/insight/workflows/R-check/badge.svg?branch=master)](https://github.com/easystats/insight/actions)

Run the following to install the stable release of **insight** from
CRAN:

``` r
install.packages("insight")
```

Or this one to install the latest development version:

``` r
install.packages("remotes")
remotes::install_github("easystats/insight")
```

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
[get\_data()](https://easystats.github.io/insight/reference/get_data.html),
[get\_priors()](https://easystats.github.io/insight/reference/get_priors.html),
[get\_variance()](https://easystats.github.io/insight/reference/get_variance.html),
[get\_parameters()](https://easystats.github.io/insight/reference/get_parameters.html),
[get\_predictors()](https://easystats.github.io/insight/reference/get_predictors.html),
[get\_random()](https://easystats.github.io/insight/reference/get_random.html),
[get\_response()](https://easystats.github.io/insight/reference/get_response.html),
[find\_algorithm()](https://easystats.github.io/insight/reference/find_algorithm.html),
[find\_formula()](https://easystats.github.io/insight/reference/find_formula.html),
[find\_variables()](https://easystats.github.io/insight/reference/find_variables.html),
[find\_terms()](https://easystats.github.io/insight/reference/find_terms.html),
[find\_parameters()](https://easystats.github.io/insight/reference/find_parameters.html),
[find\_predictors()](https://easystats.github.io/insight/reference/find_predictors.html),
[find\_random()](https://easystats.github.io/insight/reference/find_random.html),
[find\_response()](https://easystats.github.io/insight/reference/find_response.html),
and
[model\_info()](https://easystats.github.io/insight/reference/model_info.html).
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
m <- lm(Sepal.Length ~ Species + Petal.Width + Sepal.Width, data = iris)

dat <- get_data(m)
pred <- find_predictors(m, flatten = TRUE)

l <- lapply(pred, function(x) {
    if (is.numeric(dat[[x]])) 
        mean(dat[[x]]) else unique(dat[[x]])
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
    paste0("My parameters are ", paste0(row.names(summary(model)$coefficients), collapse = ", "), 
        ", thank you for your attention!")
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
    paste0("My parameters are ", paste0(insight::find_parameters(model, flatten = TRUE), 
        collapse = ", "), ", thank you for your attention!")
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

Currently, 175 model classes are supported.

``` r
supported_models()
#>   [1] "aareg"             "afex_aov"          "Anova.mlm"        
#>   [4] "aov"               "aovlist"           "Arima"            
#>   [7] "averaging"         "bamlss"            "bamlss.frame"     
#>  [10] "bayesQR"           "bayesx"            "BBmm"             
#>  [13] "BBreg"             "bcplm"             "betamfx"          
#>  [16] "betaor"            "betareg"           "BFBayesFactor"    
#>  [19] "BGGM"              "bife"              "bigglm"           
#>  [22] "biglm"             "blavaan"           "blrm"             
#>  [25] "bracl"             "brglm"             "brmsfit"          
#>  [28] "brmultinom"        "btergm"            "censReg"          
#>  [31] "cgam"              "cgamm"             "cglm"             
#>  [34] "clm"               "clm2"              "clmm"             
#>  [37] "clmm2"             "clogit"            "coeftest"         
#>  [40] "complmrob"         "coxme"             "coxph"            
#>  [43] "coxph.penal"       "coxr"              "cpglm"            
#>  [46] "cpglmm"            "crch"              "crq"              
#>  [49] "crqs"              "crr"               "DirichletRegModel"
#>  [52] "eglm"              "elm"               "ergm"             
#>  [55] "feglm"             "feis"              "felm"             
#>  [58] "fitdistr"          "fixest"            "flexsurvreg"      
#>  [61] "gam"               "Gam"               "gamlss"           
#>  [64] "gamm"              "gamm4"             "garch"            
#>  [67] "gbm"               "gee"               "geeglm"           
#>  [70] "glht"              "glimML"            "glm"              
#>  [73] "Glm"               "glmm"              "glmmadmb"         
#>  [76] "glmmPQL"           "glmmTMB"           "glmrob"           
#>  [79] "glmRob"            "glmx"              "gls"              
#>  [82] "gmnl"              "HLfit"             "htest"            
#>  [85] "hurdle"            "iv_robust"         "ivFixed"          
#>  [88] "ivprobit"          "ivreg"             "lavaan"           
#>  [91] "lm"                "lm_robust"         "lme"              
#>  [94] "lmerMod"           "lmerModLmerTest"   "lmodel2"          
#>  [97] "lmrob"             "lmRob"             "logistf"          
#> [100] "logitmfx"          "logitor"           "LORgee"           
#> [103] "lqm"               "lqmm"              "lrm"              
#> [106] "manova"            "MANOVA"            "margins"          
#> [109] "maxLik"            "mclogit"           "mcmc"             
#> [112] "mcmc.list"         "MCMCglmm"          "mediate"          
#> [115] "merMod"            "merModList"        "meta_bma"         
#> [118] "meta_fixed"        "meta_random"       "metaplus"         
#> [121] "mipo"              "mira"              "mixed"            
#> [124] "MixMod"            "mixor"             "mle"              
#> [127] "mle2"              "mlm"               "mlogit"           
#> [130] "mmlogit"           "multinom"          "negbinirr"        
#> [133] "negbinmfx"         "ols"               "orm"              
#> [136] "plm"               "poissonirr"        "poissonmfx"       
#> [139] "polr"              "probitmfx"         "psm"              
#> [142] "Rchoice"           "ridgelm"           "riskRegression"   
#> [145] "rlm"               "rlmerMod"          "RM"               
#> [148] "rma"               "rma.uni"           "robmixglm"        
#> [151] "rq"                "rqs"               "rqss"             
#> [154] "scam"              "sem"               "speedglm"         
#> [157] "speedlm"           "stanmvreg"         "stanreg"          
#> [160] "summary.lm"        "survfit"           "survreg"          
#> [163] "svy_vglm"          "svyglm"            "svyolr"           
#> [166] "tobit"             "truncreg"          "vgam"             
#> [169] "vglm"              "wbgee"             "wblm"             
#> [172] "wbm"               "zcpglm"            "zeroinfl"         
#> [175] "zerotrunc"
```

-   **Didn’t find a model?** [File an
    issue](https://github.com/easystats/insight/issues) and request
    additional model-support in *insight*!

## Credits

If this package helped you, please consider citing as follows:

Lüdecke D, Waggoner P, Makowski D. insight: A Unified Interface to
Access Information from Model Objects in R. Journal of Open Source
Software 2019;4:1412. doi:
[10.21105/joss.01412](https://doi.org/10.21105/joss.01412)
