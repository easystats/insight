# Checks if a regression model object is supported by the insight package

Small helper that checks if a model is a *supported* (regression) model
object. `supported_models()` prints a list of currently supported model
classes.

## Usage

``` r
is_model_supported(x)

supported_models()
```

## Arguments

- x:

  An object.

## Value

A logical, `TRUE` if `x` is a (supported) model object.

## Details

This function returns `TRUE` if `x` is a model object that works with
the package's functions. A list of supported models can also be found
here: <https://github.com/easystats/insight>.

## Examples

``` r

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)

is_model_supported(m)
#> [1] TRUE
is_model_supported(mtcars)
#> [1] FALSE

# to see all supported models
supported_models()
#>   [1] "AKP"                     "Anova.mlm"              
#>   [3] "Arima"                   "BBmm"                   
#>   [5] "BBreg"                   "BFBayesFactor"          
#>   [7] "BGGM"                    "DirichletRegModel"      
#>   [9] "Gam"                     "Glm"                    
#>  [11] "HLfit"                   "LORgee"                 
#>  [13] "MANOVA"                  "MCMCglmm"               
#>  [15] "MixMod"                  "PMCMR"                  
#>  [17] "RM"                      "Rchoice"                
#>  [19] "Sarlm"                   "SemiParBIV"             
#>  [21] "aareg"                   "afex_aov"               
#>  [23] "anova.rms"               "aov"                    
#>  [25] "aovlist"                 "asym"                   
#>  [27] "averaging"               "bamlss"                 
#>  [29] "bamlss.frame"            "bayesQR"                
#>  [31] "bayesx"                  "bcplm"                  
#>  [33] "betamfx"                 "betaor"                 
#>  [35] "betareg"                 "bfsl"                   
#>  [37] "bife"                    "bifeAPEs"               
#>  [39] "bigglm"                  "biglm"                  
#>  [41] "blavaan"                 "blrm"                   
#>  [43] "bracl"                   "brglm"                  
#>  [45] "brmsfit"                 "brmultinom"             
#>  [47] "btergm"                  "censReg"                
#>  [49] "cgam"                    "cgamm"                  
#>  [51] "cglm"                    "clm"                    
#>  [53] "clm2"                    "clmm"                   
#>  [55] "clmm2"                   "clogit"                 
#>  [57] "coeftest"                "complmrob"              
#>  [59] "confusionMatrix"         "coxme"                  
#>  [61] "coxph"                   "coxph.penal"            
#>  [63] "coxph_weightit"          "coxr"                   
#>  [65] "cpglm"                   "cpglmm"                 
#>  [67] "crch"                    "crq"                    
#>  [69] "crqs"                    "crr"                    
#>  [71] "dep.effect"              "draws"                  
#>  [73] "drc"                     "eglm"                   
#>  [75] "elm"                     "emmGrid"                
#>  [77] "epi.2by2"                "ergm"                   
#>  [79] "estimate_contrasts"      "estimate_means"         
#>  [81] "estimate_slopes"         "externVar"              
#>  [83] "externX"                 "fdm"                    
#>  [85] "feglm"                   "feis"                   
#>  [87] "felm"                    "fitdistr"               
#>  [89] "fixest"                  "flac"                   
#>  [91] "flexsurvreg"             "flic"                   
#>  [93] "gam"                     "gamlss"                 
#>  [95] "gamm"                    "gamm4"                  
#>  [97] "garch"                   "gbm"                    
#>  [99] "gee"                     "geeglm"                 
#> [101] "ggcomparisons"           "glht"                   
#> [103] "glimML"                  "glm"                    
#> [105] "glmRob"                  "glm_weightit"           
#> [107] "glmerMod"                "glmgee"                 
#> [109] "glmm"                    "glmmPQL"                
#> [111] "glmmTMB"                 "glmmadmb"               
#> [113] "glmrob"                  "glmx"                   
#> [115] "gls"                     "gmnl"                   
#> [117] "hglm"                    "htest"                  
#> [119] "hurdle"                  "ivFixed"                
#> [121] "iv_robust"               "ivprobit"               
#> [123] "ivreg"                   "joint"                  
#> [125] "lavaan"                  "lcmm"                   
#> [127] "lm"                      "lmRob"                  
#> [129] "lm_robust"               "lme"                    
#> [131] "lmerMod"                 "lmerModLmerTest"        
#> [133] "lmodel2"                 "lmrob"                  
#> [135] "logistf"                 "logitmfx"               
#> [137] "logitor"                 "logitr"                 
#> [139] "lqm"                     "lqmm"                   
#> [141] "lrm"                     "manova"                 
#> [143] "marginaleffects"         "marginaleffects.summary"
#> [145] "margins"                 "maxLik"                 
#> [147] "mblogit"                 "mclogit"                
#> [149] "mcmc"                    "mcmc.list"              
#> [151] "mcp1"                    "mcp12"                  
#> [153] "mcp2"                    "med1way"                
#> [155] "mediate"                 "merMod"                 
#> [157] "merModList"              "meta_bma"               
#> [159] "meta_fixed"              "meta_random"            
#> [161] "metaplus"                "mhurdle"                
#> [163] "mipo"                    "mira"                   
#> [165] "mixed"                   "mixor"                  
#> [167] "mjoint"                  "mle"                    
#> [169] "mle2"                    "mlm"                    
#> [171] "mlogit"                  "mmclogit"               
#> [173] "mmlogit"                 "mmrm"                   
#> [175] "mmrm_fit"                "mmrm_tmb"               
#> [177] "model_fit"               "multinom"               
#> [179] "multinom_weightit"       "mvord"                  
#> [181] "negbinirr"               "negbinmfx"              
#> [183] "nestedLogit"             "nlme"                   
#> [185] "ols"                     "onesampb"               
#> [187] "oohbchoice"              "ordinal_weightit"       
#> [189] "orm"                     "pgmm"                   
#> [191] "phyloglm"                "phylolm"                
#> [193] "plm"                     "poissonirr"             
#> [195] "poissonmfx"              "polr"                   
#> [197] "probitmfx"               "psm"                    
#> [199] "ridgelm"                 "riskRegression"         
#> [201] "rjags"                   "rlm"                    
#> [203] "rlmerMod"                "rma"                    
#> [205] "rma.uni"                 "rms"                    
#> [207] "robmixglm"               "robtab"                 
#> [209] "rq"                      "rqs"                    
#> [211] "rqss"                    "rvar"                   
#> [213] "scam"                    "sdmTMB"                 
#> [215] "selection"               "sem"                    
#> [217] "semLm"                   "semLme"                 
#> [219] "seqanova.svyglm"         "serp"                   
#> [221] "slm"                     "speedglm"               
#> [223] "speedlm"                 "stanfit"                
#> [225] "stanmvreg"               "stanreg"                
#> [227] "summary.lm"              "survfit"                
#> [229] "survreg"                 "svy2lme"                
#> [231] "svy_vglm"                "svychisq"               
#> [233] "svycoxph"                "svyglm"                 
#> [235] "svyolr"                  "svysurvreg"             
#> [237] "systemfit"               "t1way"                  
#> [239] "tobit"                   "trimcibt"               
#> [241] "truncreg"                "vgam"                   
#> [243] "vglm"                    "wbgee"                  
#> [245] "wblm"                    "wbm"                    
#> [247] "wmcpAKP"                 "yuen"                   
#> [249] "yuend"                   "zcpglm"                 
#> [251] "zeroinfl"                "zerotrunc"              
```
