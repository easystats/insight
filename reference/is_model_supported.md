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
#> [183] "nestedLogit"             "ols"                    
#> [185] "onesampb"                "oohbchoice"             
#> [187] "ordinal_weightit"        "orm"                    
#> [189] "pgmm"                    "phyloglm"               
#> [191] "phylolm"                 "plm"                    
#> [193] "poissonirr"              "poissonmfx"             
#> [195] "polr"                    "probitmfx"              
#> [197] "psm"                     "ridgelm"                
#> [199] "riskRegression"          "rjags"                  
#> [201] "rlm"                     "rlmerMod"               
#> [203] "rma"                     "rma.uni"                
#> [205] "rms"                     "robmixglm"              
#> [207] "robtab"                  "rq"                     
#> [209] "rqs"                     "rqss"                   
#> [211] "rvar"                    "scam"                   
#> [213] "sdmTMB"                  "selection"              
#> [215] "sem"                     "semLm"                  
#> [217] "semLme"                  "seqanova.svyglm"        
#> [219] "serp"                    "slm"                    
#> [221] "speedglm"                "speedlm"                
#> [223] "stanfit"                 "stanmvreg"              
#> [225] "stanreg"                 "summary.lm"             
#> [227] "survfit"                 "survreg"                
#> [229] "svy2lme"                 "svy_vglm"               
#> [231] "svychisq"                "svycoxph"               
#> [233] "svyglm"                  "svyolr"                 
#> [235] "svysurvreg"              "systemfit"              
#> [237] "t1way"                   "tobit"                  
#> [239] "trimcibt"                "truncreg"               
#> [241] "vgam"                    "vglm"                   
#> [243] "wbgee"                   "wblm"                   
#> [245] "wbm"                     "wmcpAKP"                
#> [247] "yuen"                    "yuend"                  
#> [249] "zcpglm"                  "zeroinfl"               
#> [251] "zerotrunc"              
```
