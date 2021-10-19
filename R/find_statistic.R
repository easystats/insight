#' @title Find statistic for model
#' @description Returns the statistic for a regression model (*t*-statistic,
#'   *z*-statistic, etc.).
#' @name find_statistic
#'
#' @description Small helper that checks if a model is a regression model
#'   object and return the statistic used.
#'
#' @param x An object.
#' @param ... Currently not used.
#'
#' @return A character describing the type of statistic. If there is no
#'   statistic available with a distribution, `NULL` will be returned.
#'
#' @examples
#' # regression model object
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' find_statistic(m)
#' @export
find_statistic <- function(x, ...) {

  # model object check --------------------------------------------------------

  # check if the object is a model object; if not, quit early
  if (isFALSE(is_model(x))) {
    stop("The entered object is not a model object.", call. = FALSE)
  }

  if (inherits(x, "mipo")) {
    x <- tryCatch(
      {
        models <- eval(x$call$object)
        x <- models$analyses[[1]]
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (inherits(x, "mira")) {
    x <- x$analyses[[1]]
  }

  if (inherits(x, "model_fit")) {
    x <- x$fit
  }

  if (inherits(x, "merModList")) {
    x <- x[[1]]
  }


  # check if model object is accessible; if not, quit early
  if (is.null(x)) {
    return(NULL)
  }

  # t-value objects ----------------------------------------------------------

  t.mods <- c(
    "bayesx", "BBreg", "BBmm", "bcplm", "biglm", "blmerMod",
    "cch", "censReg", "complmrob", "cpglm", "cpglmm", "crq", "crqs",
    "drc",
    "elm",
    "feis", "felm",
    "gamlss", "garch", "glmmPQL", "gls", "gmm", "gnls",
    "HLfit",
    "ivreg", "ivFixed", "iv_robust", "ivprobit",
    "lm", "lm_robust", "lm.beta", "lme", "lmerMod", "lmerModLmerTest",
    "lmodel2", "lmRob", "lmrob", "lqm", "lqmm",
    "maxLik", "mixed", "mhurdle", "mlm", "multinom",
    "nlmerMod", "nlrq", "nls",
    "ols", "orcutt",
    "pb1", "pb2", "polr",
    "rlm", "rms", "rlmerMod", "rq", "rqs", "rqss",
    "selection", "speedlm", "spml", "summary.lm", "svyglm", "svyolr", "systemfit",
    "truncreg",
    "varest",
    "wbm", "wblm",
    "yuen"
  )

  # z-value objects ----------------------------------------------------------

  z.mods <- c(
    "aareg", "Arima", "averaging",
    "betamfx", "betaor", "betareg", "bife", "bifeAPEs", "bglmerMod",
    "boot_test_mediation", "bracl", "brglm", "brglmFit", "brmultinom", "btergm",
    "cglm", "cph", "clm", "clm2", "clmm", "clmm2", "clogit", "coxme", "coxph",
    "coxr", "crch", "crr",
    "DirichletRegModel",
    "ergm",
    "feglm", "flexsurvreg",
    "gee", "glimML", "glmm", "glmmadmb", "glmmFit", "glmmLasso", "glmmTMB",
    "glmx", "gmnl",
    "hurdle",
    "lavaan", "loggammacenslmrob", "logitmfx", "logitor", "LORgee", "lrm",
    "margins", "metaplus", "mixor", "MixMod", "mjoint", "mle", "mle2", "mlogit",
    "mclogit", "mmclogit", "mvmeta", "mvord",
    "negbin", "negbinmfx", "negbinirr", "nlreg",
    "objectiveML", "orm",
    "poissonmfx", "poissonirr", "psm", "probitmfx", "pgmm",
    "qr", "QRNLMM", "QRLMM",
    "Rchoice", "riskRegression", "robmixglm", "rma", "rma.mv", "rma.uni", "rrvglm",
    "Sarlm", "sem", "SemiParBIV", "slm", "survreg", "svy_vglm",
    "test_mediation", "tobit",
    "vglm",
    "wbgee",
    "zcpglm", "zeroinfl", "zerotrunc"
  )

  # F-value objects ----------------------------------------------------------

  f.mods <- c(
    "afex_aov", "Anova.mlm", "aov", "aovlist", "anova",
    "Gam",
    "manova", "maov",
    "t1way"
  )

  # chi-squared value objects ------------------------------------------------

  chi.mods <- c(
    "coxph.penal",
    "epi.2by2",
    "geeglm",
    "logistf",
    "MANOVA", "mlma",
    "nparLD",
    "RM",
    "vgam"
  )

  # mixed bag ----------------------------------------------------------------

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <-
    c(
      "bam", "bigglm",
      "cgam", "cgamm",
      "eglm", "emmGrid", "emm_list",
      "fixest",
      "gam", "glm", "Glm", "glmc", "glmerMod", "glmRob", "glmrob",
      "pseudoglm",
      "scam",
      "speedglm"
    )

  # t-statistic (otherwise z-statistic: "binomial", "poisson")
  g.t.mods <-
    c(
      "gaussian", "Gamma",
      "quasi", "quasibinomial", "quasipoisson",
      "inverse.gaussian"
    )

  # pattern finding ----------------------------------------------------------

  unclear.mods <- c("plm")

  if (inherits(x, "glht")) {
    if (x$df == 0) {
      z.mods <- c(z.mods, "glht")
    } else {
      t.mods <- c(t.mods, "glht")
    }
  }

  if (inherits(x, "coeftest")) {
    if ("z value" %in% dimnames(x)[[2]]) {
      z.mods <- c(z.mods, "coeftest")
    } else {
      t.mods <- c(t.mods, "coeftest")
    }
  }

  # no statistic -------------------------------------------------------------

  unsupported.mods <- c(
    "bcplm", "BFBayesFactor", "brmsfit",
    "gbm", "glmmEP",
    "joint",
    "list",
    "MCMCglmm", "mediate", "mlergm",
    "pairwise.htest",
    "ridgelm",
    "splmm", "stanreg", "stanmvreg", "survfit"
  )

  # edge cases ---------------------------------------------------------------

  m_info <- model_info(x, return_family_only = TRUE)

  # tweedie-check needs to come first, because glm can also have tweedie
  # family, so this exception needs to be caught before checking for g.mods

  tryCatch(
    {
      suppressWarnings(
        if (!is_multivariate(x) && .is_tweedie(x, m_info)) {
          return("t-statistic")
        }
      )
    },
    error = function(e) {}
  )

  # statistic check -----------------------------------------------------------

  model_class <- class(x)[[1]]

  if (model_class %in% unsupported.mods) {
    return(NULL)
  }

  if (model_class %in% t.mods) {
    return("t-statistic")
  }

  if (model_class %in% z.mods) {
    return("z-statistic")
  }

  if (model_class %in% f.mods) {
    return("F-statistic")
  }

  if (model_class %in% chi.mods) {
    return("chi-squared statistic")
  }

  if (model_class %in% g.mods) {
    if (model_class %in% c("emmGrid", "emm_list")) {
      stat <- tryCatch(
        {
          df <- get_df(x)
          if (all(is.na(df)) || all(is.infinite(df))) {
            "z-statistic"
          } else {
            "t-statistic"
          }
        },
        error = function(e) {
          "t-statistic"
        }
      )
      return(stat)
    } else if (m_info$family %in% g.t.mods) {
      return("t-statistic")
    } else {
      return("z-statistic")
    }
  }

  # ambiguous cases -----------------------------------------------------------

  if (model_class %in% unclear.mods) {
    col_names <- colnames(as.data.frame(summary(x)$coefficients))

    t_names <- c(
      "t",
      "t-value",
      "t value",
      "t.value",
      "Pr(>|t|)"
    )

    z_names <- c(
      "z",
      "z-value",
      "z value",
      "z.value",
      "Pr(>|z|)",
      "Pr(>|Z|)",
      "Naive z",
      "Robust z",
      "san.z",
      "Wald Z"
    )

    f_names <- c("F", "F-value", "F value", "F.value")

    chi_names <- c("Chisq", "chi-sq", "chi.sq", "Wald", "W", "Pr(>|W|)")

    if (length(col_names) == 0L) {
      return(NULL)
    }

    if (any(t_names %in% col_names)) {
      return("t-statistic")
    }

    if (any(z_names %in% col_names)) {
      return("z-statistic")
    }

    if (any(f_names %in% col_names)) {
      return("F-statistic")
    }

    if (any(chi_names %in% col_names)) {
      return("chi-squared statistic")
    }
  }
}





# helper ---------------

.is_tweedie <- function(model, info) {
  if (info$family %in% c("Student's-t", "t Family", "gaussian", "Gaussian") || grepl("(\\st)$", info$family)) {
    linear_model <- TRUE
  }
  tweedie_fam <- grepl("^(tweedie|Tweedie)", info$family) | grepl("^(tweedie|Tweedie)", info$link_function)
  (linear_model && tweedie_fam) || inherits(model, c("bcplm", "cpglm", "cpglmm", "zcpglm"))
}
