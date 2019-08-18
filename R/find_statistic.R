#' @title Find statistic for model
#' @description Returns the statistic for the a regression model (*t*-statistic,
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
#'   statistic available with a distribution, a `NULL` will be returned.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#'
#' find_statistic(m)
#' @export
find_statistic <- function(x, ...) {

  # model object check --------------------------------------------------------

  # check if the object is a model object; if so, quit early
  if (isFALSE(is_model(x))) {
    stop(message("The entered object is not a model object."), call. = FALSE)
    return(invisible(NULL))
  }

  # t-value objects ----------------------------------------------------------

  t.mods <-
    c(
      "biglm",
      "cch",
      "coeftest",
      "drc",
      "felm",
      "gam",
      "gamlss",
      "garch",
      "glmmPQL",
      "gls",
      "gmm",
      "ivreg",
      "lm",
      "lm.beta",
      "lmerMod",
      "lmRob",
      "lmrob",
      "mlm",
      "multinom",
      "nlmerMod",
      "nlrq",
      "nls",
      "orcutt",
      "plm",
      "polr",
      "rlm",
      "rlmerMod",
      "rq",
      "speedglm",
      "speedlm",
      "svyglm",
      "svyolr"
    )

  # z-value objects ----------------------------------------------------------

  z.mods <-
    c(
      "aareg",
      "clm",
      "clmm",
      "coxph",
      "ergm",
      "glmmadmb",
      "glmmTMB",
      "lavaan",
      "mjoint",
      "mle2",
      "mclogit",
      "mmclogit",
      "negbin",
      "psm",
      "survreg"
    )

  # F-value objects ----------------------------------------------------------

  f.mods <-
    c(
      "aov",
      "aovlist",
      "anova",
      "Gam",
      "manova"
    )

  # chi-squared value objects ------------------------------------------------

  chi.mods <-
    c(
      "vgam"
    )

  # mixed big ----------------------------------------------------------------

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <-
    c(
      "glm",
      "glmerMod",
      "glmRob",
      "glmrob"
    )

  # t-statistic
  g.t.mods <-
    c(
      "quasi",
      "gaussian",
      "quasibinomial",
      "quasipoisson",
      "Gamma",
      "inverse.gaussian"
    )

  # z-statistic
  g.z.mods <-
    c(
      "binomial",
      "poisson"
    )

  # no statistic -------------------------------------------------------------

  unsupported.mods <-
    c(
      "stanreg"
    )

  # statistic check -----------------------------------------------------------

  if (class(x)[[1]] %in% unsupported.mods) return(NULL)
  if (class(x)[[1]] %in% t.mods) return("t-statistic")
  if (class(x)[[1]] %in% z.mods) return("z-statistic")
  if (class(x)[[1]] %in% f.mods) return("F-statistic")
  if (class(x)[[1]] %in% chi.mods) return("chi-squared statistic")

  if (class(x)[[1]] %in% g.mods) {
    if (class(x)[[1]] == "glm" && summary(x)$family$family[[1]] %in% g.t.mods) {
      return("t-statistic")
    } else if (class(x)[[1]] == "glmerMod" && summary(x)$family[[1]] %in% g.t.mods) {
      return("t-statistic")
    } else {
      return("z-statistic")
    }
  }
}
