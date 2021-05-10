#' Standardize column names
#'
#' Standardize column names from data frames, in particular objects returned
#' from \code{\link[parameters:model_parameters]{model_parameters()}}, so column
#' names are consistent and the same for any model object.
#'
#' @param data A data frame. In particular, objects from \emph{easystats}
#'   package functions like
#'   \code{\link[parameters:model_parameters]{model_parameters()}} or
#'   \code{\link[effectsize:effectsize]{effectsize()}} are accepted, but also
#'   data frames returned by \code{broom::tidy()} are valid objects.
#' @param style Standardization can either be based on the naming conventions
#'   from the \href{https://easystats.github.io/easystats/}{easystats-project},
#'   or on \pkg{broom}'s naming scheme.
#' @param ignore_estimate Logical, if \code{TRUE}, column names like
#'   \code{"mean"} or \code{"median"} will \emph{not} be converted to
#'   \code{"Coefficient"} resp. \code{"estimate"}.
#' @param ... Currently not used.
#'
#' @return A data frame, with standardized column names.
#'
#' @details This method is in particular useful for package developers or users
#'   who use, e.g.,
#'   \code{\link[parameters:model_parameters]{model_parameters()}} in their own
#'   code or functions to retrieve model parameters for further processing. As
#'   \code{model_parameters()} returns a data frame with varying column names
#'   (depending on the input), accessing the required information is probably
#'   not quite straightforward. In such cases, \code{standardize_names()} can be
#'   used to get consistent, i.e. always the same column names, no matter what
#'   kind of model was used in \code{model_parameters()}.
#'   \cr \cr
#'   For \code{style = "broom"}, column names are renamed to match \pkg{broom}'s
#'   naming scheme, i.e. \code{Parameter} is renamed to \code{term},
#'   \code{Coefficient} becomes \code{estimate} and so on.
#'   \cr \cr
#'   For \code{style = "easystats"}, when \code{data} is an object from
#'   \code{broom::tidy()}, column names are converted from "broom"-style into
#'   "easystats"-style.
#'
#' @examples
#' if (require("parameters")) {
#'   model <- lm(mpg ~ wt + cyl, data = mtcars)
#'   mp <- model_parameters(model)
#'
#'   as.data.frame(mp)
#'   standardize_names(mp)
#'   standardize_names(mp, style = "broom")
#' }
#' @export
standardize_names <- function(data, ...) {
  UseMethod("standardize_names")
}


#' @export
standardize_names.default <- function(data, ...) {
  print_color(sprintf("Objects of class '%s' are currently not supported.\n", class(data)[1]), "red")
}



#' @rdname standardize_names
#' @export
standardize_names.parameters_model <- function(data,
                                               style = c("easystats", "broom"),
                                               ignore_estimate = FALSE,
                                               ...) {
  style <- match.arg(style)
  .standardize_names(data, style, ignore_estimate = ignore_estimate, ...)
}

#' @export
standardize_names.effectsize_table <- standardize_names.parameters_model

#' @export
standardize_names.data.frame <- standardize_names.parameters_model

#' @export
standardize_names.parameters_distribution <- standardize_names.parameters_model




# helper -----


.standardize_names <- function(data, style, ignore_estimate = FALSE, ...) {
  cn <- colnames(data)

  if (style == "easystats") {
    cn <- .names_to_easystats(cn, ignore_estimate)
  } else {
    cn <- .names_to_broom(cn, ignore_estimate)
  }

  colnames(data) <- cn
  as.data.frame(data)
}






.names_to_easystats <- function(cn, ignore_estimate) {
  cn[cn %in% c("t", "z", "F", "Chi2", "chisq", "Chisq", "chi-sq", "t / F", "z / Chisq", "z / Chi2", "W")] <- "Statistic"
  if (isFALSE(ignore_estimate)) {
    cn[cn %in% c("Median", "Mean", "MAP", "Dxy", "rho", "r", "tau", "Difference")] <- "Coefficient"
  }
  cn[cn %in% c("df_residual", "df.residual", "Resid..Df", "df.error", "df_error")] <- "df"

  # convert broom-style to easystats
  cn[cn == "term"] <- "Parameter"
  cn[cn == "estimate"] <- "Coefficient"
  cn[cn == "std.error"] <- "SE"
  cn[cn == "std.dev"] <- "SD"
  cn[cn == "p.value"] <- "p"
  cn[cn == "bayes.factor"] <- "BF"
  cn[cn == "component"] <- "Component"
  cn[cn == "effect"] <- "Effects"
  cn[cn == "response"] <- "Response"
  cn[cn == "statistic"] <- "Statistic"
  cn[cn == "conf.low"] <- "CI_low"
  cn[cn == "conf.high"] <- "CI_high"
  cn[cn == "conf.level"] <- "CI"
  cn[cn == "n.obs"] <- "n_Obs"
  # anova
  cn[cn == "sumsq"] <- "Sum_Squares"
  cn[cn == "meansq"] <- "Mean_Square"
  cn[cn == "Resid..Dev"] <- "Deviance_error"
  # convert classic summary
  cn[cn == "Estimate"] <- "Coefficient"
  cn[cn == "Std. Error"] <- "SE"
  cn[cn == "t value"] <- "Statistic"
  cn[cn == "z value"] <- "Statistic"
  cn[cn == "Pr(>|t|)"] <- "p"
  cn[cn == "Pr(>|z|)"] <- "p"

  cn
}



.names_to_broom <- function(cn, ignore_estimate) {
  # TO DO: currently `htest` object output naming differs from `broom`
  # needs further discussion

  # easy replacements
  cn[cn == "Parameter"] <- "term"
  cn[cn == "SE"] <- "std.error"
  cn[cn == "SD"] <- "std.dev"
  cn[cn == "p"] <- "p.value"
  cn[cn == "BF"] <- "bayes.factor"
  cn[cn == "Component"] <- "component"
  cn[cn == "Effects"] <- "effect"
  cn[cn == "Response"] <- "response"
  cn[cn == "CI"] <- "conf.level"
  cn[cn == "df_error"] <- "df.error"
  cn[cn == "df_residual"] <- "df.residual"
  cn[cn == "n_Obs"] <- "n.obs"

  # anova
  cn[cn == "Sum_Squares"] <- "sumsq"
  cn[cn == "Mean_Square"] <- "meansq"
  cn[cn == "Deviance_error"] <- "Resid..Dev"

  # name of coefficient column for (Bayesian) models
  if (isFALSE(ignore_estimate)) {
    cn[cn %in% c("Coefficient", "Std_Coefficient", "Median", "Mean", "MAP", "Psihat", "Trimmed_Mean")] <- "estimate"
  }

  # name of coefficient column htest
  cn[cn %in% c("Dxy", "rho", "r", "tau")] <- "estimate"
  if (("Difference" %in% cn) && !("estimate" %in% cn)) {
    cn[cn == "Difference"] <- "estimate"
  }

  cn[cn %in% c("W", "S", "t", "z", "F", "Chi2", "chisq", "chi-sq", "Chisq", "t / F", "z / Chisq", "z / Chi2")] <- "statistic"

  # fancy regex replacements
  cn <- gsub("^CI_low", "conf.low", cn)
  cn <- gsub("^CI_high", "conf.high", cn)
  cn <- gsub("(.*)CI_low$", "\\1conf.low", cn)
  cn <- gsub("(.*)CI_high$", "\\1conf.high", cn)

  # from package effectisze
  if (requireNamespace("effectsize", quietly = TRUE)) {
    effectsize_names <- effectsize::is_effectsize_name(cn)
    if (any(effectsize_names)) {
      cn[effectsize_names] <- "estimate"
    }
  }

  # convert classic summary
  cn[cn == "Std. Error"] <- "std.error"
  cn[cn == "t value"] <- "statistic"
  cn[cn == "z value"] <- "statistic"
  cn[cn == "Pr(>|t|)"] <- "p.value"
  cn[cn == "Pr(>|z|)"] <- "p.value"

  # lowercase for everything
  cn <- gsub(tolower(cn), pattern = "_", replacement = ".", fixed = TRUE)

  cn
}
