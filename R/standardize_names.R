#' Standardize column names
#'
#' Standardize column names from data frames, in particular objects returned from
#' \code{\link[parameters:model_parameters]{model_parameters()}}, so column names are
#' consistent and the same for any model object.
#'
#' @param data A data frame. In particular, objects from \emph{easystats} package functions like \code{\link[parameters:model_parameters]{model_parameters()}} or \code{\link[effectsize:effectsize]{effectsize()}} are accepted.
#' @param style Standardization can either be based on the naming conventions from the easystats project, or on \pkg{broom}'s naming scheme.
#' @param ... Currently not used.
#'
#' @return A data frame, with standardized column names.
#'
#' @details This method is in particular useful for package developers or users
#'   who use, e.g., \code{\link[parameters:model_parameters]{model_parameters()}} in their own
#'   code or functions to retrieve model parameters for further processing. As
#'   \code{model_parameters()} returns a data frame with varying column names
#'   (depending on the input), accessing the required information is probably
#'   not quite straightforward. In such cases, \code{standardize_names()} can
#'   be used to get consistent, i.e. always the same column names, no matter
#'   what kind of model was used in \code{model_parameters()}.
#'   \cr \cr
#'   For \code{style = "broom"}, column names are renamed to match \pkg{broom}'s
#'   naming scheme, i.e. \code{Parameter} is renamed to \code{term}, \code{Coefficient}
#'   becomes \code{estimate} and so on.
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
standardize_names.parameters_model <- function(data, style = c("easystats", "broom"), ...) {
  style <- match.arg(style)
  .standardize_names(data, style, ...)
}

#' @export
standardize_names.effectsize_table <- standardize_names.parameters_model




.standardize_names <- function(data, style, ...) {
  cn <- colnames(data)

  if (style == "easystats") {
    cn[cn %in% c("t", "z", "F", "chisq", "Chisq", "chi-sq", "t / F", "z / Chisq")] <- "Statistic"
    cn[cn %in% c("Median", "Mean", "MAP", "rho", "r", "tau", "Difference")] <- "Coefficient"
    cn[cn %in% c("df_residual", "df_error")] <- "df"
  } else {
    # easy replacements
    cn[cn == "Parameter"] <- "term"
    cn[cn == "SE"] <- "std.error"
    cn[cn == "SD"] <- "std.dev"
    cn[cn == "p"] <- "p.value"
    cn[cn == "BF"] <- "bayes.factor"
    cn[cn == "Component"] <- "component"
    cn[cn == "Effects"] <- "effect"
    cn[cn == "Response"] <- "response"
    cn[cn == "CI"] <- "ci.width"
    cn[cn == "df_error"] <- "den.df"
    cn[cn == "df_residual"] <- "res.df"
    # anova
    cn[cn == "Sum_Squares"] <- "sumsq"
    cn[cn == "Mean_Square"] <- "meansq"
    # name of coefficient column for (Bayesian) models
    cn[cn %in% c("Coefficient", "Std_Coefficient", "Median", "Mean", "MAP")] <- "estimate"
    # name of coefficient column htest
    cn[cn %in% c("rho", "r", "tau", "Difference")] <- "estimate"
    cn[cn %in% c("t", "z", "F", "chisq", "chi-sq", "Chisq", "t / F", "z / Chisq")] <- "statistic"
    # fancy regex replacements
    cn <- gsub("^CI_low", "conf.low", cn)
    cn <- gsub("^CI_high", "conf.high", cn)
    # from package effectisze
    if (requireNamespace("effectsize", quietly = TRUE)) {
      effectsize_names <- effectsize::is_effectsize_name(cn)
      if (any(effectsize_names)) {
        cn[effectsize_names] <- "estimate"
      }
    }
    # lowercase for everything
    cn <- tolower(cn)
  }

  colnames(data) <- cn
  as.data.frame(data)
}




#' @title Tidy methods for easystats-objects
#' @name tidy
#'
#' @description Tidy methods for easystats-objects. \code{tidy()} usually returns
#'   the same data frame that was used as input, however, with broom-alike
#'   column names.
#'
#' @param x A data frame, as returned by functions from easystats-packages like \code{\link[parameters:model_parameters]{model_parameters()}} or \code{\link[effectsize:effectsize]{effectsize()}}.
#' @param ... Currently not used.
#'
#' @return \code{x}, with "standardized" column names (see \code{\link{standardize_names}}).
#'
#' @note Due to possible namespace conflicts with other packages that define
#'   a generic \code{tidy()}-method, the lifecycle of this function is still
#'   experimental, and there might be a chance that it will become defunct and
#'   removed in a future update. If possible, the preferred and stable alternative
#'   to \code{tidy()} for \emph{easystats}-objects is \code{\link{standardize_names}}.
#'
#' @examples
#' if (require("parameters")) {
#'   model <- lm(mpg ~ wt + cyl, data = mtcars)
#'   mp <- model_parameters(model)
#'
#'   as.data.frame(mp)
#'   tidy(mp)
#' }
#' @export
tidy <- function(x, ...) {
  UseMethod("tidy")
}

#' @export
tidy.parameters_model <- function(x, ...) {
  standardize_names(x, style = "broom")
}

#' @export
tidy.effectsize_table <- tidy.parameters_model
