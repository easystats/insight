#' @title Parameter table formatting
#' @name format_table
#'
#' @description This functions takes a data frame with model parameters as input
#'   and formats certain columns into a more readable layout (like collapsing
#'   separate columns for lower and upper confidence interval values). Furthermore,
#'   column names are formatted as well.
#'
#' @param x A data frame of model's parameters, as returned by various functions of the \strong{easystats}-packages. May also be a result from \code{broom::tidy()}.
#' @param pretty_names Return "pretty" (i.e. more human readable) parameter names.
#' @param digits Number of decimal places for numeric values (except confidence intervals and p-values).
#' @param ci_width Minimum width of the returned string for confidence intervals. If not \code{NULL} and width is larger than the string's length, leading whitespaces are added to the string. If \code{width="auto"}, width will be set to the length of the longest string.
#' @param ci_brackets Logical, if \code{TRUE} (default), CI-values are encompassed in square brackets (else in parentheses).
#' @param ci_digits Number of decimal places for confidence intervals.
#' @param p_digits Number of decimal places for p-values. May also be \code{"scientific"} for scientific notation of p-values.
#' @param rope_digits Number of decimal places for the ROPE percentage values.
#' @param preserve_attributes Logical, if \code{TRUE}, preserves all attributes from the input data frame.
#' @inheritParams format_p
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' if (require("parameters")) {
#'   x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#'   as.data.frame(format_table(x))
#'   as.data.frame(format_table(x, p_digits = "scientific"))
#' }
#' \donttest{
#' if (require("rstanarm") && require("parameters")) {
#'   model <- stan_glm(Sepal.Length ~ Species, data = iris, refresh = 0, seed = 123)
#'   x <- model_parameters(model, ci = c(0.69, 0.89, 0.95))
#'   as.data.frame(format_table(x))
#' }}
#' @return A data frame.
#' @export
format_table <- function(x, pretty_names = TRUE, stars = FALSE, digits = 2, ci_width = "auto", ci_brackets = TRUE, ci_digits = 2, p_digits = 3, rope_digits = 2, preserve_attributes = FALSE, ...) {

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)
  if (missing(rope_digits)) rope_digits <- .additional_arguments(x, "rope_digits", 2)

  att <- attributes(x)
  x <- as.data.frame(x)


  # Format parameters names ----
  if (pretty_names & !is.null(att$pretty_names)) {
    # remove strings with NA names
    att$pretty_names <- att$pretty_names[!is.na(names(att$pretty_names))]
    if (length(att$pretty_names) != length(x$Parameter)) {
      match_pretty_names <- stats::na.omit(match(names(att$pretty_names), x$Parameter))
      if (length(match_pretty_names)) {
        x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
      }
    } else {
      match_pretty_names <- att$pretty_names[x$Parameter]
      if (!anyNA(match_pretty_names)) {
        x$Parameter <- att$pretty_names[x$Parameter]
      } else {
        match_pretty_names <- stats::na.omit(match(names(att$pretty_names), x$Parameter))
        if (length(match_pretty_names)) {
          x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
        }
      }
    }
  }


  # Format specific columns ----
  if ("n_Obs" %in% names(x)) x$n_Obs <- format_value(x$n_Obs, protect_integers = TRUE)
  if ("n_Missing" %in% names(x)) x$n_Missing <- format_value(x$n_Missing, protect_integers = TRUE)


  # Format df columns ----
  x <- .format_df_columns(x)


  # Format frequentist stats ----
  x <- .format_freq_stats(x)


  # P values ----
  x <- .format_p_values(x, stars = stars, p_digits = p_digits)


  # Main CI ----
  x <- .format_main_ci_columns(x, att, ci_digits, ci_width, ci_brackets)
  x <- .format_broom_ci_columns(x, ci_digits, ci_width, ci_brackets)


  # Other CIs ----
  out <- .format_other_ci_columns(x, att, ci_digits, ci_width, ci_brackets)
  x <- out$x
  other_ci_colname <- out$other_ci_colname


  # Misc / Effect Sizes
  names(x)[names(x) == "Cohens_d"] <- "Cohen's d"
  names(x)[names(x) == "Cramers_v"] <- "Cramer's V"
  names(x)[names(x) == "phi_adjusted"] <- "phi (adj.)"
  names(x)[names(x) == "Cramers_v_adjusted"] <- "Cramer's V (adj.)"


  # Standardized ----
  x <- .format_std_columns(x, other_ci_colname, digits)


  # Partial ----
  x[names(x)[grepl("_partial$", names(x))]] <- format_value(x[names(x)[grepl("_partial$", names(x))]])
  names(x)[grepl("_partial$", names(x))] <- paste0(gsub("_partial$", "", names(x)[grepl("_partial$", names(x))]), " (partial)")


  # metafor ----
  if ("Weight" %in% names(x)) x$Weight <- format_value(x$Weight, protect_integers = TRUE)


  # Bayesian ---
  x <- .format_bayes_columns(x, stars, rope_digits = rope_digits)


  # rename performance columns
  x <- .format_performance_columns(x)


  # Format remaining columns
  other_cols <- names(x)[sapply(x, is.numeric)]
  x[other_cols[other_cols %in% names(x)]] <- format_value(x[other_cols[other_cols %in% names(x)]], digits = digits)

  # SEM links
  if (all(c("To", "Operator", "From") %in% names(x))) {
    x$Link <- paste(x$To, x$Operator, x$From)

    col_position <- which(names(x) == "To")
    x <- x[c(names(x)[0:(col_position - 1)], "Link", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$To <- x$Operator <- x$From <- NULL
  }

  # restore attributes
  if (isTRUE(preserve_attributes)) {
    attributes(x) <- utils::modifyList(att, attributes(x))
  }

  x
}




#' @rdname format_table
#' @export
parameters_table <- format_table




# sub-routines ---------------


.format_p_values <- function(x, stars, p_digits) {
  if ("p" %in% names(x)) {
    x$p <- format_p(x$p, stars = stars, name = NULL, missing = "", digits = p_digits)
    x$p <- format(x$p, justify = "left")
  }
  if ("p.value" %in% names(x)) {
    x$p.value <- format_p(x$p.value, stars = stars, name = NULL, missing = "", digits = p_digits)
    x$p.value <- format(x$p.value, justify = "left")
  }

  for (stats in c("p_CochransQ", "p_Omnibus", "p_Chi2", "p_Baseline", "p_RMSEA", "p_ROPE", "Wu_Hausman_p", "Sargan_p")) {
    if (stats %in% names(x)) {
      x[[stats]] <- format_p(x[[stats]], stars = stars, name = NULL, missing = "", digits = p_digits)
      x[[stats]] <- format(x[[stats]], justify = "left")
      p_name <- gsub("(.*)_p$", "\\1", gsub("^p_(.*)", "\\1", stats))
      names(x)[names(x) == stats] <- paste0("p(", p_name, ")")
    }
  }
  x
}




.format_df_columns <- function(x) {
  # generic df
  if ("df" %in% names(x)) x$df <- format_value(x$df, protect_integers = TRUE)
  # residual df
  if ("df_residual" %in% names(x)) x$df_residual <- format_value(x$df_residual, protect_integers = TRUE)
  names(x)[names(x) == "df_residual"] <- "df"
  # df for errors
  if ("df_error" %in% names(x)) x$df_error <- format_value(x$df_error, protect_integers = TRUE)
  if (!("df" %in% names(x))) names(x)[names(x) == "df_error"] <- "df"
  # denominator and numerator df
  if ("df_num" %in% names(x)) x$df_num <- format_value(x$df_num, protect_integers = TRUE)
  if ("df_denom" %in% names(x)) x$df_denom <- format_value(x$df_denom, protect_integers = TRUE)
  x
}




#' @importFrom stats na.omit
.format_freq_stats <- function(x) {
  for (stats in c("t", "Chi2")) {
    if (stats %in% names(x) && "df" %in% names(x)) {
      df <- stats::na.omit(unique(x$df))
      if (length(df) == 1 && !all(is.infinite(df))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", df, ")")
        x$df <- NULL
      }
    }
  }

  for (stats in c("Baseline", "Chi2")) {
    df_col <- paste0(stats, "_df")
    if (stats %in% names(x) && df_col %in% names(x)) {
      df <- stats::na.omit(unique(x[[df_col]]))
      if (length(df) == 1 && !all(is.infinite(df))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", df, ")")
        x[[df_col]] <- NULL
      }
    }
  }

  if ("Success" %in% names(x)) x$Success <- format_value(x$Success, protect_integers = TRUE)
  if ("Trials" %in% names(x)) x$Trials <- format_value(x$Trials, protect_integers = TRUE)

  x
}




#' @importFrom stats na.omit
.format_main_ci_columns <- function(x, att, ci_digits, ci_width = "auto", ci_brackets = TRUE) {
  # Main CI
  ci_low <- names(x)[grep("^CI_low", names(x))]
  ci_high <- names(x)[grep("^CI_high", names(x))]
  if (length(ci_low) >= 1 & length(ci_low) == length(ci_high)) {
    if (!is.null(att$ci)) {
      if (length(unique(stats::na.omit(att$ci))) > 1) {
        ci_colname <- "?% CI"
      } else {
        ci_colname <- sprintf("%i%% CI", unique(stats::na.omit(att$ci))[1] * 100)
      }
    } else if (!is.null(x$CI)) {
      ci_colname <- sprintf("%i%% CI", unique(stats::na.omit(x$CI))[1] * 100)
      x$CI <- NULL
    } else {
      ci_colname <- "CI"
    }

    # Get characters to align the CI
    for (i in 1:length(ci_colname)) {
      x[ci_colname[i]] <- format_ci(x[[ci_low[i]]], x[[ci_high[i]]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets)
    }
    # Replace at initial position
    ci_position <- which(names(x) == ci_low[1])
    x <- x[c(names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname], ci_colname, names(x)[ci_position:(length(names(x)) - 1)][!names(x)[ci_position:(length(names(x)) - 1)] %in% ci_colname])]
    x <- x[!names(x) %in% c(ci_low, ci_high)]
  }

  x
}




#' @importFrom stats na.omit
.format_other_ci_columns <- function(x, att, ci_digits, ci_width = "auto", ci_brackets = TRUE) {
  other_ci_low <- names(x)[grep("_CI_low$", names(x))]
  other_ci_high <- names(x)[grep("_CI_high$", names(x))]
  if (length(other_ci_low) >= 1 & length(other_ci_low) == length(other_ci_high)) {
    other <- unlist(strsplit(other_ci_low, "_CI_low$"))

    # CI percentage
    if (length(other) == 1 && !is.null(att[[paste0("ci_", other)]])) {
      other_ci_colname <- sprintf("%s %i%% CI", other, unique(stats::na.omit(att[[paste0("ci_", other)]])) * 100)
    } else if (!is.null(att$ci)) {
      other_ci_colname <- sprintf("%s %i%% CI", other, unique(stats::na.omit(att$ci)) * 100)
    } else {
      other_ci_colname <- paste(other, "CI")
    }

    # Get characters to align the CI
    for (i in 1:length(other_ci_colname)) {
      x[[other_ci_low[i]]] <- format_ci(x[[other_ci_low[i]]], x[[other_ci_high[i]]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets)
      # rename lower CI into final CI column
      other_ci_position <- which(names(x) == other_ci_low[i])
      colnames(x)[other_ci_position] <- other_ci_colname[i]
      # remove upper CI column
      other_ci_position <- which(names(x) == other_ci_high[i])
      x[[other_ci_position]] <- NULL
    }
  } else {
    other_ci_colname <- c()
  }

  list(x = x, other_ci_colname = other_ci_colname)
}




.format_broom_ci_columns <- function(x, ci_digits, ci_width = "auto", ci_brackets = TRUE) {
  if (!any(grepl("conf.low", names(x), fixed = TRUE))) {
    return(x)
  }
  if (!any(grepl("conf.high", names(x), fixed = TRUE))) {
    return(x)
  }
  tryCatch(
    {
      ci_low <- names(x)[which(names(x) == "conf.low")]
      ci_high <- names(x)[which(names(x) == "conf.high")]
      x$conf.int <- format_ci(x[[ci_low]], x[[ci_high]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets)
      x$conf.low <- NULL
      x$conf.high <- NULL
      x
    },
    error = function(e) {
      x
    }
  )
}




.format_rope_columns <- function(x, ci_width = "auto", ci_brackets = TRUE) {
  if (all(c("ROPE_low", "ROPE_high") %in% names(x))) {
    x$ROPE_low <- format_ci(x$ROPE_low, x$ROPE_high, ci = NULL, width = ci_width, brackets = ci_brackets)
    x$ROPE_high <- NULL
    names(x)[names(x) == "ROPE_low"] <- "ROPE"
  }
  x
}




.format_std_columns <- function(x, other_ci_colname, digits) {
  std_cols <- names(x)[grepl("Std_", names(x))]
  if (length(std_cols) == 0) {
    return(x)
  }

  std_cis <- NULL

  if (!is.null(other_ci_colname)) {
    std_cis <- std_cols[std_cols %in% other_ci_colname]
    std_cols <- std_cols[!std_cols %in% other_ci_colname]
  }

  x[std_cols] <- format_value(x[std_cols], digits = digits)
  names(x)[names(x) == std_cols] <- .replace_words(std_cols, "Std_Coefficient", "Std. Coef.")
  names(x)[names(x) == std_cols] <- .replace_words(std_cols, "Std_Median", "Std. Median")
  names(x)[names(x) == std_cols] <- .replace_words(std_cols, "Std_Mean", "Std. Mean")
  names(x)[names(x) == std_cols] <- .replace_words(std_cols, "Std_MAP", "Std. MAP")

  if (!is.null(std_cis) && length(std_cis)) {
    # std_cis_replacement <- .replace_words(std_cis, "^Std_", "Std. ")
    std_cis_replacement <- gsub("^Std_Coefficient(.*)", "Std. Coef.\\1", std_cis)
    names(x)[names(x) == std_cis] <- std_cis_replacement
  }

  x
}




.format_bayes_columns <- function(x, stars, rope_digits = 2) {
  # Indices
  if ("BF" %in% names(x)) x$BF <- format_bf(x$BF, name = NULL, stars = stars)
  if ("pd" %in% names(x)) x$pd <- format_pd(x$pd, name = NULL, stars = stars)
  if ("Rhat" %in% names(x)) x$Rhat <- format_value(x$Rhat, digits = 3)
  if ("ESS" %in% names(x)) x$ESS <- format_value(x$ESS, protect_integers = TRUE)

  # ROPE
  if ("ROPE_Percentage" %in% names(x)) {
    x$ROPE_Percentage <- format_rope(x$ROPE_Percentage, name = NULL, digits = rope_digits)
    names(x)[names(x) == "ROPE_Percentage"] <- "% in ROPE"
  }
  x <- .format_rope_columns(x)

  # Priors
  if ("Prior_Location" %in% names(x)) x$Prior_Location <- format_value(x$Prior_Location, protect_integers = TRUE)
  if ("Prior_Scale" %in% names(x)) x$Prior_Scale <- format_value(x$Prior_Scale, protect_integers = TRUE)
  if ("Prior_Distribution" %in% names(x)) x$Prior_Distribution <- ifelse(is.na(x$Prior_Distribution), "", x$Prior_Distribution)
  if (all(c("Prior_Distribution", "Prior_Location", "Prior_Scale") %in% names(x))) {
    x$Prior <- paste0(
      .capitalize(x$Prior_Distribution),
      " (",
      x$Prior_Location,
      " +- ",
      x$Prior_Scale,
      ")"
    )
    x$Prior <- ifelse(x$Prior == " ( +- )", "", x$Prior) # Remove empty

    col_position <- which(names(x) == "Prior_Distribution")
    x <- x[c(names(x)[0:(col_position - 1)], "Prior", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- NULL
  }



  x
}




.format_performance_columns <- function(x) {
  if ("R2_adjusted" %in% names(x)) names(x)[names(x) == "R2_adjusted"] <- "R2 (adj.)"
  if ("R2_conditional" %in% names(x)) names(x)[names(x) == "R2_conditional"] <- "R2 (cond.)"
  if ("R2_marginal" %in% names(x)) names(x)[names(x) == "R2_marginal"] <- "R2 (marg.)"
  if ("R2_Tjur" %in% names(x)) names(x)[names(x) == "R2_Tjur"] <- "Tjur's R2"
  if ("R2_Nagelkerke" %in% names(x)) names(x)[names(x) == "R2_Nagelkerke"] <- "Nagelkerke's R2"
  if ("Performance_Score" %in% names(x)) names(x)[names(x) == "Performance_Score"] <- "Performance-Score"
  if ("Wu_Hausman" %in% names(x)) names(x)[names(x) == "Wu_Hausman"] <- "Wu & Hausman"
  if ("p(Wu_Hausman)" %in% names(x)) names(x)[names(x) == "p(Wu_Hausman)"] <- "p(Wu & Hausman)"
  x
}




# helper ---------------------


.replace_words <- function(x, target, replacement) {
  for (i in 1:length(x)) {
    if (grepl(target, x[i], fixed = TRUE)) {
      x[i] <- gsub(target, replacement, x[i])
    }
  }
  x
}




.additional_arguments <- function(x, value, default) {
  args <- attributes(x)$additional_arguments

  if (length(args) > 0 && value %in% names(args)) {
    out <- args[[value]]
  } else {
    out <- attributes(x)[[value]]
  }

  if (is.null(out)) {
    out <- default
  }

  out
}
