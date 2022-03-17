#' @title Parameter table formatting
#' @name format_table
#'
#' @description This functions takes a data frame with model parameters as input
#'   and formats certain columns into a more readable layout (like collapsing
#'   separate columns for lower and upper confidence interval values). Furthermore,
#'   column names are formatted as well. Note that `format_table()`
#'   converts all columns into character vectors!
#'
#' @param x A data frame of model's parameters, as returned by various functions
#'   of the **easystats**-packages. May also be a result from
#'   `broom::tidy()`.
#' @param pretty_names Return "pretty" (i.e. more human readable) parameter
#'   names.
#' @param digits,ci_digits,p_digits,rope_digits Number of digits for rounding or
#'   significant figures. May also be `"signif"` to return significant
#'   figures or `"scientific"` to return scientific notation. Control the
#'   number of digits by adding the value as suffix, e.g. `digits = "scientific4"`
#'   to have scientific notation with 4 decimal places, or `digits = "signif5"`
#'   for 5 significant figures (see also [signif()]).
#' @param ci_width Minimum width of the returned string for confidence
#'   intervals. If not `NULL` and width is larger than the string's length,
#'   leading whitespaces are added to the string. If `width="auto"`, width
#'   will be set to the length of the longest string.
#' @param ci_brackets Logical, if `TRUE` (default), CI-values are
#'   encompassed in square brackets (else in parentheses).
#' @param preserve_attributes Logical, if `TRUE`, preserves all attributes
#'   from the input data frame.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams format_p
#' @inheritParams format_value
#' @inheritParams get_data
#'
#' @seealso Vignettes [Formatting, printing and exporting tables](https://easystats.github.io/insight/articles/display.html)
#' and [Formatting model parameters](https://easystats.github.io/parameters/articles/model_parameters_formatting.html).
#'
#' @examples
#' format_table(head(iris), digits = 1)
#'
#' if (require("parameters")) {
#'   x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#'   as.data.frame(format_table(x))
#'   as.data.frame(format_table(x, p_digits = "scientific"))
#' }
#' \donttest{
#' if (require("rstanarm", warn.conflicts = FALSE) &&
#'   require("parameters", , warn.conflicts = FALSE)) {
#'   model <- stan_glm(Sepal.Length ~ Species, data = iris, refresh = 0, seed = 123)
#'   x <- model_parameters(model, ci = c(0.69, 0.89, 0.95))
#'   as.data.frame(format_table(x))
#' }
#' }
#' @return A data frame. Note that `format_table()` converts all columns
#' into character vectors!
#' @export
format_table <- function(x,
                         pretty_names = TRUE,
                         stars = FALSE,
                         digits = 2,
                         ci_width = "auto",
                         ci_brackets = TRUE,
                         ci_digits = 2,
                         p_digits = 3,
                         rope_digits = 2,
                         zap_small = FALSE,
                         preserve_attributes = FALSE,
                         verbose = TRUE,
                         ...) {


  # sanity check
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0)) {
    if (isTRUE(verbose)) {
      message("Can't format table, data frame is empty.")
    }
    return(NULL)
  }

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)
  if (missing(rope_digits)) rope_digits <- .additional_arguments(x, "rope_digits", 2)

  att <- attributes(x)
  x <- as.data.frame(x, stringsAsFactors = FALSE)


  # Format parameters names ----
  if (pretty_names && !is.null(att$pretty_names)) {
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


  # Format special anova columns ----
  x <- .format_aov_columns(x)


  # Format frequentist stats ----
  x <- .format_freq_stats(x)


  # P values ----
  x <- .format_p_values(x, stars = stars, p_digits = p_digits)


  # Main CI and Prediction Intervals ----
  x <- .format_main_ci_columns(x, att, ci_digits, ci_width, ci_brackets, zap_small)
  x <- .format_main_ci_columns(x, att, ci_digits, ci_width, ci_brackets, zap_small, ci_name = "PI")
  x <- .format_broom_ci_columns(x, ci_digits, ci_width, ci_brackets, zap_small)


  # Other CIs ----
  out <- .format_other_ci_columns(x, att, ci_digits, ci_width, ci_brackets, zap_small)
  x <- out$x
  other_ci_colname <- out$other_ci_colname


  # Misc / Effect Sizes
  names(x)[names(x) == "Cohens_d"] <- "Cohen's d"
  names(x)[names(x) == "Cramers_v"] <- "Cramer's V"
  names(x)[names(x) == "phi_adjusted"] <- "phi (adj.)"
  names(x)[names(x) == "r_rank_biserial"] <- "r (rank biserial)"
  names(x)[names(x) == "Cramers_v_adjusted"] <- "Cramer's V (adj.)"


  # Standardized ----
  x <- .format_std_columns(x, other_ci_colname, digits, zap_small)


  # Partial ----
  x[names(x)[grepl("_partial$", names(x))]] <- format_value(x[names(x)[grepl("_partial$", names(x))]], zap_small = zap_small)
  names(x)[grepl("_partial$", names(x))] <- paste0(gsub("_partial$", "", names(x)[grepl("_partial$", names(x))]), " (partial)")


  # metafor ----
  if ("Weight" %in% names(x)) x$Weight <- format_value(x$Weight, protect_integers = TRUE)


  # Bayesian ---
  x <- .format_bayes_columns(
    x,
    stars,
    rope_digits = rope_digits,
    zap_small = zap_small,
    ci_width = ci_width,
    ci_brackets = ci_brackets
  )


  # rename performance columns
  x <- .format_performance_columns(x)


  # Format remaining columns
  other_cols <- names(x)[sapply(x, is.numeric)]
  x[other_cols[other_cols %in% names(x)]] <- format_value(x[other_cols[other_cols %in% names(x)]], digits = digits, zap_small = zap_small)

  # SEM links
  if (all(c("To", "Operator", "From") %in% names(x))) {
    x$Link <- paste(x$To, x$Operator, x$From)

    col_position <- which(names(x) == "To")
    x <- x[c(names(x)[0:(col_position - 1)], "Link", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$To <- x$Operator <- x$From <- NULL
  }

  x[] <- lapply(x, as.character)

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

  for (stats in c("p_CochransQ", "p_Omnibus", "p_Chi2", "p_Baseline", "p_RMSEA", "p_ROPE", "p_MAP", "Wu_Hausman_p", "Sargan_p", "p_Omega2", "p_LR")) {
    if (stats %in% names(x)) {
      x[[stats]] <- format_p(x[[stats]], stars = stars, name = NULL, missing = "", digits = p_digits)
      x[[stats]] <- format(x[[stats]], justify = "left")
      p_name <- gsub("(.*)_p$", "\\1", gsub("^p_(.*)", "\\1", stats))
      names(x)[names(x) == stats] <- paste0("p (", p_name, ")")
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
  if ("df_error" %in% names(x)) {
    x$df_error <- format_value(x$df_error, protect_integers = TRUE)
    if (!("df" %in% names(x))) {
      names(x)[names(x) == "df_error"] <- "df"
    } else {
      names(x)[names(x) == "df_error"] <- "df (error)"
    }
  }
  # denominator and numerator df
  if ("df_num" %in% names(x)) {
    x$df_num <- format_value(x$df_num, protect_integers = TRUE)
    names(x)[names(x) == "df_num"] <- "df (num.)"
  }
  if ("df_denom" %in% names(x)) {
    x$df_denom <- format_value(x$df_denom, protect_integers = TRUE)
    names(x)[names(x) == "df_denom"] <- "df (denom.)"
  }
  x
}




.format_aov_columns <- function(x) {
  if ("Deviance_error" %in% names(x)) {
    x$Deviance_error <- format_value(x$Deviance_error, protect_integers = TRUE)
    names(x)[names(x) == "Deviance_error"] <- "Deviance (error)"
  }
  if ("Power" %in% names(x)) {
    x$Power <- format_value(x$Power, as_percent = TRUE, digits = 1)
  }
  x
}




.format_freq_stats <- function(x) {
  for (stats in c("t", "Chi2")) {
    if (stats %in% names(x) && "df" %in% names(x)) {
      if (is.character(x$df)) {
        x$df[x$df == ""] <- NA_character_
      }
      df <- stats::na.omit(unique(x$df))
      if (length(df) == 1 && !all(is.infinite(df))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", df, ")")
        x$df <- NULL
      }
    } else if (stats %in% names(x) && "df_error" %in% names(x)) {
      if (is.character(x$df_error)) {
        x$df_error[x$df_error == ""] <- NA_character_
      }
      df <- stats::na.omit(unique(x$df_error))
      if (length(df) == 1 && !all(is.infinite(df))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", df, ")")
        x$df_error <- NULL
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




.format_main_ci_columns <- function(x, att, ci_digits, ci_width = "auto", ci_brackets = TRUE, zap_small, ci_name = "CI") {
  # Main CI
  ci_low <- names(x)[grep(paste0("^", ci_name, "_low"), names(x))]
  ci_high <- names(x)[grep(paste0("^", ci_name, "_high"), names(x))]
  ci_value <- att[["ci"]]

  # CI or SI?
  ci_method <- att[["ci_method"]]

  if (length(ci_low) >= 1 && length(ci_low) == length(ci_high)) {
    if (!is.null(ci_value)) {
      if (length(unique(stats::na.omit(ci_value))) > 1) {
        ci_value <- unique(stats::na.omit(ci_value))
      } else {
        ci_value <- unique(stats::na.omit(ci_value))[1]
      }
    } else if (!is.null(x$CI)) {
      ci_value <- unique(stats::na.omit(x$CI))[1]
    } else {
      # all these edge cases... for some objects in "parameters::model_parameters()",
      # when we have multiple ci-levels, column names can be "CI_low_0.8" or
      # "CI_low_0.95" etc. - this is handled here, if we have no ci-attribute
      ptrn <- paste0("((?<=", ci_name, "_low_)|(?<=", ci_name, "_high_))\\d*\\.?\\d*")
      if (all(grepl(ptrn, ci_low, perl = TRUE)) && all(grepl(ptrn, ci_high, perl = TRUE))) {
        m <- regexpr(ptrn, ci_low, perl = TRUE)
        ci_value <- as.numeric(regmatches(ci_low, m))
      } else {
        ci_value <- NULL
      }
    }
    x$CI <- NULL

    if (is.null(ci_value)) {
      ci_colname <- ci_name
    } else if (isTRUE(tolower(ci_method) %in% "si")) {
      ci_colname <- sprintf("BF = %.5g SI", ci_value)
    } else {
      ci_colname <- sprintf("%g%% %s", ci_value * 100, ci_name)
    }


    # Get characters to align the CI
    for (i in 1:length(ci_colname)) {
      x[[ci_low[i]]] <- format_ci(x[[ci_low[i]]], x[[ci_high[i]]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets, zap_small = zap_small)
      # rename lower CI into final CI column
      ci_position <- which(names(x) == ci_low[i])
      colnames(x)[ci_position] <- ci_colname[i]
      # remove upper CI column
      ci_position <- which(names(x) == ci_high[i])
      x[[ci_position]] <- NULL
    }
  }


  x
}




.format_other_ci_columns <- function(x, att, ci_digits, ci_width = "auto", ci_brackets = TRUE, zap_small) {
  other_ci_low <- names(x)[grep("_CI_low$", names(x))]
  other_ci_high <- names(x)[grep("_CI_high$", names(x))]
  if (length(other_ci_low) >= 1 && length(other_ci_low) == length(other_ci_high)) {
    other <- unlist(strsplit(other_ci_low, "_CI_low$"))

    # CI percentage
    if (length(other) == 1 && !is.null(att[[paste0("ci_", other)]])) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(att[[paste0("ci_", other)]])) * 100)
    } else if (!is.null(att[["ci"]])) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(att[["ci"]])) * 100)
    } else if (length(other == 1) && paste0(other, "_CI") %in% colnames(x)) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(x[[paste0(other, "_CI")]])) * 100)
    } else {
      other_ci_colname <- paste(other, " CI")
    }

    # Get characters to align the CI
    for (i in 1:length(other_ci_colname)) {
      x[[other_ci_low[i]]] <- format_ci(x[[other_ci_low[i]]], x[[other_ci_high[i]]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets, zap_small = zap_small)
      # rename lower CI into final CI column
      other_ci_position <- which(names(x) == other_ci_low[i])
      colnames(x)[other_ci_position] <- other_ci_colname[i]
      # remove upper CI column
      other_ci_position <- which(names(x) == other_ci_high[i])
      x[[other_ci_position]] <- NULL
    }
    # remove columns with CI level
    for (i in other) {
      x[[paste0(i, "_CI")]] <- NULL
    }
  } else {
    other_ci_colname <- c()
  }

  list(x = x, other_ci_colname = other_ci_colname)
}




.format_broom_ci_columns <- function(x,
                                     ci_digits,
                                     ci_width = "auto",
                                     ci_brackets = TRUE,
                                     zap_small) {
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
      x$conf.int <- format_ci(x[[ci_low]], x[[ci_high]], ci = NULL, digits = ci_digits, width = ci_width, brackets = ci_brackets, zap_small = zap_small)
      x$conf.low <- NULL
      x$conf.high <- NULL
      x
    },
    error = function(e) {
      x
    }
  )
}




.format_rope_columns <- function(x, ci_width = "auto", ci_brackets = TRUE, zap_small) {
  if (all(c("ROPE_low", "ROPE_high") %in% names(x))) {
    x$ROPE_low <- format_ci(x$ROPE_low, x$ROPE_high, ci = NULL, width = ci_width, brackets = ci_brackets, zap_small = zap_small)
    x$ROPE_high <- NULL
    names(x)[names(x) == "ROPE_low"] <- "ROPE"
    x$ROPE_CI <- NULL
  }
  x
}




.format_std_columns <- function(x, other_ci_colname, digits, zap_small) {
  std_cols <- names(x)[grepl("Std_", names(x))]
  if (length(std_cols) == 0) {
    return(x)
  }

  std_cis <- NULL

  if (!is.null(other_ci_colname)) {
    std_cis <- std_cols[std_cols %in% other_ci_colname]
    std_cols <- std_cols[!std_cols %in% other_ci_colname]
  }

  x[std_cols] <- format_value(x[std_cols], digits = digits, zap_small = zap_small)
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




.format_bayes_columns <- function(x, stars, rope_digits = 2, zap_small, ci_width = "auto", ci_brackets = TRUE) {
  # Indices
  if ("BF" %in% names(x)) x$BF <- format_bf(x$BF, name = NULL, stars = stars)
  if ("log_BF" %in% names(x)) {
    x$BF <- format_bf(exp(x$log_BF), name = NULL, stars = stars)
    x$log_BF <- NULL
  }
  if ("pd" %in% names(x)) x$pd <- format_pd(x$pd, name = NULL, stars = stars)
  if ("Rhat" %in% names(x)) x$Rhat <- format_value(x$Rhat, digits = 3)
  if ("ESS" %in% names(x)) x$ESS <- round(x$ESS)

  if ("ROPE_Equivalence" %in% names(x)) names(x)[names(x) == "ROPE_Equivalence"] <- "Equivalence (ROPE)"
  if ("ROPE_Percentage" %in% names(x)) {
    x$ROPE_Percentage <- format_rope(x$ROPE_Percentage, name = NULL, digits = rope_digits)
    names(x)[names(x) == "ROPE_Percentage"] <- "% in ROPE"
  }
  x <- .format_rope_columns(x, ci_width, ci_brackets, zap_small)


  # Priors
  if ("Prior_Location" %in% names(x)) x$Prior_Location <- format_value(x$Prior_Location, protect_integers = TRUE)
  if ("Prior_Scale" %in% names(x)) x$Prior_Scale <- format_value(x$Prior_Scale, protect_integers = TRUE)
  if ("Prior_Distribution" %in% names(x)) x$Prior_Distribution <- ifelse(is.na(x$Prior_Distribution), "", x$Prior_Distribution)
  if ("Prior_df" %in% names(x)) x$Prior_df <- format_value(x$Prior_df, protect_integers = TRUE)
  if (all(c("Prior_Distribution", "Prior_df") %in% names(x))) {
    missing_df <- is.na(x$Prior_df) | x$Prior_df == ""
    x$Prior_Distribution[!missing_df] <- paste0(x$Prior_Distribution[!missing_df], " (df=", x$Prior_df[!missing_df], ")")
  }
  if (all(c("Prior_Distribution", "Prior_Location", "Prior_Scale") %in% names(x))) {
    x$Prior <- paste0(
      format_capitalize(x$Prior_Distribution),
      " (",
      x$Prior_Location,
      " +- ",
      x$Prior_Scale,
      ")"
    )
    x$Prior <- ifelse(x$Prior == " ( +- )", "", x$Prior) # Remove empty
    x$Prior <- trim_ws(gsub("( +- )", "", x$Prior, fixed = TRUE))

    col_position <- which(names(x) == "Prior_Distribution")
    x <- x[c(names(x)[0:(col_position - 1)], "Prior", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- x$Prior_df <- NULL
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
  if ("AIC_wt" %in% names(x)) names(x)[names(x) == "AIC_wt"] <- "AIC weights"
  if ("BIC_wt" %in% names(x)) names(x)[names(x) == "BIC_wt"] <- "BIC weights"
  if ("AICc_wt" %in% names(x)) names(x)[names(x) == "AICc_wt"] <- "AICc weights"
  if ("WAIC_wt" %in% names(x)) names(x)[names(x) == "WAIC_wt"] <- "WAIC weights"
  if ("LOOIC_wt" %in% names(x)) names(x)[names(x) == "LOOIC_wt"] <- "LOOIC weights"
  x
}




# helper ---------------------


.replace_words <- function(x, target, replacement) {
  for (i in 1:length(x)) {
    if (grepl(target, x[i], fixed = TRUE)) {
      x[i] <- gsub(target, replacement, x[i], fixed = TRUE)
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
