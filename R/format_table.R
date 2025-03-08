#' @title Parameter table formatting
#' @name format_table
#'
#' @description This functions takes a data frame (usually with model
#'   parameters) as input and formats certain columns into a more readable
#'   layout (like collapsing separate columns for lower and upper confidence
#'   interval values). Furthermore, column names are formatted as well. Note
#'   that `format_table()` converts all columns into character vectors!
#'
#' @param x A data frame of model's parameters, as returned by various functions
#'   of the **easystats**-packages. May also be a result from
#'   `broom::tidy()`.
#' @param pretty_names Return "pretty" (i.e. more human readable) parameter
#'   names.
#' @param digits,ci_digits,p_digits,rope_digits,ic_digits Number of digits for
#'   rounding or significant figures. May also be `"signif"` to return significant
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
#' @param exact Formatting for Bayes factor columns, in case the provided data
#'   frame contains such a column (i.e. columns named `"BF"` or `"log_BF"`).
#'   For `exact = TRUE`, very large or very small values are then either reported
#'   with a scientific format (e.g., 4.24e5), else as truncated values (as "> 1000"
#'   and "< 1/1000").
#' @param use_symbols Logical, if `TRUE`, column names that refer to particular
#'   effectsizes (like Phi, Omega or Epsilon) include the related unicode-character
#'   instead of the written name. This only works on Windows for R >= 4.2, and on
#'   OS X or Linux for R >= 4.0. It is possible to define a global option for this
#'   setting, see 'Note'.
#' @param stars If `TRUE`, add significance stars (e.g., `p < .001***`). Can
#'   also be a character vector, naming the columns that should include stars
#'   for significant values. This is especially useful for Bayesian models,
#'   where we might have multiple columns with significant values, e.g. `BF`
#'   for the Bayes factor or `pd` for the probability of direction. In such
#'   cases, use `stars = c("pd", "BF")` to add stars to both columns, or
#'   `stars = "BF"` to only add stars to the Bayes factor and exclude the `pd`
#'   column. Currently, following columns are recognized: `"BF"`, `"pd"` and `"p"`.
#' @param stars_only If `TRUE`, return significant stars only (and no p-values).
#' @param select Determines which columns are printed and the table layout.
#' There are two options for this argument:
#'
#' * **A string expression with layout pattern**
#'
#'   `select` is a string with "tokens" enclosed in braces. These tokens will be
#'   replaced by their associated columns, where the selected columns will be
#'   collapsed into one column. Following tokens are replaced by the related
#'   coefficients or statistics: `{estimate}`, `{se}`, `{ci}` (or `{ci_low}` and
#'   `{ci_high}`), `{p}`, `{pd}` and `{stars}`. The token `{ci}` will be replaced
#'   by `{ci_low}, {ci_high}`. Example: `select = "{estimate}{stars} ({ci})"`
#'
#'   It is possible to create multiple columns as well. A `|` separates values
#'   into new cells/columns. Example: `select = "{estimate} ({ci})|{p}"`.
#'
#' * **A string indicating a pre-defined layout**
#'
#'   `select` can be one of the following string values, to create one of the
#'   following pre-defined column layouts:
#'
#'   - `"minimal"`: Estimates, confidence intervals and numeric p-values, in two
#'     columns. This is equivalent to `select = "{estimate} ({ci})|{p}"`.
#'   - `"short"`: Estimate, standard errors and numeric p-values, in two columns.
#'     This is equivalent to `select = "{estimate} ({se})|{p}"`.
#'   - `"ci"`: Estimates and confidence intervals, no asterisks for p-values.
#'     This is equivalent to `select = "{estimate} ({ci})"`.
#'   - `"se"`: Estimates and standard errors, no asterisks for p-values. This is
#'     equivalent to `select = "{estimate} ({se})"`.
#'   - `"ci_p"`: Estimates, confidence intervals and asterisks for p-values. This
#'     is equivalent to `select = "{estimate}{stars} ({ci})"`.
#'   - `"se_p"`: Estimates, standard errors and asterisks for p-values. This is
#'     equivalent to `select = "{estimate}{stars} ({se})"`..
#'
#' Using `select` to define columns will re-order columns and remove all columns
#' related to uncertainty (standard errors, confidence intervals), test statistics,
#' and p-values (and similar, like `pd` or `BF` for Bayesian models), because
#' these are assumed to be included or intentionally excluded when using `select`.
#' The new column order will be: Parameter columns first, followed by the "glue"
#' columns, followed by all remaining columns. If further columns should also be
#' placed first, add those as `focal_terms` attributes to `x`. I.e., following
#' columns are considers as "parameter columns" and placed first:
#' `c(easystats_columns("parameter"), attributes(x)$focal_terms)`.
#'
#' **Note:** glue-like syntax is still experimental in the case of more complex models
#' (like mixed models) and may not return expected results.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams format_p
#' @inheritParams format_value
#' @inheritParams get_data
#'
#' @seealso Vignettes
#' [Formatting, printing and exporting tables](https://easystats.github.io/insight/articles/display.html)
#' and [Formatting model parameters](https://easystats.github.io/parameters/articles/model_parameters_formatting.html).
#'
#' @note `options(insight_use_symbols = TRUE)` overrides the `use_symbols` argument
#'   and always displays symbols, if possible.
#' @examplesIf require("rstanarm", warn.conflicts = FALSE) && require("parameters", warn.conflicts = FALSE) && packageVersion("parameters") > "0.22.2"
#' format_table(head(iris), digits = 1)
#'
#' m <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
#' x <- parameters::model_parameters(m)
#' as.data.frame(format_table(x))
#' as.data.frame(format_table(x, p_digits = "scientific"))
#' # "glue" columns
#' as.data.frame(format_table(x, select = "minimal"))
#' as.data.frame(format_table(x, select = "{estimate}{stars}|{p}"))
#'
#' \donttest{
#' model <- rstanarm::stan_glm(
#'   Sepal.Length ~ Species,
#'   data = iris,
#'   refresh = 0,
#'   seed = 123
#' )
#' x <- parameters::model_parameters(model, ci = c(0.69, 0.89, 0.95))
#' as.data.frame(format_table(x))
#' }
#' @return A data frame. Note that `format_table()` converts all columns
#' into character vectors!
#' @export
format_table <- function(x,
                         pretty_names = TRUE,
                         stars = FALSE,
                         stars_only = FALSE,
                         digits = 2,
                         ci_width = "auto",
                         ci_brackets = TRUE,
                         ci_digits = digits,
                         p_digits = 3,
                         rope_digits = digits,
                         ic_digits = 1,
                         zap_small = FALSE,
                         preserve_attributes = FALSE,
                         exact = TRUE,
                         use_symbols = getOption("insight_use_symbols", FALSE),
                         select = NULL,
                         verbose = TRUE,
                         ...) {
  # validation check
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0)) {
    if (isTRUE(verbose)) {
      format_alert("Can't format table, data frame is empty.")
    }
    return(NULL)
  }

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", digits)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)
  if (missing(rope_digits)) rope_digits <- .additional_arguments(x, "rope_digits", digits)
  if (missing(ic_digits)) ic_digits <- .additional_arguments(x, "ic_digits", 1)

  # find name of coefficient, if present
  coef_column_name <- attributes(x)$coef_name
  # create p_stars, needed for glue
  if (any(c("p", "p.value") %in% colnames(x))) {
    p_col <- ifelse("p" %in% colnames(x), "p", "p.value")
    p_stars <- format_p(x[[p_col]], stars = TRUE, stars_only = TRUE)
  } else {
    p_stars <- NULL
  }

  att <- attributes(x)
  x <- as.data.frame(x, stringsAsFactors = FALSE)


  # Format parameters names ----
  if (pretty_names && !is.null(att$pretty_names)) {
    shared <- intersect(x$Parameter, names(att$pretty_names))
    index <- match(shared, x$Parameter)
    x$Parameter[index] <- as.vector(att$pretty_names[x$Parameter[index]])
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
  x <- .format_p_values(x, stars = stars, p_digits = p_digits, stars_only = stars_only)


  # Main CI and Prediction Intervals ----
  x <- .format_main_ci_columns(x, att, ci_digits, zap_small, ci_width, ci_brackets)
  x <- .format_main_ci_columns(x, att, ci_digits, zap_small, ci_width, ci_brackets, ci_name = "PI")
  x <- .format_broom_ci_columns(x, ci_digits, zap_small, ci_width, ci_brackets)


  # Misc / Effect Sizes
  x <- .format_effectsize_columns(x, use_symbols)

  # Other CIs ----
  out <- .format_other_ci_columns(x, att, ci_digits, zap_small, ci_width, ci_brackets)
  x <- out$x
  other_ci_colname <- out$other_ci_colname


  # Standardized ----
  x <- .format_std_columns(x, other_ci_colname, digits, zap_small)


  # Partial ----
  x[names(x)[endsWith(names(x), "_partial")]] <- format_value(
    x[names(x)[endsWith(names(x), "_partial")]],
    zap_small = zap_small
  )
  names(x)[endsWith(names(x), "_partial")] <- paste0(
    gsub("_partial$", "", names(x)[endsWith(names(x), "_partial")]),
    " (partial)"
  )


  # metafor ----
  if ("Weight" %in% names(x)) x$Weight <- format_value(x$Weight, protect_integers = TRUE)


  # Bayesian ---
  x <- .format_bayes_columns(
    x,
    stars = stars,
    rope_digits = rope_digits,
    zap_small = zap_small,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    exact = exact
  )


  # rename performance columns
  x <- .format_performance_columns(x, digits, ic_digits, zap_small, use_symbols)


  # format symbols in column names, if any left
  x <- .format_symbols(x, use_symbols)


  # Format remaining columns
  other_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  x[other_cols[other_cols %in% names(x)]] <- format_value(
    x[other_cols[other_cols %in% names(x)]],
    digits = digits,
    zap_small = zap_small
  )

  # SEM links
  if (all(c("To", "Operator", "From") %in% names(x))) {
    x$Link <- paste(x$To, x$Operator, x$From)

    col_position <- which(names(x) == "To")
    # Replace at initial position
    x <- x[c(names(x)[0:(col_position - 1)], "Link", names(x)[col_position:(length(names(x)) - 1)])]
    x$To <- x$Operator <- x$From <- NULL
  }

  x[] <- lapply(x, as.character)

  # apply glue-styled formatting
  if (!is.null(select)) {
    x <- .format_glue_table(
      x,
      style = select,
      coef_column = coef_column_name,
      p_stars = p_stars,
      ...
    )
  }

  # restore attributes
  if (isTRUE(preserve_attributes)) {
    attributes(x) <- utils::modifyList(att, attributes(x))
  }
  x
}


# sub-routines ---------------


# Format various p-values, coming from different easystats-packages
# like bayestestR (p_ROPE, p_MAP) or performance (p_Chi2)

.format_p_values <- function(x, p_digits, stars = FALSE, stars_only = FALSE) {
  # Specify stars for which column (#656)
  if (is.character(stars)) {
    starlist <- list(p = FALSE)
    starlist[stars] <- TRUE
  } else {
    starlist <- list(p = stars)
  }

  for (pv in c("p", "p.value", "SGPV")) {
    if (pv %in% names(x)) {
      x[[pv]] <- format_p(
        x[[pv]],
        stars = starlist[["p"]],
        stars_only = stars_only,
        name = NULL,
        missing = "",
        digits = p_digits
      )
      x[[pv]] <- format(x[[pv]], justify = "left")
    }
  }

  for (stats in c(
    "p_CochransQ", "p_Omnibus", "p_Chi2", "p_Baseline", "p_RMSEA", "p_ROPE",
    "p_MAP", "Wu_Hausman_p", "Sargan_p", "p_Omega2", "p_LR", "p_calibrated",
    "weak_instruments_p"
  )) {
    if (stats %in% names(x)) {
      x[[stats]] <- format_p(
        x[[stats]],
        stars = starlist[["p"]],
        stars_only = stars_only,
        name = NULL,
        missing = "",
        digits = p_digits
      )
      x[[stats]] <- format(x[[stats]], justify = "left")
      p_name <- gsub("(.*)_p$", "\\1", gsub("^p_(.*)", "\\1", stats))
      names(x)[names(x) == stats] <- paste0("p (", p_name, ")")
    }
  }
  x
}


# Format df-columns. We have df's for errors, numerator and denominator
# However, if df and df-error present, we have two different df-column names,
# but df-error becomes "df" is no df-column present. This is taken care of in
# this function.

.format_df_columns <- function(x) {
  # generic df
  if ("df" %in% names(x)) x$df <- format_value(x$df, protect_integers = TRUE)
  # residual df
  if ("df_residual" %in% names(x)) x$df_residual <- format_value(x$df_residual, protect_integers = TRUE)
  names(x)[names(x) == "df_residual"] <- "df"
  # df for errors
  if ("df_error" %in% names(x)) {
    x$df_error <- format_value(x$df_error, protect_integers = TRUE)
    if ("df" %in% names(x)) {
      names(x)[names(x) == "df_error"] <- "df (error)"
    } else {
      names(x)[names(x) == "df_error"] <- "df"
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


# formatting for various effect size columns. This function also checks if
# unicode for special characters is available, and if so, use these for
# column name formatting

.format_effectsize_columns <- function(x, use_symbols) {
  names(x)[names(x) == "Cohens_d"] <- "Cohen's d"
  names(x)[names(x) == "Cohens_d_CI_low"] <- "Cohen's d_CI_low"
  names(x)[names(x) == "Cohens_d_CI_high"] <- "Cohen's d_CI_high"
  names(x)[names(x) == "Cohens_w"] <- "Cohen's w"
  names(x)[names(x) == "Cohens_h"] <- "Cohen's h"
  names(x)[names(x) == "Cohens_g"] <- "Cohen's g"
  names(x)[names(x) == "Cohens_f"] <- "Cohen's f"
  names(x)[names(x) == "Cohens_f_partial"] <- "Cohen's f (partial)"
  names(x)[names(x) == "Cohens_d_robust"] <- "Robust Cohen's d"
  names(x)[names(x) == "Cohens_d_robust_CI_low"] <- "Robust Cohen's d_CI_low"
  names(x)[names(x) == "Cohens_d__robust_CI_high"] <- "Robust Cohen's d_CI_high"
  names(x)[names(x) == "d_partial"] <- "d (partial)"
  names(x)[names(x) == "d_marginal"] <- "d (marginal)"
  names(x)[names(x) == "Cramers_v"] <- "Cramer's V"
  names(x)[names(x) == "Cramers_v_adjusted"] <- "Cramer's V (adj.)"
  names(x)[names(x) == "r_rank_biserial"] <- "r (rank biserial)"
  names(x)[names(x) == "Hedges_g"] <- "Hedges' g"
  names(x)[names(x) == "Hedges_g_CI_low"] <- "Hedges' g_CI_low"
  names(x)[names(x) == "Hedges_g_CI_high"] <- "Hedges' g_CI_high"
  names(x)[names(x) == "Mahalanobis_D"] <- "Mahalanobis' D"
  names(x)[names(x) == "Pearsons_c"] <- "Pearson's C"
  names(x)[names(x) == "Kendalls_W"] <- "Kendall's W"
  names(x)[names(x) == "Odds_ratio"] <- "Odds ratio"
  names(x)[names(x) == "log_Odds_ratio"] <- "log(Odds ratio)"
  names(x)[names(x) == "Risk_ratio"] <- "Risk ratio"
  names(x)[names(x) == "log_Risk_ratio"] <- "log(Risk ratio)"

  # we can use unicode symbols
  if (isTRUE(use_symbols) && .unicode_symbols()) {
    names(x)[names(x) == "Glass_delta"] <- "Glass' \u0394"
    names(x)[names(x) == "phi"] <- "\u03D5"
    names(x)[names(x) == "phi_adjusted"] <- "\u03D5 (adj.)"
    names(x)[names(x) == "Fei"] <- "\u05E4\u200E"
    names(x)[names(x) == "Eta2"] <- "\u03B7\u00b2"
    names(x)[names(x) == "Eta2_partial"] <- "\u03B7\u00b2 (partial)"
    names(x)[names(x) == "Eta2_generalized"] <- "\u03B7\u00b2 (generalized)"
    names(x)[names(x) == "Epsilon2"] <- "\u03b5\u00b2"
    names(x)[names(x) == "Epsilon2_partial"] <- "\u03b5\u00b2 (partial)"
    names(x)[names(x) == "Omega2"] <- "\u03C9\u00b2"
    names(x)[names(x) == "Omega2_partial"] <- "\u03C9\u00b2 (partial)"
    names(x)[names(x) == "Cohens_f2"] <- "Cohen's f\u00b2"
    names(x)[names(x) == "Cohens_f2"] <- "Cohen's f\u00b2 (partial)"
    names(x)[names(x) == "rank_epsilon_squared"] <- "\u03B5\u00b2(R)"
    names(x)[names(x) == "rank_eta_squared"] <- "\u03B7\u00b2(H)"
  } else {
    names(x)[names(x) == "Glass_delta"] <- "Glass' delta"
    names(x)[names(x) == "phi"] <- "Phi"
    names(x)[names(x) == "phi_adjusted"] <- "Phi (adj.)"
    names(x)[names(x) == "Fei"] <- "Fei"
    names(x)[names(x) == "Eta2"] <- "Eta2"
    names(x)[names(x) == "Eta2_partial"] <- "Eta2 (partial)"
    names(x)[names(x) == "Eta2_generalized"] <- "Eta2 (generalized)"
    names(x)[names(x) == "Epsilon2"] <- "Epsilon2"
    names(x)[names(x) == "Epsilon2_partial"] <- "Epsilon2 (partial)"
    names(x)[names(x) == "Omega2"] <- "Omega2"
    names(x)[names(x) == "Omega2_partial"] <- "Omega2 (partial)"
    names(x)[names(x) == "Cohens_f2"] <- "Cohen's f2"
    names(x)[names(x) == "Cohens_f2_partial"] <- "Cohen's f2 (partial)"
    names(x)[names(x) == "rank_epsilon_squared"] <- "Epsilon2 (rank)"
    names(x)[names(x) == "rank_eta_squared"] <- "Eta2 (rank)"
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
        x$df[x$df == ""] <- NA_character_ # nolint
      }
      dof <- unique(x$df)
      dof <- dof[!is.na(dof)]
      if (length(dof) == 1 && !all(is.infinite(dof))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", dof, ")")
        x$df <- NULL
      }
    } else if (stats %in% names(x) && "df_error" %in% names(x)) {
      if (is.character(x$df_error)) {
        x$df_error[x$df_error == ""] <- NA_character_ # nolint
      }
      dof <- unique(x$df_error)
      dof <- dof[!is.na(dof)]
      if (length(dof) == 1 && !all(is.infinite(dof))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", dof, ")")
        x$df_error <- NULL
      }
    }
  }

  for (stats in c("Baseline", "Chi2")) {
    df_col <- paste0(stats, "_df")
    if (stats %in% names(x) && df_col %in% names(x)) {
      dof <- unique(x[[df_col]])
      dof <- dof[!is.na(dof)]
      if (length(dof) == 1 && !all(is.infinite(dof))) {
        names(x)[names(x) == stats] <- paste0(stats, "(", dof, ")")
        x[[df_col]] <- NULL
      }
    }
  }

  if ("Success" %in% names(x)) x$Success <- format_value(x$Success, protect_integers = TRUE)
  if ("Trials" %in% names(x)) x$Trials <- format_value(x$Trials, protect_integers = TRUE)

  x
}


.format_main_ci_columns <- function(x,
                                    att,
                                    ci_digits,
                                    zap_small,
                                    ci_width = "auto",
                                    ci_brackets = TRUE,
                                    ci_name = "CI") {
  # Main CI
  ci_low <- names(x)[grep(paste0("^", ci_name, "_low"), names(x))]
  ci_high <- names(x)[grep(paste0("^", ci_name, "_high"), names(x))]
  ci_value <- att[["ci"]]

  # CI or SI?
  ci_method <- att[["ci_method"]]

  if (length(ci_low) >= 1 && length(ci_low) == length(ci_high)) {
    if (!is.null(ci_value)) {
      ci_value <- ci_value[!is.na(ci_value)]
      if (n_unique(ci_value) > 1L) {
        ci_value <- unique(ci_value)
      } else {
        ci_value <- unique(ci_value)[1]
      }
    } else if (is.null(x$CI)) {
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
    } else {
      ci_value <- unique(x$CI[!is.na(x$CI)])[1]
    }
    x$CI <- NULL

    if (is.null(ci_value)) {
      ci_colname <- ci_name
    } else if (isTRUE(tolower(ci_method) == "si")) {
      ci_colname <- sprintf("BF = %.5g SI", ci_value)
    } else {
      ci_colname <- sprintf("%g%% %s", ci_value * 100, ci_name)
    }


    # Get characters to align the CI
    for (i in seq_along(ci_colname)) {
      x[[ci_low[i]]] <- format_ci(
        x[[ci_low[i]]],
        x[[ci_high[i]]],
        ci = NULL,
        digits = ci_digits,
        width = ci_width,
        brackets = ci_brackets,
        zap_small = zap_small
      )
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


.format_other_ci_columns <- function(x, att, ci_digits, zap_small, ci_width = "auto", ci_brackets = TRUE) {
  other_ci_low <- names(x)[endsWith(names(x), "_CI_low")]
  other_ci_high <- names(x)[endsWith(names(x), "_CI_high")]
  if (length(other_ci_low) >= 1 && length(other_ci_low) == length(other_ci_high)) {
    other <- unlist(strsplit(other_ci_low, "_CI_low$"), use.names = FALSE)

    # CI percentage
    if (length(other) == 1 && !is.null(att[[paste0("ci_", other)]])) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(att[[paste0("ci_", other)]])) * 100)
    } else if (!is.null(att[["ci"]])) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(att[["ci"]])) * 100)
    } else if (length(other) == 1 && paste0(other, "_CI") %in% colnames(x)) {
      other_ci_colname <- sprintf("%s %g%% CI", other, unique(stats::na.omit(x[[paste0(other, "_CI")]])) * 100)
    } else {
      other_ci_colname <- paste(other, " CI")
    }

    # Get characters to align the CI
    for (i in seq_along(other_ci_colname)) {
      x[[other_ci_low[i]]] <- format_ci(
        x[[other_ci_low[i]]],
        x[[other_ci_high[i]]],
        ci = NULL,
        digits = ci_digits,
        width = ci_width,
        brackets = ci_brackets,
        zap_small = zap_small
      )
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
    other_ci_colname <- NULL
  }

  list(x = x, other_ci_colname = other_ci_colname)
}


.format_broom_ci_columns <- function(x,
                                     ci_digits,
                                     zap_small,
                                     ci_width = "auto",
                                     ci_brackets = TRUE) {
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
      x$conf.low <- format_ci(
        x[[ci_low]],
        x[[ci_high]],
        ci = NULL,
        digits = ci_digits,
        width = ci_width,
        brackets = ci_brackets,
        zap_small = zap_small
      )
      names(x)[names(x) == "conf.low"] <- "conf.int"
      x$conf.high <- NULL
      x
    },
    error = function(e) {
      x
    }
  )
}


.format_rope_columns <- function(x, zap_small, ci_width = "auto", ci_brackets = TRUE) {
  if (all(c("ROPE_low", "ROPE_high") %in% names(x))) {
    x$ROPE_low <- format_ci(
      x$ROPE_low,
      x$ROPE_high,
      ci = NULL,
      width = ci_width,
      brackets = ci_brackets,
      zap_small = zap_small
    )
    x$ROPE_high <- NULL
    names(x)[names(x) == "ROPE_low"] <- "ROPE"
    x$ROPE_CI <- NULL
  }
  x
}


.format_std_columns <- function(x, other_ci_colname, digits, zap_small) {
  std_cols <- names(x)[grepl("Std_", names(x), fixed = TRUE)]
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


.format_bayes_columns <- function(x,
                                  zap_small,
                                  stars = FALSE,
                                  rope_digits = 2,
                                  ci_width = "auto",
                                  ci_brackets = TRUE,
                                  exact = TRUE) {
  # Specify stars for which column
  if (is.character(stars)) {
    starlist <- list(BF = FALSE, pd = FALSE)
    starlist[stars] <- TRUE
  } else {
    starlist <- list(BF = stars, pd = stars)
  }

  # Indices
  if ("BF" %in% names(x)) {
    x$BF <- format_bf(x$BF, name = NULL, stars = starlist[["BF"]], exact = exact)
  }
  if ("log_BF" %in% names(x)) {
    x$log_BF <- format_bf(exp(x$log_BF), name = NULL, stars = starlist[["BF"]], exact = exact)
    x$BF <- NULL
    colnames(x)[colnames(x) == "log_BF"] <- "BF"
  }
  if ("pd" %in% names(x)) x$pd <- format_pd(x$pd, name = NULL, stars = starlist[["pd"]])
  if ("Rhat" %in% names(x)) x$Rhat <- format_value(x$Rhat, digits = 3)
  if ("ESS" %in% names(x)) x$ESS <- round(x$ESS)

  if ("ROPE_Equivalence" %in% names(x)) names(x)[names(x) == "ROPE_Equivalence"] <- "Equivalence (ROPE)"
  if ("ROPE_Percentage" %in% names(x)) {
    x$ROPE_Percentage <- format_rope(x$ROPE_Percentage, name = NULL, digits = rope_digits)
    names(x)[names(x) == "ROPE_Percentage"] <- "% in ROPE"
  }
  x <- .format_rope_columns(
    x,
    zap_small = zap_small,
    ci_width = ci_width,
    ci_brackets = ci_brackets
  )


  # Priors
  if ("Prior_Location" %in% names(x)) x$Prior_Location <- format_value(x$Prior_Location, protect_integers = TRUE)
  if ("Prior_Scale" %in% names(x)) x$Prior_Scale <- format_value(x$Prior_Scale, protect_integers = TRUE)
  if ("Prior_Distribution" %in% names(x)) {
    x$Prior_Distribution <- ifelse(
      is.na(x$Prior_Distribution), "", x$Prior_Distribution
    )
  }
  if ("Prior_df" %in% names(x)) x$Prior_df <- format_value(x$Prior_df, protect_integers = TRUE)
  if (all(c("Prior_Distribution", "Prior_df") %in% names(x))) {
    missing_df <- is.na(x$Prior_df) | x$Prior_df == ""
    x$Prior_Distribution[!missing_df] <- paste0(
      x$Prior_Distribution[!missing_df], " (df=", x$Prior_df[!missing_df], ")"
    )
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
    # Replace at initial position
    x <- x[c(names(x)[0:(col_position - 1)], "Prior", names(x)[col_position:(length(names(x)) - 1)])]
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- x$Prior_df <- NULL
  }

  x
}


.format_performance_columns <- function(x, digits, ic_digits, zap_small, use_symbols) {
  if (isTRUE(use_symbols) && .unicode_symbols()) {
    if ("R2" %in% names(x)) names(x)[names(x) == "R2"] <- "R\u00b2"
    if ("R2_adjusted" %in% names(x)) names(x)[names(x) == "R2_adjusted"] <- "R\u00b2 (adj.)"
    if ("R2_conditional" %in% names(x)) names(x)[names(x) == "R2_conditional"] <- "R\u00b2 (cond.)"
    if ("R2_marginal" %in% names(x)) names(x)[names(x) == "R2_marginal"] <- "R\u00b2 (marg.)"
    if ("R2_Tjur" %in% names(x)) names(x)[names(x) == "R2_Tjur"] <- "Tjur's R\u00b2"
    if ("R2_Nagelkerke" %in% names(x)) names(x)[names(x) == "R2_Nagelkerke"] <- "Nagelkerke's R\u00b2"
  } else {
    if ("R2_adjusted" %in% names(x)) names(x)[names(x) == "R2_adjusted"] <- "R2 (adj.)"
    if ("R2_conditional" %in% names(x)) names(x)[names(x) == "R2_conditional"] <- "R2 (cond.)"
    if ("R2_marginal" %in% names(x)) names(x)[names(x) == "R2_marginal"] <- "R2 (marg.)"
    if ("R2_Tjur" %in% names(x)) names(x)[names(x) == "R2_Tjur"] <- "Tjur's R2"
    if ("R2_Nagelkerke" %in% names(x)) names(x)[names(x) == "R2_Nagelkerke"] <- "Nagelkerke's R2"
  }
  if ("Performance_Score" %in% names(x)) names(x)[names(x) == "Performance_Score"] <- "Performance-Score"
  if ("Wu_Hausman" %in% names(x)) names(x)[names(x) == "Wu_Hausman"] <- "Wu & Hausman"
  if ("p(Wu_Hausman)" %in% names(x)) names(x)[names(x) == "p(Wu_Hausman)"] <- "p(Wu & Hausman)"
  if ("weak_instruments" %in% names(x)) names(x)[names(x) == "weak_instruments"] <- "Weak instruments"
  if ("weak_instruments_p" %in% names(x)) names(x)[names(x) == "weak_instruments_p"] <- "p(Weak instruments)"

  # Formatting if we have IC and IC weight columns ----

  # add IC weights to IC columns. The next code lines only apply if we have
  # both the IC and IC weights in the data frame
  all_ics <- list(
    AIC = c("AIC", "AIC_wt"), BIC = c("BIC", "BIC_wt"),
    AICc = c("AICc", "AICc_wt"), WAIC = c("WAIC", "WAIC_wt"),
    LOOIC = c("LOOIC", "LOOIC_wt")
  )
  for (ic in names(all_ics)) {
    ics <- all_ics[[ic]]
    if (all(ics %in% colnames(x))) {
      x[[ics[1]]] <- format_value(x[[ics[1]]], digits = ic_digits, zap_small = zap_small)
      x[[ics[2]]] <- format_p(x[[ics[2]]], digits = digits, name = NULL, whitespace = FALSE)
      x[[ics[1]]] <- sprintf("%s (%s)", x[[ics[1]]], x[[ics[2]]])
      x[ics[2]] <- NULL
      names(x)[names(x) == ics[1]] <- sprintf("%s (weights)", ics[1])
    }
  }

  # Formatting if we only have IC columns ----

  # if we don't have IC weights, format regular IC columns
  if ("AIC" %in% names(x)) format_value(x[["AIC"]], digits = ic_digits, zap_small = zap_small)
  if ("BIC" %in% names(x)) format_value(x[["BIC"]], digits = ic_digits, zap_small = zap_small)
  if ("AICc" %in% names(x)) format_value(x[["AICc"]], digits = ic_digits, zap_small = zap_small)
  if ("WAIC" %in% names(x)) format_value(x[["WAIC"]], digits = ic_digits, zap_small = zap_small)
  if ("LOOIC" %in% names(x)) format_value(x[["LOOIC"]], digits = ic_digits, zap_small = zap_small)

  # Formatting if we only have IC weights ----

  # if we don't have regular ICs, format and rename IC weights
  if ("AIC_wt" %in% names(x)) {
    format_p(x[["AIC_wt"]], digits = digits, name = NULL, whitespace = FALSE)
    names(x)[names(x) == "AIC_wt"] <- "AIC weights"
  }
  if ("BIC_wt" %in% names(x)) {
    format_p(x[["BIC_wt"]], digits = digits, name = NULL, whitespace = FALSE)
    names(x)[names(x) == "BIC_wt"] <- "BIC weights"
  }
  if ("AICc_wt" %in% names(x)) {
    format_p(x[["AICc_wt"]], digits = digits, name = NULL, whitespace = FALSE)
    names(x)[names(x) == "AICc_wt"] <- "AICc weights"
  }
  if ("WAIC_wt" %in% names(x)) {
    format_p(x[["WAIC_wt"]], digits = digits, name = NULL, whitespace = FALSE)
    names(x)[names(x) == "WAIC_wt"] <- "WAIC weights"
  }
  if ("LOOIC_wt" %in% names(x)) {
    format_p(x[["LOOIC_wt"]], digits = digits, name = NULL, whitespace = FALSE)
    names(x)[names(x) == "LOOIC_wt"] <- "LOOIC weights"
  }

  x
}


.format_symbols <- function(x, use_symbols) {
  if (isTRUE(use_symbols) && .unicode_symbols()) {
    colnames(x) <- gsub("Delta", "\u0394", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Phi", "\u03D5", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Eta2", "\u03B7\u00b2", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Eta", "\u03B7", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Epsilon", "\u03b5", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Omega2", "\u03C9\u00b2", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Omega", "\u03C9", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("R2", "R\u00b2", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Chi2", "\u03C7\u00b2", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Chi", "\u03C7", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Sigma", "\u03C3", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Rho", "\u03C1", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Mu", "\u03BC", colnames(x), ignore.case = TRUE)
    colnames(x) <- gsub("Theta", "\u03B8", colnames(x), ignore.case = TRUE)
  }
  x
}


# helper ---------------------


.replace_words <- function(x, target, replacement) {
  for (i in seq_along(x)) {
    if (grepl(target, x[i], fixed = TRUE)) {
      x[i] <- gsub(target, replacement, x[i], fixed = TRUE)
    }
  }
  x
}


.additional_arguments <- function(x, value, default) {
  my_args <- attributes(x)$additional_arguments

  if (length(my_args) > 0 && value %in% names(my_args)) {
    out <- my_args[[value]]
  } else {
    out <- attributes(x)[[value]]
  }

  if (is.null(out)) {
    out <- default
  }

  out
}


.unicode_symbols <- function() {
  # symbols only work on windows from R 4.2 and higher
  win_os <- tryCatch(
    {
      si <- Sys.info()
      if (is.null(si["sysname"])) {
        FALSE
      } else {
        si["sysname"] == "Windows" || startsWith(R.version$os, "mingw")
      }
    },
    error = function(e) {
      TRUE
    }
  )

  l10n_info()[["UTF-8"]] && ((win_os && getRversion() >= "4.2") || (!win_os && getRversion() >= "4.0"))
}
