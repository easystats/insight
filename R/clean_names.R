#' @title Get clean names of model terms
#' @name clean_names
#'
#' @description This function "cleans" names of model terms (or a character
#'   vector with such names) by removing patterns like `log()` or
#'   `as.factor()` etc.
#'
#' @param x A fitted model, or a character vector.
#' @param include_names Logical, if `TRUE`, returns a named vector where
#'   names are the original values of `x`.
#' @param ... Currently not used.
#'
#' @return The "cleaned" variable names as character vector, i.e. pattern
#'   like `s()` for splines or `log()` are removed from
#'   the model terms.
#'
#' @note Typically, this method is intended to work on character vectors,
#'   in order to remove patterns that obscure the variable names. For
#'   convenience reasons it is also possible to call `clean_names()`
#'   also on a model object. If `x` is a regression model, this
#'   function is (almost) equal to calling `find_variables()`. The
#'   main difference is that `clean_names()` always returns a character
#'   vector, while `find_variables()` returns a list of character
#'   vectors, unless `flatten = TRUE`. See 'Examples'.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' # example from ?stats::glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- as.numeric(gl(3, 1, 9))
#' treatment <- gl(3, 3)
#' m <- glm(counts ~ log(outcome) + as.factor(treatment), family = poisson())
#' clean_names(m)
#'
#' # difference "clean_names()" and "find_variables()"
#' data(cbpp, package = "lme4")
#' m <- lme4::glmer(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#'
#' clean_names(m)
#' find_variables(m)
#' find_variables(m, flatten = TRUE)
#' @export
clean_names <- function(x, ...) {
  UseMethod("clean_names")
}


#' @export
clean_names.default <- function(x, ...) {
  if (is.null(x)) {
    return(x)
  }
  cleaned <- unname(find_variables(x, flatten = TRUE, verbose = FALSE))
  .remove_values(cleaned, c("1", "0"))
}


#' @rdname clean_names
#' @export
clean_names.character <- function(x, include_names = FALSE, ...) {
  .clean_names(x = x, include_names = include_names, ...)
}


# helper -----------------

.clean_names <- function(x, include_names = FALSE, is_emmeans = FALSE, ...) {
  if (is.null(x)) {
    return(x)
  }
  out <- unlist(lapply(x, function(.x) {
    # in case we have ranges, like [2:5], remove those first, so it's not
    # treated as "interaction"
    .x <- sub("\\[(\\d+):(\\d+)\\]", "", .x)
    if (grepl(":", .x, fixed = TRUE) && !grepl("::", .x, fixed = TRUE) && !startsWith(.x, "mm(")) {
      paste(
        sapply(
          strsplit(.x, ":", fixed = TRUE),
          .remove_pattern_from_names,
          is_emmeans = is_emmeans
        ),
        collapse = ":"
      )
    } else {
      .remove_pattern_from_names(.x, is_emmeans = is_emmeans)
    }
  }), use.names = FALSE)

  if (isTRUE(include_names)) {
    stats::setNames(out, x)
  } else {
    unname(out)
  }
}


# utils ---------------------


.remove_pattern_from_names <- function(x,
                                       ignore_asis = FALSE,
                                       ignore_lag = FALSE,
                                       is_emmeans = FALSE) {
  # return if x is empty
  if (.is_empty_string(x)) {
    return("")
  }

  # for gam-smoothers/loess, remove s()- and lo()-function in column name
  # for survival, remove strata(), and so on...
  pattern <- c(
    "as.factor", "as.numeric", "factor", "frailty", "offset", "log1p", "log10",
    "log2", "log(log", "scale(log", "log", "lag", "diff", "lspline", "as.logical",
    "logical", "ordered", "as.ordered", "pspline", "scale(poly", "poly", "catg",
    "asis", "matrx", "pol", "strata", "strat", "scale", "scored", "interaction",
    "sqrt", "sin", "cos", "tan", "acos", "asin", "atan", "atan2", "exp", "lsp",
    "rcs", "pb", "lo", "bs", "ns", "mSpline", "bSpline", "t2", "te", "ti", "tt",
    "mmc", "mm", "mi", "mo", "gp", "s", "I", "gr", "relevel(as.factor", "relevel"
  )

  # sometimes needed for panelr models, where we need to preserve "lag()"
  if (ignore_lag) {
    lag_pattern <- which(pattern == "lag")
    if (length(lag_pattern)) pattern <- pattern[-lag_pattern]
  }

  # do we have a "log()" pattern here? if yes, get capture region
  # which matches the "cleaned" variable name
  cleaned <- unlist(lapply(seq_along(x), function(i) {
    # copy value(s)
    out <- x[i]
    # check if we have special patterns like 100 * log(xy), and remove it
    if (isFALSE(is_emmeans) && grepl("^([0-9]+)", out)) {
      out <- gsub("^([0-9]+)[^(\\.|[:alnum:])]+(.*)", "\\2", out)
    }
    for (j in seq_along(pattern)) {
      # check if we find pattern at all
      if (any(grepl(pattern[j], out, fixed = TRUE))) {
        # remove possible namespace
        if (grepl("::", out, fixed = TRUE)) {
          out <- sub("(.*)::(.*)", "\\2", out)
        }
        if (pattern[j] == "offset") { # nolint
          out <- trim_ws(unique(sub("^offset\\(([^-+ )]*).*", "\\1", out)))
        } else if (pattern[j] == "I") {
          if (!ignore_asis && grepl("I\\((.*)\\)", out)) {
            # out <- trim_ws(unique(sub("I\\(((\\w|\\.)*).*", "\\1", out)))
            # for compatibility with `get_data(source = "mf")`, we return only
            # the first value for "I()". But in some examples, like `I(food/income)`,
            # we would actutually have two names - but this breaks get_data()
            out <- all.vars(stats::as.formula(paste("~", out)))[1]
          }
        } else if (pattern[j] == "asis") {
          if (!ignore_asis && grepl("asis\\((.*)\\)", out)) {
            # out <- trim_ws(unique(sub("asis\\(((\\w|\\.)*).*", "\\1", out)))
            out <- all.vars(stats::as.formula(paste("~", out)))[1]
          }
        } else if (pattern[j] == "log(log") {
          out <- trim_ws(unique(sub("^log\\(log\\(((\\w|\\.)*).*", "\\1", out)))
        } else if (pattern[j] == "relevel(as.factor") {
          out <- trim_ws(unique(sub("^relevel\\(as.factor\\(((\\w|\\.)*).*", "\\1", out)))
        } else if (pattern[j] == "scale(log") {
          out <- trim_ws(unique(sub("^scale\\(log\\(((\\w|\\.)*).*", "\\1", out)))
          out <- trim_ws(unique(sub("^scale\\(log1p\\(((\\w|\\.)*).*", "\\1", out)))
          out <- trim_ws(unique(sub("^scale\\(log2\\(((\\w|\\.)*).*", "\\1", out)))
          out <- trim_ws(unique(sub("^scale\\(log10\\(((\\w|\\.)*).*", "\\1", out)))
        } else if (pattern[j] == "scale(poly") {
          out <- trim_ws(unique(sub("^scale\\(poly\\(((\\w|\\.)*).*", "\\1", out)))
        } else if (pattern[j] %in% c("mmc", "mm")) {
          if (grepl(paste0("^", pattern[j], "\\((.*)\\).*"), out)) {
            out <- all.vars(stats::as.formula(paste("~", out)))
          }
        } else if (pattern[j] == "s" && startsWith(out, "s(")) {
          out <- gsub("^s\\(", "", out)
          out <- gsub("\\)$", "", out)
          if (grepl("=|[[:digit:]]", out)) {
            new_x <- trim_ws(unlist(strsplit(out, ",", fixed = TRUE), use.names = FALSE))
            to_remove <- which(!grepl("\\D", new_x))
            to_remove <- c(to_remove, grep("=", new_x, fixed = TRUE))
            if (length(to_remove) == 0) {
              out <- toString(new_x)
            } else {
              out <- toString(new_x[-to_remove])
            }
          }
        } else {
          # p <- paste0("^", pattern[j], "\\(([^,/)]*).*")
          # this one should be more generic...
          p <- paste0("^", pattern[j], "\\(((\\w|\\.)*).*")
          out <- unique(sub(p, "\\1", out))
        }
      }
    }
    # for coxme-models, remove random-effect things...
    if (any(grepl("|", out, fixed = TRUE))) {
      out <- sub("^(.*)\\|(.*)", "\\2", out)
    }

    trim_ws(out)
  }), use.names = FALSE)

  # remove for random intercept only models
  .remove_values(cleaned, c("1", "0"))
}


## TODO multimembership-models may also have weights, this does not work yet

.clean_brms_mm <- function(x) {
  # only clean for mm() / mmc() functions, else return x
  if (!grepl("^(mmc|mm)\\(", x)) {
    return(x)
  }

  # extract terms from mm() / mmc() functions, i.e. get
  # multimembership-terms
  compact_character(unlist(lapply(c("mmc", "mm"), function(j) {
    if (grepl(paste0("^", j, "\\("), x = x)) {
      all.vars(stats::as.formula(paste("~", x)))
    } else {
      ""
    }
  }, simplify = FALSE), use.names = FALSE))
}
