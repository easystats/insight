#' Create a reference grid
#'
#' Create a reference matrix, useful for visualisation, with evenly spread and
#' combined values. Usually used to make generate predictions using [get_predicted()].
#' See this [vignette](https://easystats.github.io/modelbased/articles/visualisation_matrix.html)
#' for a tutorial on how to create a visualisation matrix using this function.
#'
#' @param x An object from which to construct the reference grid.
#' @param at Indicates the _focal predictors_ (variables) for the reference grid
#'   and at which values focal predictors should be represented. If not specified
#'   otherwise, representative values for numeric variables or predictors are
#'   evenly distributed from the minimum to the maximum, with a total number of
#'   `length` values covering that range (see 'Examples'). Possible options for
#'   `at` are:
#'   - `"all"`, which will include all variables or predictors.
#'   - a character vector of one or more variable or predictor names, like
#'   `c("Species", "Sepal.Width")`, which will create a grid of all combinations
#'   of unique values. For factors, will use all levels, for numeric variables,
#'   will use a range of length `length` (evenly spread from minimum to maximum)
#'   and for character vectors, will use all unique values.
#'   - a list of named elements, indicating focal predictors and their representative
#'   values, e.g. `at = list(Sepal.Length = c(2, 4), Species = "setosa")`.
#'   - a string with assignments, e.g. `at = "Sepal.Length = 2"` or
#'   `at = c("Sepal.Length = 2", "Species = 'setosa'")` - note the usage of single
#'   and double quotes to assign strings within strings.
#'
#'   There is a special handling of assignments with _brackets_, i.e. values
#'   defined inside `[` and `]`.For **numeric** variables, the value(s) inside
#'   the brackets should either be
#'   - two values, indicating minimum and maximum (e.g. `at = "Sepal.Length = [0, 5]"`),
#'   for which a range of length `length` (evenly spread from given minimum to
#'   maximum) is created.
#'   - more than two numeric values `at = "Sepal.Length = [2,3,4,5]"`, in which
#'   case these values are used as representative values.
#'   - a "token" that creates pre-defined representative values:
#'     - for mean and -/+ 1 SD around the mean: `"x = [sd]"`
#'     - for median and -/+ 1 MAD around the median: `"x = [mad]"`
#'     - for Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum): `"x = [fivenum]"`
#'     - for terciles, including minimum and maximum: `"x = [terciles]"`
#'     - for terciles, excluding minimum and maximum: `"x = [terciles2]"`
#'     - for quartiles, including minimum and maximum: `"x = [quartiles]"`
#'     - for quartiles, excluding minimum and maximum: `"x = [quartiles2]"`
#'     - for minimum and maximum value: `"x = [minmax]"`
#'     - for 0 and the maximum value: `"x = [zeromax]"`
#'
#'   For **factor** variables, the value(s) inside the brackets should indicate
#'   one or more factor levels, like `at = "Species = [setosa, versicolor]"`.
#'   **Note**: the `length` argument will be ignored when using brackets-tokens.
#'
#'   The remaining variables not specified in `at` will be fixed (see also arguments
#'   `factors` and `numerics`).
#' @param length Length of numeric target variables selected in `"at"`. This arguments
#'   controls the number of (equally spread) values that will be taken to represent the
#'   continuous variables. A longer length will increase precision, but can also
#'   substantially increase the size of the datagrid (especially in case of interactions).
#'   If `NA`, will return all the unique values. In case of multiple continuous target
#'   variables, `length` can also be a vector of different values (see examples).
#' @param range Option to control the representative values given in `at`, if
#'   no specific values were provided. Use in combination with the `length` argument
#'   to control the number of values within the specified range. `range` can be
#'   one of the following:
#'   - `"range"` (default), will use the minimum and maximum of the original data
#'   vector as end-points (min and max).
#'   - if an interval type is specified, such as [`"iqr"`][IQR()],
#'   [`"ci"`][bayestestR::ci()], [`"hdi"`][bayestestR::hdi()] or
#'   [`"eti"`][bayestestR::eti()], it will spread the values within that range
#'   (the default CI width is `95%` but this can be changed by adding for instance
#'   `ci = 0.90`.) See [`IQR()`] and [`bayestestR::ci()`]. This can be useful to have
#'   more robust change and skipping extreme values.
#'   - if [`"sd"`][sd()] or [`"mad"`][mad()], it will spread by this dispersion
#'   index around the mean or the median, respectively. If the `length` argument
#'   is an even number (e.g., `4`), it will have one more step on the positive
#'   side (i.e., `-1, 0, +1, +2`). The result is a named vector. See 'Examples.'
#'   - `"grid"` will create a reference grid that is useful when plotting
#'   predictions, by choosing representative values for numeric variables based
#'   on their position in the reference grid. If a numeric variable is the first
#'   predictor in `at`, values from minimum to maximum of the same length as
#'   indicated in `length` are generated. For numeric predictors not specified at
#'   first in `at`, mean and -1/+1 SD around the mean are returned. For factors,
#'   all levels are returned.
#' @param factors Type of summary for factors. Can be `"reference"` (set at the
#'   reference level), `"mode"` (set at the most common level) or `"all"` to
#'   keep all levels.
#' @param numerics Type of summary for numeric values. Can be `"all"` (will
#'   duplicate the grid for all unique values), any function (`"mean"`,
#'   `"median"`, ...) or a value (e.g., `numerics = 0`).
#' @param preserve_range In the case of combinations between numeric variables
#'   and factors, setting `preserve_range = TRUE` will drop the observations
#'   where the value of the numeric variable is originally not present in the
#'   range of its factor level. This leads to an unbalanced grid. Also, if you
#'   want the minimum and the maximum to closely match the actual ranges, you
#'   should increase the `length` argument.
#' @param reference The reference vector from which to compute the mean and SD.
#'   Used when standardizing or unstandardizing the grid using `effectsize::standardize`.
#' @param include_smooth If `x` is a model object, decide whether smooth terms
#'   should be included in the data grid or not.
#' @param include_random If `x` is a mixed model object, decide whether random
#'   effect terms should be included in the data grid or not. If
#'   `include_random` is `FALSE`, but `x` is a mixed model with random effects,
#'   these will still be included in the returned grid, but set to their
#'   "population level" value (e.g., `NA` for *glmmTMB* or `0` for *merMod*).
#'   This ensures that common `predict()` methods work properly, as these
#'   usually need data with all variables in the model included.
#' @param include_response If `x` is a model object, decide whether the response
#'   variable should be included in the data grid or not.
#' @param data Optional, the data frame that was used to fit the model. Usually,
#'   the data is retrieved via `get_data()`.
#' @param verbose Toggle warnings.
#' @param ... Arguments passed to or from other methods (for instance, `length`
#'   or `range` to control the spread of numeric variables.).
#'
#' @return Reference grid data frame.
#'
#' @seealso [get_predicted()]
#'
#' @examples
#' # Datagrids of variables and dataframes =====================================
#' if (require("bayestestR", quietly = TRUE) & require("datawizard", quietly = TRUE)) {
#'   # Single variable is of interest; all others are "fixed" ------------------
#'   # Factors
#'   get_datagrid(iris, at = "Species") # Returns all the levels
#'   get_datagrid(iris, at = "Species = c('setosa', 'versicolor')") # Specify an expression
#'
#'   # Numeric variables
#'   get_datagrid(iris, at = "Sepal.Length") # default spread length = 10
#'   get_datagrid(iris, at = "Sepal.Length", length = 3) # change length
#'   get_datagrid(iris[2:150, ],
#'     at = "Sepal.Length",
#'     factors = "mode", numerics = "median"
#'   ) # change non-targets fixing
#'   get_datagrid(iris, at = "Sepal.Length", range = "ci", ci = 0.90) # change min/max of target
#'   get_datagrid(iris, at = "Sepal.Length = [0, 1]") # Manually change min/max
#'   get_datagrid(iris, at = "Sepal.Length = [sd]") # -1 SD, mean and +1 SD
#'   # identical to previous line: -1 SD, mean and +1 SD
#'   get_datagrid(iris, at = "Sepal.Length", range = "sd", length = 3)
#'   get_datagrid(iris, at = "Sepal.Length = [quartiles]") # quartiles
#'
#'   # Numeric and categorical variables, generating a grid for plots
#'   # default spread length = 10
#'   get_datagrid(iris, at = c("Sepal.Length", "Species"), range = "grid")
#'   # default spread length = 3 (-1 SD, mean and +1 SD)
#'   get_datagrid(iris, at = c("Species", "Sepal.Length"), range = "grid")
#'
#'
#'   # Standardization and unstandardization
#'   data <- get_datagrid(iris, at = "Sepal.Length", range = "sd", length = 3)
#'   data$Sepal.Length # It is a named vector (extract names with `names(out$Sepal.Length)`)
#'   datawizard::standardize(data, select = "Sepal.Length")
#'   data <- get_datagrid(iris, at = "Sepal.Length = c(-2, 0, 2)") # Manually specify values
#'   data
#'   datawizard::unstandardize(data, select = "Sepal.Length")
#'
#'
#'   # Multiple variables are of interest, creating a combination --------------
#'   get_datagrid(iris, at = c("Sepal.Length", "Species"), length = 3)
#'   get_datagrid(iris, at = c("Sepal.Length", "Petal.Length"), length = c(3, 2))
#'   get_datagrid(iris, at = c(1, 3), length = 3)
#'   get_datagrid(iris, at = c("Sepal.Length", "Species"), preserve_range = TRUE)
#'   get_datagrid(iris, at = c("Sepal.Length", "Species"), numerics = 0)
#'   get_datagrid(iris, at = c("Sepal.Length = 3", "Species"))
#'   get_datagrid(iris, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))
#'
#'   # With list-style at-argument
#'   get_datagrid(iris, at = list(Sepal.Length = c(1, 3), Species = "setosa"))
#' }
#'
#' # With models ===============================================================
#' # Fit a linear regression
#' model <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
#' # Get datagrid of predictors
#' data <- get_datagrid(model, length = c(20, 3), range = c("range", "sd"))
#' # same as: get_datagrid(model, range = "grid", length = 20)
#' # Add predictions
#' data$Sepal.Length <- get_predicted(model, data = data)
#' # Visualize relationships (each color is at -1 SD, Mean, and + 1 SD of Petal.Length)
#' plot(data$Sepal.Width, data$Sepal.Length,
#'   col = data$Petal.Length,
#'   main = "Relationship at -1 SD, Mean, and + 1 SD of Petal.Length"
#' )
#' @export
get_datagrid <- function(x, ...) {
  UseMethod("get_datagrid")
}


# -------------------------------------------------------------------------
# Below are get_datagrid functions for DataFrames
# -------------------------------------------------------------------------

#' @rdname get_datagrid
#' @export
get_datagrid.data.frame <- function(x,
                                    at = "all",
                                    factors = "reference",
                                    numerics = "mean",
                                    preserve_range = FALSE,
                                    reference = x,
                                    length = 10,
                                    range = "range",
                                    ...) {
  # find numerics that were coerced to factor in-formula
  numeric_factors <- colnames(x)[vapply(x, function(i) isTRUE(attributes(i)$factor), logical(1))]

  specs <- NULL

  if (is.null(at)) {
    targets <- data.frame()
  } else {
    # check for interactions in "at"
    at <- .extract_at_interactions(at)

    # Validate at argument ============================

    # if list, convert to character
    if (is.list(at)) {
      at <- unname(vapply(names(at), function(i) {
        if (is.numeric(at[[i]])) {
          paste0(i, " = c(", toString(at[[i]]), ")")
        } else {
          paste0(i, " = c(", toString(sprintf("'%s'", at[[i]])), ")")
        }
      }, character(1)))
    }

    if (all(at == "all")) {
      at <- colnames(x)
    }

    if (is.numeric(at) || is.logical(at)) {
      at <- colnames(x)[at]
    }

    # Deal with factor in-formula transformations ============================

    x[] <- lapply(x, function(i) {
      if (isTRUE(attributes(i)$factor)) {
        as.factor(i)
      } else {
        i
      }
    })

    # Deal with logical in-formula transformations ============================

    x[] <- lapply(x, function(i) {
      if (isTRUE(attributes(i)$logical)) {
        as.logical(i)
      } else {
        i
      }
    })

    # Deal with targets =======================================================

    # Find eventual user-defined specifications for each target
    specs <- do.call(rbind, lapply(at, .get_datagrid_clean_target, x = x))
    specs$varname <- as.character(specs$varname) # make sure it's a string not fac
    specs <- specs[!duplicated(specs$varname), ] # Drop duplicates

    specs$is_factor <- vapply(x[specs$varname], function(x) is.factor(x) || is.character(x), TRUE)

    # Create target list of factors -----------------------------------------
    facs <- list()
    for (fac in specs[specs$is_factor, "varname"]) {
      facs[[fac]] <- get_datagrid(
        x[[fac]],
        at = specs[specs$varname == fac, "expression"]
      )
    }

    # Create target list of numerics ----------------------------------------
    nums <- list()
    numvars <- specs[!specs$is_factor, "varname"]
    if (length(numvars)) {
      # Sanitize 'length' argument
      if (length(length) == 1L) {
        length <- rep(length, length(numvars))
      } else if (length(length) != length(numvars)) {
        format_error(
          "The number of elements in `length` must match the number of numeric target variables (n = ",
          length(numvars), ")."
        )
      }
      # Sanitize 'range' argument
      if (length(range) == 1) {
        range <- rep(range, length(numvars))
      } else if (length(range) != length(numvars)) {
        format_error(
          "The number of elements in `range` must match the number of numeric target variables (n = ",
          length(numvars), ")."
        )
      }

      # Get datagrids
      for (i in seq_along(numvars)) {
        num <- numvars[i]
        nums[[num]] <- get_datagrid(x[[num]],
          at = specs[specs$varname == num, "expression"],
          reference = reference[[num]],
          length = length[i],
          range = range[i],
          is_first_predictor = specs$varname[1] == num,
          ...
        )
      }
    }

    # Assemble the two - the goal is to have two named lists, where variable
    # names are the names of the list-elements: one list contains elements of
    # numeric variables, the other one factors.
    targets <- expand.grid(c(nums, facs))

    # sort targets data frame according to order specified in "at"
    targets <- .safe(targets[specs$varname], targets)

    # Preserve range ---------------------------------------------------------
    if (preserve_range && length(facs) > 0 && length(nums) > 0L) {
      # Loop through the combinations of factors
      facs_combinations <- expand.grid(facs)
      for (i in seq_len(nrow(facs_combinations))) {
        # Query subset of original dataset
        subset <- x[.data_match(x, to = facs_combinations[i, , drop = FALSE]), , drop = FALSE]
        idx <- .data_match(targets, to = facs_combinations[i, , drop = FALSE])

        # Skip if no instance of factor combination, drop the chunk
        if (nrow(subset) == 0) {
          targets <- targets[-idx, ]
          break
        }

        # Else, filter given the range of numerics
        rows_to_remove <- NULL
        for (num in names(nums)) {
          mini <- min(subset[[num]], na.rm = TRUE)
          maxi <- max(subset[[num]], na.rm = TRUE)
          rows_to_remove <- c(rows_to_remove, which(targets[[num]] < mini | targets[[num]] > maxi))
        }
        if (length(rows_to_remove) > 0) {
          targets <- targets[-idx[idx %in% rows_to_remove], ] # Drop incompatible rows
          row.names(targets) <- NULL # Reset row.names
        }
      }

      if (nrow(targets) == 0) {
        format_error("No data left was left after range preservation. Try increasing `length` or setting `preserve_range` to FALSE.")
      }
    }
  }



  # Deal with the rest =========================================================
  rest_vars <- names(x)[!names(x) %in% names(targets)]
  if (length(rest_vars) >= 1) {
    rest_df <- lapply(x[rest_vars], .get_datagrid_summary, numerics = numerics, factors = factors, ...)
    rest_df <- expand.grid(rest_df, stringsAsFactors = FALSE)
    if (nrow(targets) == 0) {
      targets <- rest_df # If at = NULL
    } else {
      targets <- merge(targets, rest_df, sort = FALSE)
    }
  } else {
    rest_vars <- NA
  }

  # Prepare output =============================================================
  # Reset row names
  row.names(targets) <- NULL

  # convert factors back to numeric, if these variables were actually
  # numeric in the original data
  if (!is.null(numeric_factors) && length(numeric_factors)) {
    for (i in numeric_factors) {
      targets[[i]] <- .factor_to_numeric(targets[[i]])
    }
  }

  # Attributes
  attr(targets, "adjusted_for") <- rest_vars
  attr(targets, "at_specs") <- specs
  attr(targets, "at") <- at
  attr(targets, "preserve_range") <- preserve_range
  attr(targets, "reference") <- reference
  attr(targets, "data") <- x

  # Printing decorations
  attr(targets, "table_title") <- c("Visualisation Grid", "blue")
  if (!(length(rest_vars) == 1 && is.na(rest_vars)) && length(rest_vars) >= 1) {
    attr(targets, "table_footer") <- paste0("\nMaintained constant: ", toString(rest_vars))
  }
  if (!is.null(attr(targets, "table_footer"))) {
    attr(targets, "table_footer") <- c(attr(targets, "table_footer"), "blue")
  }

  class(targets) <- unique(c("datagrid", "visualisation_matrix", class(targets)))
  targets
}












# Utils -------------------------------------------------------------------

#' @keywords internal
.get_datagrid_summary <- function(x, numerics = "mean", factors = "reference", na.rm = TRUE, ...) {
  if (na.rm) x <- stats::na.omit(x)

  if (is.numeric(x)) {
    if (is.numeric(numerics)) {
      out <- numerics
    } else {
      if (numerics %in% c("all", "combination")) {
        out <- unique(x)
      } else {
        out <- eval(parse(text = paste0(numerics, "(x)")))
      }
    }
  } else {
    if (factors %in% c("all", "combination")) {
      out <- unique(x)
    } else if (factors == "mode") {
      # Get mode
      out <- names(sort(table(x), decreasing = TRUE)[1])
    } else {
      # Get reference
      if (is.factor(x)) {
        all_levels <- levels(x)
      } else if (is.character(x) || is.logical(x)) {
        all_levels <- unique(x)
      } else {
        format_error(paste0(
          "Argument is not numeric nor factor but ", class(x), ".",
          "Please report the bug at https://github.com/easystats/insight/issues"
        ))
      }
      # see "get_modelmatrix()" and #626. Reference level is currently
      # a character vector, which causes the error
      # > Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) :
      # > contrasts can be applied only to factors with 2 or more levels
      # this is usually avoided by calling ".pad_modelmatrix()", but this
      # function ignores character vectors. so we need to make sure that this
      # factor level is also of class factor.
      out <- factor(all_levels[1])
      # although we have reference level only, we still need information
      # about all levels, see #695
      levels(out) <- all_levels
    }
  }
  out
}





# -------------------------------------------------------------------------
# Below are get_datagrid functions that work on a vector (a single column)
# See tests/test-get_datagrid.R for examples
# -------------------------------------------------------------------------



# Numeric -----------------------------------------------------------------

#' @rdname get_datagrid
#' @export
get_datagrid.numeric <- function(x, length = 10, range = "range", ...) {
  # Check and clean the target argument
  specs <- .get_datagrid_clean_target(x, ...)

  # If an expression is detected, run it and return it - we don't need
  # to create any spread of values to cover the range; spread is user-defined
  if (!is.na(specs$expression)) {
    return(eval(parse(text = specs$expression)))
  }

  # If NA, return all unique
  if (is.na(length)) {
    return(sort(unique(x)))
  }

  # Sanity check
  if (!is.numeric(length)) {
    format_error("`length` argument should be an number.")
  }

  # Create a spread
  out <- .create_spread(x, length = length, range = range, ...)
  out
}

#' @export
get_datagrid.double <- get_datagrid.numeric


#' @keywords internal
.create_spread <- function(x, length = 10, range = "range", ci = 0.95, ...) {
  range <- match.arg(tolower(range), c("range", "iqr", "ci", "hdi", "eti", "sd", "mad", "grid"))

  # bayestestR only for some options
  if (range %in% c("ci", "hdi", "eti")) {
    check_if_installed("bayestestR")
  }

  # check if range = "grid" - then use mean/sd for every numeric that
  # is not first predictor...
  if (range == "grid") {
    range <- "sd"
    if (isFALSE(list(...)$is_first_predictor)) {
      length <- 3
    }
  }

  # If Range is a dispersion (e.g., SD or MAD)
  if (range %in% c("sd", "mad")) {
    spread <- -floor((length - 1) / 2):ceiling((length - 1) / 2)
    if (range == "sd") {
      disp <- stats::sd(x, na.rm = TRUE)
      center <- mean(x, na.rm = TRUE)
      labs <- ifelse(sign(spread) == -1, paste(spread, "SD"),
        ifelse(sign(spread) == 1, paste0("+", spread, " SD"), "Mean") # nolint
      )
    } else {
      disp <- stats::mad(x, na.rm = TRUE)
      center <- stats::median(x, na.rm = TRUE)
      labs <- ifelse(sign(spread) == -1, paste(spread, "MAD"),
        ifelse(sign(spread) == 1, paste0("+", spread, " MAD"), "Median") # nolint
      )
    }
    out <- center + spread * disp
    names(out) <- labs

    return(out)
  }

  # If Range is an interval
  if (range == "iqr") {
    mini <- stats::quantile(x, (1 - ci) / 2, ...)
    maxi <- stats::quantile(x, (1 + ci) / 2, ...)
  } else if (range == "ci") {
    out <- bayestestR::ci(x, ci = ci, verbose = FALSE, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "eti") {
    out <- bayestestR::eti(x, ci = ci, verbose = FALSE, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "hdi") {
    out <- bayestestR::hdi(x, ci = ci, verbose = FALSE, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else {
    mini <- min(x, na.rm = TRUE)
    maxi <- max(x, na.rm = TRUE)
  }
  seq(mini, maxi, length.out = length)
}


# Factors & Characters ----------------------------------------------------


#' @rdname get_datagrid
#' @export
get_datagrid.factor <- function(x, ...) {
  # Check and clean the target argument
  specs <- .get_datagrid_clean_target(x, ...)

  if (is.na(specs$expression)) {
    # Keep only unique levels
    if (is.factor(x)) {
      out <- factor(levels(droplevels(x)), levels = levels(droplevels(x)))
    } else {
      out <- unique(x)
    }
  } else {
    # Run the expression cleaned from target
    out <- eval(parse(text = specs$expression))
  }
  out
}

#' @export
get_datagrid.character <- get_datagrid.factor

#' @export
get_datagrid.logical <- get_datagrid.character




# Utilities -----------------------------------------------------------------

#' @keywords internal
.get_datagrid_clean_target <- function(x, at = NULL, ...) {
  expression <- NA
  varname <- NA
  original_target <- at

  if (!is.null(at)) {
    if (is.data.frame(x) && at %in% names(x)) {
      return(data.frame(varname = at, expression = NA))
    }

    # If there is an equal sign
    if (grepl("length.out =", at, fixed = TRUE)) {
      expression <- at # This is an edgecase
    } else if (grepl("=", at, fixed = TRUE)) {
      parts <- trim_ws(unlist(strsplit(at, "=", fixed = TRUE), use.names = FALSE)) # Split and clean
      varname <- parts[1] # left-hand part is probably the name of the variable
      at <- parts[2] # right-hand part is the real target
    }

    if (is.na(expression) && is.data.frame(x)) {
      if (!is.na(varname)) {
        x <- x[[varname]]
      } else {
        format_error(
          "Couldn't find which variable were selected in `at`. Check spelling and specification."
        )
      }
    }

    # If brackets are detected [a, b]
    if (is.na(expression) && grepl("\\[.*\\]", at)) {
      # Clean --------------------
      # Keep the content
      parts <- trim_ws(unlist(regmatches(at, gregexpr("\\[.+?\\]", at)), use.names = FALSE))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      parts <- trim_ws(unlist(strsplit(parts, ",", fixed = TRUE), use.names = FALSE))
      # If the elements have quotes around them, drop them
      if (all(grepl("\\'.*\\'", parts))) parts <- gsub("'", "", parts, fixed = TRUE)
      if (all(grepl('\\".*\\"', parts))) parts <- gsub('"', "", parts, fixed = TRUE)

      # Make expression ----------
      if (is.factor(x) || is.character(x)) {
        # Factor
        # Add quotes around them
        parts <- paste0("'", parts, "'")
        # Convert to character
        expression <- paste0("as.factor(c(", toString(parts), "))")
      } else {
        # Numeric
        # If one, might be a shortcut
        if (length(parts) == 1) {
          shortcuts <- c("meansd", "sd", "mad", "quartiles", "quartiles2", "zeromax", "minmax", "terciles", "terciles2", "fivenum")
          if (parts %in% shortcuts) {
            if (parts %in% c("meansd", "sd")) {
              center <- mean(x, na.rm = TRUE)
              spread <- stats::sd(x, na.rm = TRUE)
              expression <- paste0("c(", center - spread, ",", center, ",", center + spread, ")")
            } else if (parts == "mad") {
              center <- stats::median(x, na.rm = TRUE)
              spread <- stats::mad(x, na.rm = TRUE)
              expression <- paste0("c(", center - spread, ",", center, ",", center + spread, ")")
            } else if (parts == "quartiles") {
              expression <- paste0("c(", paste0(as.vector(stats::quantile(x, na.rm = TRUE)), collapse = ","), ")")
            } else if (parts == "quartiles2") {
              expression <- paste0("c(", paste0(as.vector(stats::quantile(x, na.rm = TRUE))[2:4], collapse = ","), ")")
            } else if (parts == "terciles") {
              expression <- paste0("c(", paste0(as.vector(stats::quantile(x, probs = (1:2) / 3, na.rm = TRUE)), collapse = ","), ")")
            } else if (parts == "terciles2") {
              expression <- paste0("c(", paste0(as.vector(stats::quantile(x, probs = (0:3) / 3, na.rm = TRUE)), collapse = ","), ")")
            } else if (parts == "fivenum") {
              expression <- paste0("c(", paste0(as.vector(stats::fivenum(x, na.rm = TRUE)), collapse = ","), ")")
            } else if (parts == "zeromax") {
              expression <- paste0("c(0,", max(x, na.rm = TRUE), ")")
            } else if (parts == "minmax") {
              expression <- paste0("c(", min(x, na.rm = TRUE), ",", max(x, na.rm = TRUE), ")")
            }
          } else if (is.numeric(parts)) {
            expression <- parts
          } else {
            format_error(
              paste0("The `at` argument (", at, ") should either indicate the minimum and the maximum, or one of the following options: ", paste0(shortcuts, collapse = ", ", "."))
            )
          }
          # If only two, it's probably the range
        } else if (length(parts) == 2) {
          expression <- paste0("seq(", parts[1], ", ", parts[2], ", length.out = length)")
          # If more, it's probably the vector
        } else if (length(parts) > 2) {
          parts <- as.numeric(parts)
          expression <- paste0("c(", toString(parts), ")")
        }
      }
      # Else, try to directly eval the content
    } else {
      expression <- at
      # Try to eval and make sure it works
      tryCatch(
        {
          # This is just to make sure that an expression with `length` in
          # it doesn't fail because of this undefined var
          length <- 10
          eval(parse(text = at))
        },
        error = function(r) {
          format_error(
            paste0("The `at` argument (`", original_target, "`) cannot be read and could be mispecified.")
          )
        }
      )
    }
  }
  data.frame(varname = varname, expression = expression, stringsAsFactors = FALSE)
}




# -------------------------------------------------------------------------
# Below are get_datagrid functions that work on statistical models
# -------------------------------------------------------------------------

#' @rdname get_datagrid
#' @export
get_datagrid.default <- function(x,
                                 at = "all",
                                 factors = "reference",
                                 numerics = "mean",
                                 preserve_range = TRUE,
                                 reference = x,
                                 include_smooth = TRUE,
                                 include_random = FALSE,
                                 include_response = FALSE,
                                 data = NULL,
                                 verbose = TRUE,
                                 ...) {
  # sanity check
  if (!is_model(x)) {
    format_error("`x` must be a statistical model.")
  }

  # Retrieve data from model
  data <- .get_model_data_for_grid(x, data)
  data_attr <- attributes(data)

  # save response - might be necessary to include
  response <- find_response(x)

  # check some exceptions here: logistic regression models with factor response
  # usually require the response to be included in the model, else `get_modelmatrix()`
  # fails, which is required to compute SE/CI for `get_predicted()`
  minfo <- model_info(x)
  if (minfo$is_binomial && minfo$is_logit && is.factor(data[[response]]) && !include_response && verbose) {
    format_warning(
      "Logistic regression model has a categorical response variable. You may need to set `include_response=TRUE` to make it work for predictions."
    )
  }

  # Deal with intercept-only models
  if (isFALSE(include_response)) {
    data <- data[!colnames(data) %in% response]
    if (ncol(data) < 1) {
      format_error("Model only seems to be an intercept-only model. Use `include_response=TRUE` to create the reference grid.")
    }
  }

  # check for interactions in "at"
  at <- .extract_at_interactions(at)

  # Drop random factors
  random_factors <- find_random(x, flatten = TRUE)
  if (isFALSE(include_random) && !is.null(random_factors)) {
    keep <- c(find_predictors(x, effects = "fixed", flatten = TRUE), response)
    if (!is.null(keep)) {
      if (all(at != "all")) {
        keep <- c(keep, at[at %in% random_factors])
        random_factors <- setdiff(random_factors, at)
      }
      data <- data[colnames(data) %in% keep]
    }
  }

  # user wants to include all predictors?
  if (all(at == "all")) at <- colnames(data)

  # exluce smooth terms?
  if (isFALSE(include_smooth) || identical(include_smooth, "fixed")) {
    s <- find_smooth(x, flatten = TRUE)
    if (!is.null(s)) {
      at <- colnames(data)[!colnames(data) %in% clean_names(s)]
    }
  }

  # set back custom attributes
  data <- .replace_attr(data, data_attr)

  vm <- get_datagrid(
    data,
    at = at,
    factors = factors,
    numerics = numerics,
    preserve_range = preserve_range,
    reference = data,
    ...
  )

  # we still need random factors in data grid. we set these to
  # "population level" if not conditioned on via "at"
  if (isFALSE(include_random) && !is.null(random_factors)) {
    if (inherits(x, c("glmmTMB", "brmsfit", "MixMod"))) {
      vm[random_factors] <- NA
    } else if (inherits(x, c("merMod", "rlmerMod", "lme"))) {
      vm[random_factors] <- 0
    }
  }

  if (isFALSE(include_smooth)) {
    vm[colnames(vm) %in% clean_names(find_smooth(x, flatten = TRUE))] <- NULL
  }

  attr(vm, "model") <- x
  vm
}


#' @export
get_datagrid.logitr <- function(x, ...) {
  datagrid <- get_datagrid.default(x, ...)
  obsID <- parse(text = safe_deparse(get_call(x)))[[1]]$obsID
  datagrid[[obsID]] <- x$data[[obsID]][1]
  datagrid
}


#' @export
get_datagrid.wbm <- function(x,
                             at = "all",
                             factors = "reference",
                             numerics = "mean",
                             preserve_range = TRUE,
                             reference = x,
                             include_smooth = TRUE,
                             include_random = FALSE,
                             data = NULL,
                             ...) {
  # Retrieve data from model
  data <- .get_model_data_for_grid(x, data)

  # add id and time variables
  data[[x@call_info$id]] <- levels(stats::model.frame(x)[[x@call_info$id]])[1]
  wave <- stats::model.frame(x)[[x@call_info$wave]]
  if (is.factor(wave)) {
    data[[x@call_info$wave]] <- levels(wave)[1]
  } else {
    data[[x@call_info$wave]] <- mean(wave)
  }

  # clean variable names
  colnames(data) <- clean_names(colnames(data))

  get_datagrid.default(
    x = x, at = at, factors = factors, numerics = numerics,
    preserve_range = preserve_range, reference = reference,
    include_smooth = include_smooth, include_random = include_random,
    include_response = TRUE, data = data, ...
  )
}




# -------------------------------------------------------------------------
# Below are get_datagrid functions that work on get_datagrid
# -------------------------------------------------------------------------

#' @export
get_datagrid.visualisation_matrix <- function(x, reference = attributes(x)$reference, ...) {
  grid <- get_datagrid(as.data.frame(x), reference = reference, ...)

  if ("model" %in% names(attributes(x))) {
    attr(grid, "model") <- attributes(x)$model
  }

  grid
}


#' @export
get_datagrid.datagrid <- get_datagrid.visualisation_matrix





# helper -----------------

.data_match <- function(x, to, ...) {
  if (!is.data.frame(to)) {
    to <- as.data.frame(to)
  }
  idx <- seq_len(nrow(x))
  for (col in names(to)) {
    if (col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }
  .to_numeric(row.names(x)[idx])
}


.get_model_data_for_grid <- function(x, data) {
  # Retrieve data, based on variable names
  if (is.null(data)) {
    data <- get_data(x, verbose = FALSE)
    # make sure we only have variables from original data
    all_vars <- find_variables(x, effects = "all", component = "all", flatten = TRUE)
    if (!is.null(all_vars)) {
      data <- .safe(data[intersect(all_vars, colnames(data))], data)
    }
  }

  # still found no data - stop here
  if (is.null(data)) {
    format_error(
      "Can't access data that was used to fit the model in order to create the reference grid.",
      "Please use the `data` argument."
    )
  }

  # find variables that were coerced on-the-fly
  terms <- find_terms(x, flatten = TRUE)
  factors <- grepl("^(as\\.factor|as_factor|factor|as\\.ordered|ordered)\\((.*)\\)", terms)
  if (any(factors)) {
    factor_expressions <- lapply(terms[factors], str2lang)
    cleaned_terms <- vapply(factor_expressions, all.vars, character(1))
    for (i in cleaned_terms) {
      if (is.numeric(data[[i]])) {
        attr(data[[i]], "factor") <- TRUE
      }
    }
    attr(data, "factors") <- cleaned_terms
  }
  logicals <- grepl("^(as\\.logical|as_logical|logical)\\((.*)\\)", terms)
  if (any(logicals)) {
    logical_expressions <- lapply(terms[logicals], str2lang)
    cleaned_terms <- vapply(logical_expressions, all.vars, character(1))
    for (i in cleaned_terms) {
      if (is.numeric(data[[i]])) {
        attr(data[[i]], "logical") <- TRUE
      }
    }
    attr(data, "logicals") <- cleaned_terms
  }

  data
}



.extract_at_interactions <- function(at) {
  # get interaction terms, but only if these are not inside brackets (like "[4:8]")
  interaction_terms <- grepl("(:|\\*)(?![^\\[]*\\])", at, perl = TRUE)
  if (any(interaction_terms)) {
    at <- unique(clean_names(trim_ws(compact_character(c(
      at[!interaction_terms],
      unlist(strsplit(at[interaction_terms], "(:|\\*)"))
    )))))
  }
  at
}


.replace_attr <- function(data, custom_attr) {
  for (nm in setdiff(names(custom_attr), names(attributes(data.frame())))) {
    attr(data, which = nm) <- custom_attr[[nm]]
  }
  data
}
