#' Create a reference grid
#'
#' Create a reference matrix, useful for visualisation, with evenly spread and
#' combined values.
#'
#' @param x An object from which to construct the reference grid.
#' @param at Can be `"all"`, a character vector or list of named elements, indicating
#'   the predictors of interest (focal predictors). Can also contain assignments
#'   (as named list, e.g. `at = list(c(Sepal.Length = c(2, 4), Species = "setosa"))`,
#'   or as string, e.g. `at = "Sepal.Length = 2"` or
#'   `at = c("Sepal.Length = 2", "Species = 'setosa'")` - note the usage of single
#'   and double quotes to assign strings within strings). The remaining variables
#'   will be fixed.
#' @param length Length of numeric `"at"` variables.
#' @param range Can be one of `"range"`, `"iqr"`, `"ci"`, `"hdi"` or `"eti"`. If
#'   `"range"` (default), will use the minimum and maximum of the original
#'   vector as end-points. If any other interval, will spread within the range
#'   (the default CI width is `95%` but this can be changed by setting something
#'   else, e.g., `ci = 0.90`). See [IQR()] and [bayestestR::ci()].
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
#' @param ... Arguments passed to or from other methods (for instance, `length`
#'   or `range` to control the spread of numeric variables.).
#'
#' @return Reference grid data frame.
#'
#' @examples
#' if (require("bayestestR", quietly = TRUE)) {
#'   # Add one row to change the "mode" of Species
#'   data <- rbind(iris, iris[149, ], make.row.names = FALSE)
#'
#'   # Single variable is of interest; all others are "fixed"
#'   get_datagrid(data, at = "Sepal.Length")
#'   get_datagrid(data, at = "Sepal.Length", length = 3)
#'   get_datagrid(data, at = "Sepal.Length", range = "ci", ci = 0.90)
#'   get_datagrid(data, at = "Sepal.Length", factors = "mode")
#'
#'   # Multiple variables are of interest, creating a combination
#'   get_datagrid(data, at = c("Sepal.Length", "Species"), length = 3)
#'   get_datagrid(data, at = c(1, 3), length = 3)
#'   get_datagrid(data, at = c("Sepal.Length", "Species"), preserve_range = TRUE)
#'   get_datagrid(data, at = c("Sepal.Length", "Species"), numerics = 0)
#'   get_datagrid(data, at = c("Sepal.Length = 3", "Species"))
#'   get_datagrid(data, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))
#'
#'   # with list-style at-argument
#'   get_datagrid(data, at = list(Sepal.Length = c(1, 3), Species = "setosa"))
#' }
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
                                    ...) {
  target <- at

  # find numerics that were coerced to factor in-formula
  numeric_factors <- colnames(x)[sapply(x, function(i) isTRUE(attributes(i)$factor))]

  specs <- NULL

  if (is.null(target)) {
    targets <- data.frame()
  } else {
    # Valid target argument
    if (all(target == "all")) {
      target <- names(x)
    }

    if (is.numeric(target) || is.logical(target)) {
      target <- names(x)[target]
    }

    # Deal with factor in-formula transformations ============================

    x[] <- lapply(x, function(i) {
      if (isTRUE(attributes(i)$factor)) {
        as.factor(i)
      } else {
        i
      }
    })

    # Deal with targets ==========================================================

    if (is.character(target)) {

      # Find eventual user-defined specifications for each target
      specs <- do.call(rbind, lapply(target, .get_datagrid_clean_target, x = x))
      specs$varname <- as.character(specs$varname) # make sure it's a string not fac
      specs <- specs[!duplicated(specs$varname), ] # Drop duplicates

      specs$is_factor <- sapply(x[specs$varname], function(x) is.factor(x) || is.character(x))

      # Create target list of factors -----------------------------------------
      facs <- list()
      for (fac in specs[specs$is_factor == TRUE, "varname"]) {
        facs[[fac]] <- get_datagrid(x[[fac]], target = specs[specs$varname == fac, "expression"])
      }

      # Create target list of numerics ----------------------------------------
      nums <- list()
      for (num in specs[specs$is_factor == FALSE, "varname"]) {
        nums[[num]] <- get_datagrid(x[[num]],
          target = specs[specs$varname == num, "expression"],
          reference = reference[[num]],
          ...
        )
      }
    } else if (is.list(target)) {

      # we have a list as at-values
      facs <- target[sapply(x[names(target)], is.factor)]
      nums <- target[sapply(x[names(target)], is.numeric)]
    }

    # Assemble the two - the goal is to have two named lists, where variable
    # names are the names of the list-elements: one list contains elements of
    # numeric variables, the other one factors.
    targets <- expand.grid(c(nums, facs))

    # Preserve range ---------------------------------------------------------
    if (preserve_range == TRUE && length(facs) > 0 && length(nums) > 0) {

      # Loop through the combinations of factors
      facs_combinations <- expand.grid(facs)
      for (i in 1:nrow(facs_combinations)) {
        # Query subset of original dataset
        subset <- x[.data_match(x, to = facs_combinations[i, , drop = FALSE]), , drop = FALSE]
        idx <- .data_match(targets, to = facs_combinations[i, , drop = FALSE])

        # Skip if no instance of factor combination, drop the chunk
        if (nrow(subset) == 0) {
          targets <- targets[-idx, ]
          break
        }

        # Else, filter given the range of numerics
        rows_to_remove <- c()
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
        stop("No data left was left after range preservation. Try increasing `length` or setting `preserve_range` to FALSE.")
      }
    }
  }



  # Deal with the rest =========================================================
  rest_vars <- names(x)[!names(x) %in% names(targets)]
  if (length(rest_vars) >= 1) {
    rest_df <- lapply(x[rest_vars], .get_datagrid_summary, numerics = numerics, factors = factors, ...)
    rest_df <- expand.grid(rest_df, stringsAsFactors = FALSE)
    if (nrow(targets) == 0) {
      targets <- rest_df # If target = NULL
    } else {
      targets <- merge(targets, rest_df, sort = FALSE)
    }
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
  attr(targets, "adjusted_for") <- ifelse(length(rest_vars) >= 1, rest_vars, NA)
  attr(targets, "at_specs") <- specs
  attr(targets, "at") <- target
  attr(targets, "preserve_range") <- preserve_range
  attr(targets, "reference") <- reference
  attr(targets, "data") <- x

  # Printing decorations
  attr(targets, "table_title") <- c("Visualisation Grid", "blue")
  if (length(rest_vars) >= 1) attr(targets, "table_footer") <- paste0("\nMaintained constant: ", paste0(rest_vars, collapse = ", "))
  if (!is.null(attr(targets, "table_footer"))) attr(targets, "table_footer") <- c(attr(targets, "table_footer"), "blue")

  class(targets) <- unique(c("datagrid", "visualisation_matrix", class(targets)))
  targets
}












# Utils -------------------------------------------------------------------

#' @keywords internal
.get_datagrid_summary <- function(x, numerics = "mean", factors = "reference", na.rm = TRUE, ...) {
  if (na.rm == TRUE) x <- stats::na.omit(x)

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
        out <- levels(x)[1]
      } else if (is.character(x) || is.logical(x)) {
        out <- unique(x)[1]
      } else {
        stop(paste0(
          "Argument is not numeric nor factor but ",
          class(x),
          ". Please report the bug at https://github.com/easystats/modelbased/issues"
        ))
      }
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

  # Sanity check
  if (!is.numeric(length)) {
    stop("`length` argument should be an number.")
  }

  # Check and clean the target argument
  specs <- .get_datagrid_clean_target(x, ...)

  if (is.na(specs$expression)) {
    # Create a spread
    out <- .create_spread(x, length = length, range = range, ...)
  } else {
    # Run the expression cleaned from target
    out <- eval(parse(text = specs$expression))
  }

  out
}

#' @export
get_datagrid.double <- get_datagrid.numeric





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



#' @keywords internal
.create_spread <- function(x, length = 10, range = "range", ci = 0.95, ...) {
  range <- match.arg(tolower(range), c("range", "iqr", "ci", "hdi", "eti"))

  # bayestestR only for some options
  if (range %in% c("ci", "hdi", "eti")) {
    check_if_installed("bayestestR")
  }

  if (range == "iqr") {
    mini <- stats::quantile(x, (1 - ci) / 2, ...)
    maxi <- stats::quantile(x, (1 + ci) / 2, ...)
  } else if (range == "ci") {
    out <- bayestestR::ci(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "eti") {
    out <- bayestestR::eti(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "hdi") {
    out <- bayestestR::hdi(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else {
    mini <- min(x, na.rm = TRUE)
    maxi <- max(x, na.rm = TRUE)
  }
  seq(mini, maxi, length.out = length)
}


# Utilities -----------------------------------------------------------------

#' @keywords internal
.get_datagrid_clean_target <- function(x, target = NULL, ...) {
  expression <- NA
  varname <- NA
  original_target <- target

  if (!is.null(target)) {
    if (is.data.frame(x) && target %in% names(x)) {
      return(data.frame(varname = target, expression = NA))
    }

    # If there is an equal sign
    if (grepl("length.out =", target)) {
      expression <- target # This is an edgecase
    } else if (grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE))) # Split and clean
      varname <- parts[1] # left-hand part is probably the name of the variable
      target <- parts[2] # right-hand part is the real target
    }

    if (is.na(expression) && is.data.frame(x)) {
      if (!is.na(varname)) {
        x <- x[[varname]]
      } else {
        stop("Couldn't find which variable were selected in `target`. Check spelling and specification.")
      }
    }

    # If brackets are detected [a, b]
    if (is.na(expression) && grepl("\\[.*\\]", target)) {

      # Clean --------------------
      # Keep the content
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      parts <- trimws(unlist(strsplit(parts, ",")))
      # If the elements have quotes around them, drop them
      if (all(grepl("\\'.*\\'", parts))) parts <- gsub("'", "", parts)
      if (all(grepl('\\".*\\"', parts))) parts <- gsub('"', "", parts)

      # Make expression ----------
      if (is.factor(x) || is.character(x)) { # Factor
        # Add quotes around them
        parts <- paste0("'", parts, "'")
        # Convert to character
        expression <- paste0("as.factor(c(", paste0(parts, collapse = ", "), "))")
      } else { # Numeric
        # If only two, it's probably the range
        if (length(parts) == 2) {
          expression <- paste0("seq(", parts[1], ", ", parts[2], ", length.out = length)")
          # If more, it's probably the vector
        } else if (length(parts) > 2) {
          parts <- as.numeric(parts)
          expression <- paste0("c(", paste0(parts, collapse = ", "), ")")
        } else {
          stop(paste0("The `target` argument (", target, ") should indicate the min and the max."))
        }
      }
      # Else, try to directly eval the content
    } else {
      expression <- target
      # Try to eval and make sure it works
      tryCatch(
        {
          # This is just to make sure that an expression with `length` in
          # it doesn't fail because of this undefined var
          length <- 10
          eval(parse(text = target))
        },
        error = function(r) {
          stop(paste0("The `target` argument (`", original_target, "`) cannot be read and could be mispecified."))
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
                                 ...) {
  # Retrieve data from model
  if (is.null(data)) {
    data <- tryCatch(get_data(x)[find_variables(x, "all", flatten = TRUE)], error = function(e) NULL)
  }

  # For models with transformed parameters, "find_variables" may not return
  # matching column names - then try retrieving terms instead
  if (is.null(data)) {
    data <- tryCatch(get_data(x)[find_terms(x, "all", flatten = TRUE)], error = function(e) NULL)
  }

  # still found no data - stop here
  if (is.null(data)) {
    stop(format_message(
      "Can't access data that was used to fit the model in order to create the reference grid.",
      "Please use the `data` argument."
    ))
  }

  # Deal with intercept-only models
  response <- find_response(x)
  if (include_response == FALSE) {
    data <- data[!names(data) %in% response]
    if (ncol(data) < 1) {
      stop(format_message("Model only seems to be an intercept-only model. Use `include_response=TRUE` to create the reference grid."), call. = FALSE)
    }
  }

  # Drop random factors
  random_factors <- find_random(x, flatten = TRUE)
  if (isFALSE(include_random) && !is.null(random_factors)) {
    keep <- c(find_predictors(x, effects = "fixed", flatten = TRUE), response)
    if (!is.null(keep)) {
      if (all(at != "all")) {
        keep <- c(keep, at[at %in% random_factors])
        random_factors <- setdiff(random_factors, at)
      }
      data <- data[names(data) %in% keep]
    }
  }

  if (all(at == "all")) at <- names(data)
  if (include_smooth == FALSE || include_smooth == "fixed") {
    s <- find_smooth(x, flatten = TRUE)
    if (!is.null(s)) {
      at <- names(data)[!names(data) %in% clean_names(s)]
    }
  }

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

  if (include_smooth == FALSE) {
    vm[names(vm) %in% clean_names(find_smooth(x, flatten = TRUE))] <- NULL
  }

  attr(vm, "model") <- x
  vm
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
  if (is.null(data)) {
    data <- tryCatch(get_data(x)[find_variables(x, "all", flatten = TRUE)], error = function(e) NULL)
  }

  # For models with transformed parameters, "find_variables" may not return
  # matching column names - then try retrieving terms instead
  if (is.null(data)) {
    data <- tryCatch(get_data(x)[find_terms(x, "all", flatten = TRUE)], error = function(e) NULL)
  }

  # still found no data - stop here
  if (is.null(data)) {
    stop(format_message(
      "Can't access data that was used to fit the model in order to create the reference grid.",
      "Please use the `data` argument."
    ))
  }

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
  idx <- 1:nrow(x)
  for (col in names(to)) {
    if (col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }
  .to_numeric(row.names(x)[idx])
}
