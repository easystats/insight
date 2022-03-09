#' Gather information about objects in ellipsis (dot dot dot)
#'
#' Provides information regarding the models entered in an ellipsis.
#' It detects whether all are models, regressions, nested regressions etc.,
#' assigning different classes to the list of objects.
#'
#' @param objects,... Arbitrary number of objects. May also be a list of model objects.
#' @param only_models Only keep supported models (default to `TRUE`).
#'
#' @return The list with objects that were passed to the function, including
#' additional information as attributes (e.g. if models have same response or
#' are nested).
#'
#' @examples
#' m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m2 <- lm(Sepal.Length ~ Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#' m4 <- lm(Sepal.Length ~ 1, data = iris)
#' m5 <- lm(Petal.Width ~ 1, data = iris)
#'
#' objects <- ellipsis_info(m1, m2, m3, m4)
#' class(objects)
#'
#' objects <- ellipsis_info(m1, m2, m4)
#' attributes(objects)$is_nested
#'
#' objects <- ellipsis_info(m1, m2, m5)
#' attributes(objects)$same_response
#' @export
ellipsis_info <- function(objects, ...) {
  UseMethod("ellipsis_info")
}

#' @rdname ellipsis_info
#' @export
ellipsis_info.default <- function(..., only_models = TRUE) {
  # Create list with names
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # If only one object was provided, check if it is a list of models, like "list(m1, m2)"
  if (length(objects) == 1) {
    # only proceed if we have at least one model object in that list
    if (any(sapply(objects[[1]], insight::is_model))) {
      # unlist
      object_names <- object_names[[1]]
      # make sure objects-names is a character vector
      if (!is.character(object_names)) {
        object_names <- .safe_deparse(object_names)
      }
      if (all(grepl("^list\\(", object_names))) {
        # we now should have something like "list(m1, m2)" ...
        object_names <- .trim(unlist(strsplit(gsub("list\\((.*)\\)", "\\1", object_names), ",")))
      } else {
        # ... or a variable/object name, in which case we can use the names
        # of the list-elements directly
        object_names <- names(objects[[1]])
      }
      objects <- objects[[1]]
      names(objects) <- object_names
    } else {
      return(objects[[1]])
    }
  }

  # Check whether all are models
  is_model <- sapply(objects, insight::is_model)

  # Drop non-models if need be
  if (only_models && any(is_model == FALSE)) {
    warning(paste(
      paste0(object_names[is_model == FALSE], collapse = ", "),
      "are not supported models and have been dropped."
    ))
    objects <- objects[is_model]
    object_names <- object_names[is_model]
  }

  # Add class
  if (all(is_model)) {
    class(objects) <- c("ListModels", class(objects))
  } else {
    class(objects) <- c("ListObjects", class(objects))
  }

  # Now objects is of class ListObjects or ListModels, so dispatching on the appropriate method
  ellipsis_info(objects)
}


# ListObjects and ListModels ----------------------------------------------

#' @export
ellipsis_info.ListObjects <- function(objects, ...) {
  # Do nothing
  objects
}


#' @export
ellipsis_info.ListModels <- function(objects, ...) {

  # Lavaan
  if (all(sapply(objects, inherits, what = "lavaan"))) {
    class(objects) <- c("ListLavaan", class(objects))

    # Regressions
  } else if (all(sapply(objects, is_regression_model))) {
    class(objects) <- c("ListRegressions", class(objects))

    # Mixed bag
  } else {
    class(objects) <- c("ListVarious", class(objects))
  }

  # Dispatch on the next appropriate method
  ellipsis_info(objects)
}


# ListRegressions ---------------------------------------------------------


#' @export
ellipsis_info.ListVarious <- function(objects, ...) {
  # Do nothing (for now?)
  objects
}


#' @export
ellipsis_info.ListLavaan <- function(objects, ...) {
  # TODO: check the nesting
  objects
}



#' @export
ellipsis_info.ListRegressions <- function(objects, ..., verbose = TRUE) {
  object_names <- names(objects)

  # Check if same outcome
  outcome <- get_response(objects[[1]], verbose = FALSE)
  same_response <- TRUE
  i <- 2
  while (same_response && i <= length(objects)) {
    same_response <- isTRUE(all.equal(get_response(objects[[i]], verbose = FALSE), outcome))
    i <- i + 1
  }
  attr(objects, "same_response") <- isTRUE(same_response)

  # Check if nested
  is_nested_increasing <- is_nested_decreasing <- same_fixef <- c()
  len <- length(objects)

  for (i in 2:len) {
    nested_decreasing <- .nested_regressions(objects[[i - 1]], objects[[i]])
    nested_increasing <- .nested_regressions(objects[[len + 2 - i]], objects[[len + 1 - i]])

    is_nested_decreasing <- c(is_nested_decreasing, nested_decreasing$is_nested)
    is_nested_increasing <- c(is_nested_increasing, nested_increasing$is_nested)
    same_fixef <- c(same_fixef, nested_decreasing$same_fixef)
  }

  is_nested <- all(is_nested_decreasing) || all(is_nested_increasing)

  # Check if all fixed effects are the same across models
  all_same_fixef <- all(same_fixef)
  attr(objects, "same_fixef") <- all_same_fixef

  # Check if all models are null models
  attr(objects, "all_nullmodels") <- all(sapply(objects, is_nullmodel))

  # if we have nested models, check if models are provided in
  # increasing or decreasing order (according to number of DFs)
  if (isTRUE(same_response) & is_nested) {
    class(objects) <- c("ListNestedRegressions", class(objects))
    attr(objects, "is_nested") <- TRUE

    # order of df from models
    model_df <- sapply(objects, n_parameters)
    identical_models <- is_nested && any(duplicated(model_df)) && length(unique(sapply(objects, model_name, include_formula = FALSE))) == 1

    if (identical_models && verbose) {
      message("Some of the nested models seem to be identical.")
    }

    attr(objects, "is_nested_increasing") <- all(is_nested_increasing)
    attr(objects, "is_nested_decreasing") <- all(is_nested_decreasing)
    attr(objects, "identical_nested_models") <- isTRUE(identical_models)
  } else {
    class(objects) <- c("ListNonNestedRegressions", class(objects))
    attr(objects, "is_nested") <- FALSE
  }

  # if we have mixed models, check for nested random effects
  if (all(sapply(objects, is_mixed_model))) {
    re_nested_increasing <- re_nested_decreasing <- same_ranef <- c()
    len <- length(objects)

    for (i in 2:len) {
      nested_decreasing <- .nested_random_effects(objects[[i - 1]], objects[[i]])
      nested_increasing <- .nested_random_effects(objects[[len + 2 - i]], objects[[len + 1 - i]])

      re_nested_decreasing <- c(re_nested_decreasing, nested_decreasing$is_nested)
      re_nested_increasing <- c(re_nested_increasing, nested_increasing$is_nested)
      same_ranef <- c(same_ranef, nested_decreasing$same_ranef)
    }

    attr(objects, "re_nested_increasing") <- all(re_nested_increasing)
    attr(objects, "re_nested_decreasing") <- all(re_nested_decreasing)
    attr(objects, "re_nested") <- (all(re_nested_increasing) || all(re_nested_decreasing))
    attr(objects, "same_ranef") <- all(same_ranef)
    attr(objects, "all_mixed_models") <- TRUE
  } else {
    attr(objects, "all_mixed_models") <- FALSE
  }

  objects
}



# Helpers -----------------------------------------------------------------

#' @keywords internal
.nested_regressions <- function(basemodel, model) {
  params_base <- find_parameters(basemodel,
    effects = "fixed",
    component = "conditional",
    flatten = TRUE
  )

  params <- find_parameters(model,
    effects = "fixed",
    component = "conditional",
    flatten = TRUE
  )

  # poly() are not properly recognized as nested, so remove poly() syntax here
  pattern <- paste0("^poly\\(((\\w|\\.)*).*\\)(\\d)")

  poly_terms <- grepl("^poly\\((.*)\\)", params)
  if (any(poly_terms)) {
    params[poly_terms] <- gsub(pattern, "\\1\\3", params[poly_terms])
  }

  poly_terms <- grepl("^poly\\((.*)\\)", params_base)
  if (any(poly_terms)) {
    params_base[poly_terms] <- gsub(pattern, "\\1\\3", params_base[poly_terms])
  }

  list(
    is_nested = all(params %in% params_base),
    same_fixef = all(params %in% params_base) && all(params_base %in% params)
  )
}


#' @keywords internal
.nested_random_effects <- function(basemodel, model) {
  re_basemodel <- find_random(basemodel, flatten = TRUE)
  re_model <- find_random(model, flatten = TRUE)

  if (is.null(re_basemodel) || is.null(re_model)) {
    return(NULL)
  }

  list(
    is_nested = all(re_model %in% re_basemodel),
    same_ranef = all(re_model %in% re_basemodel) && all(re_basemodel %in% re_model)
  )
}
