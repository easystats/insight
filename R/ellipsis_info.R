#' Gather information about objects in ellipsis (dot dot dot)
#'
#' Provides information regarding the models entered in an ellipsis.
#' It detects whether all are models, regressions, nested regressions etc.,
#' assigning different classes to the list of objects.
#'
#' @param objects,... Arbitrary number of objects. May also be a list of model objects.
#' @param only_models Only keep supported models (default to `TRUE`).
#' @param verbose Toggle warnings.
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
ellipsis_info.default <- function(..., only_models = TRUE, verbose = TRUE) {
  # Create list with names
  model_objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)[["..."]]
  # fix names - if "..." is a list of models, the name is of type "language"
  # and we coerce to character here
  object_names <- lapply(object_names, function(i) {
    if (is.language(i)) {
      safe_deparse(i)
    } else {
      # all other classes are unchanged
      i
    }
  })
  # now check if we have character names. If `ellipses_info()` is called
  # via `do.call()`, we have the model objects instead of their names (see #778)
  # and we then use fixed names
  if (!all(vapply(object_names, is.character, logical(1)))) {
    object_names <- paste0("model", seq_along(model_objects))
  } else if (is.list(object_names)) {
    # convert list of characters into regular character vector
    object_names <- unlist(object_names, use.names = FALSE)
  }
  names(model_objects) <- object_names

  # If only one object was provided, check if it is a list of models, like "list(m1, m2)"
  if (length(model_objects) == 1) {
    # single model?
    if (is_model(model_objects[[1]])) {
      return(model_objects[[1]])
    }
    # only proceed if we have at least one valid model object in that list
    if (!any(sapply(model_objects[[1]], insight::is_model))) {
      return(model_objects[[1]])
    }
    # unlist
    object_names <- object_names[[1]]
    # make sure objects-names is a character vector
    if (!is.character(object_names)) {
      object_names <- safe_deparse(object_names)
    }
    if (all(startsWith(object_names, "list("))) {
      # we now should have something like "list(m1, m2)" ...
      object_names <- trim_ws(unlist(
        strsplit(gsub("list\\((.*)\\)", "\\1", object_names), ",", fixed = TRUE),
        use.names = FALSE
      ))
    } else {
      # ... or a variable/object name, in which case we can use the names
      # of the list-elements directly
      object_names <- names(model_objects[[1]])
    }
    # unlist model objects, so "objects" contains the list of models
    model_objects <- model_objects[[1]]
    # validation check
    if (is.null(object_names)) {
      object_names <- paste("Model", seq_along(model_objects), sep = " ")
    }
    names(model_objects) <- object_names
  }

  # Check whether all are models
  is_model <- vapply(model_objects, is_model, logical(1))

  # Drop non-models if need be
  if (only_models && !all(is_model)) {
    if (isTRUE(verbose)) {
      format_warning(paste(
        toString(object_names[!is_model]),
        "are not supported models and have been dropped."
      ))
    }
    model_objects <- model_objects[is_model]
    object_names <- object_names[is_model]
    is_model <- is_model[is_model]
  }

  # Add class
  if (all(is_model)) {
    class(model_objects) <- c("ListModels", class(model_objects))
  } else {
    class(model_objects) <- c("ListObjects", class(model_objects))
  }

  # Now objects is of class ListObjects or ListModels, so dispatching on the appropriate method
  ellipsis_info(objects = model_objects, verbose = verbose)
}


# ListObjects and ListModels ----------------------------------------------

#' @export
ellipsis_info.ListObjects <- function(objects, ...) {
  # Do nothing
  objects
}


#' @export
ellipsis_info.ListModels <- function(objects, ..., verbose = TRUE) {
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
  ellipsis_info(objects, verbose = verbose)
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
  is_nested_increasing <- is_nested_decreasing <- same_fixef <- NULL
  len <- length(objects)

  for (i in 2:len) {
    nested_decreasing <- .nested_regressions(objects[[i - 1]], objects[[i]])
    nested_increasing <- .nested_regressions(objects[[len + 2 - i]], objects[[len + 1 - i]])

    is_nested_decreasing <- c(is_nested_decreasing, nested_decreasing$is_nested)
    is_nested_increasing <- c(is_nested_increasing, nested_increasing$is_nested)
    same_fixef <- c(same_fixef, nested_decreasing$same_fixef)
  }

  # init here, needed later for warning
  identical_models <- FALSE

  is_nested <- all(is_nested_decreasing) || all(is_nested_increasing)

  # Check if all fixed effects are the same across models
  all_same_fixef <- all(same_fixef)
  attr(objects, "same_fixef") <- all_same_fixef

  # Check if all models are null models
  attr(objects, "all_nullmodels") <- all(sapply(objects, is_nullmodel))

  # if we have nested models, check if models are provided in
  # increasing or decreasing order (according to number of DFs)
  if (isTRUE(same_response) && is_nested) {
    class(objects) <- c("ListNestedRegressions", class(objects))
    attr(objects, "is_nested") <- TRUE

    # order of df from models
    model_df <- sapply(objects, n_parameters)
    model_names <- sapply(objects, model_name, include_formula = FALSE)
    identical_models <- is_nested && anyDuplicated(model_df) > 0 && all(model_names == model_names[1])

    attr(objects, "is_nested_increasing") <- all(is_nested_increasing)
    attr(objects, "is_nested_decreasing") <- all(is_nested_decreasing)
    attr(objects, "identical_nested_models") <- isTRUE(identical_models)
  } else {
    class(objects) <- c("ListNonNestedRegressions", class(objects))
    attr(objects, "is_nested") <- FALSE
  }

  # check which models are mixed models
  mixed_models <- sapply(objects, is_mixed_model)

  # if we have mixed models, check for nested random effects
  if (all(mixed_models)) {
    re_nested_increasing <- re_nested_decreasing <- same_ranef <- NULL
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

  if (identical_models && verbose) {
    if (any(mixed_models)) {
      msg <- "Some of the nested models seem to be identical and probably only vary in their random effects."
    } else {
      msg <- "Some of the nested models seem to be identical"
    }
    format_alert(msg)
  }

  # Get other info
  model_infos <- lapply(objects, model_info)

  # Bayesian
  attr(objects, "all_bayesian") <- all(vapply(
    model_infos,
    function(i) i$is_bayesian,
    logical(1)
  ))

  # determine which is linear or binomial model
  attr(objects, "is_linear") <- vapply(model_infos, function(i) i$is_linear, logical(1))
  attr(objects, "is_binomial") <- vapply(model_infos, function(i) i$is_binomial, logical(1))

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
