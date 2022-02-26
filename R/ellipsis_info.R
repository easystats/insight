#' Gather information about objects in ellipsis (dot dot dot)
#'
#' Provides information regarding the models entered in an ellipsis.
#' It detects whether all are models, regressions, nested regressions etc.,
#' assigning different classes to the list of objects.
#'
#' @param objects,... Arbitrary number of objects.
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

  # If only one objects was provided
  if (length(objects) == 1) {
    return(objects[[1]])
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
  same_response <- all(sapply(objects[2:length(object_names)], function(i) isTRUE(all.equal(get_response(i, verbose = FALSE), outcome))))
  attr(objects, "same_response") <- isTRUE(same_response)

  # Check if nested
  is_nested_increasing <- is_nested_decreasing <- c()
  len <- length(objects)

  for (i in 2:len) {
    is_nested_decreasing <- c(is_nested_decreasing, .nested_regressions(objects[[i - 1]], objects[[i]]))
    is_nested_increasing <- c(is_nested_increasing, .nested_regressions(objects[[len + 2 - i]], objects[[len + 1 - i]]))
  }

  is_nested <- all(is_nested_decreasing) || all(is_nested_increasing)

  if (isTRUE(same_response) & is_nested) {
    class(objects) <- c("ListNestedRegressions", class(objects))
    attr(objects, "is_nested") <- TRUE

    # order of df from models
    model_df <- sapply(objects, n_parameters)

    if (is_nested && any(duplicated(model_df)) && length(unique(sapply(objects, model_name, include_formula = FALSE))) == 1 && verbose) {
      message("Some of the nested models seem to be identical.")
    }

    attr(objects, "is_nested_increasing") <- all(is_nested_increasing)
    attr(objects, "is_nested_decreasing") <- all(is_nested_decreasing)
  } else {
    class(objects) <- c("ListNonNestedRegressions", class(objects))
    attr(objects, "is_nested") <- FALSE
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

  all(params %in% params_base)
}
