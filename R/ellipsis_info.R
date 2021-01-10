#' Gather information about objects in ellipsis (dot dot dot)
#'
#' Provides information regarding the models entered in an ellipsis.
#' It detects whether all are models, regressions, nested regressions etc.,
#' assigning different classes to the list of objects.
#'
#' @param objects,... Arbitrary number of objects.
#' @param only_models Only keep supported models (default to \code{TRUE}).
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
#'
#' # Lavaan Models
#' if (require("lavaan")) {
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ textual + speed "
#'   m1 <- lavaan::sem(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ 0 * textual + speed "
#'   m2 <- lavaan::sem(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " x1  =~ mpg + cyl
#'                  x2 =~ gear + am "
#'
#'   m3 <- lavaan::sem(structure, data = mtcars)
#'
#'   ellipsis_info(m1, m2, m3)
#' }
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

  # Check whether all are models
  is_model <- sapply(objects, insight::is_model)

  # Drop non-models if need be
  if (only_models && any(is_model == FALSE)) {
    warning(paste(
      paste0(object_names[is_model == TRUE], collapse = ", "),
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
    # TODO: model_info(x)$is_regression is not yet implemented
    # is_regression <- sapply(objects, function(x) model_info(x)$is_regression)
  } else if (all(rep(TRUE, length(objects)))) {
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
ellipsis_info.ListRegressions <- function(objects, ...) {
  object_names <- names(objects)

  # Check if same outcome
  same_response <- c()
  outcome <- get_response(objects[[1]])
  for (i in 2:length(object_names)) {
    rez <- FALSE
    new_outcome <- get_response(objects[[i]])
    if (length(outcome) == length(new_outcome)) {
      if (sum(outcome - new_outcome) == 0) {
        rez <- TRUE
      }
    }
    same_response <- c(same_response, rez)
  }
  attr(objects, "same_response") <- all(same_response)

  # Check if nested
  is_nested_increasing <- is_nested_decreasing <- c()
  len <- length(objects)
  for (i in 2:len) {
    is_nested_decreasing <- c(is_nested_decreasing, .nested_regressions(objects[[i - 1]], objects[[i]]))
    is_nested_increasing <- c(is_nested_increasing, .nested_regressions(objects[[len + 2 - i]], objects[[len + 1 - i]]))
  }
  is_nested <- all(is_nested_decreasing) || all(is_nested_increasing)

  if (all(same_response) & is_nested) {
    class(objects) <- c("ListNestedRegressions", class(objects))
    attr(objects, "is_nested") <- TRUE

    # order of df from models
    model_df <- sapply(objects, n_parameters)

    if (is_nested && any(duplicated(model_df)) && length(unique(sapply(objects, model_name, include_formula = FALSE))) == 1) {
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
  params_base <- find_parameters(basemodel, effects = "fixed", component = "conditional", flatten = TRUE)
  params <- find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE)

  all(params %in% params_base)
}
