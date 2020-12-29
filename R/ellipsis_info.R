#' Gather information about objects in ellipsis (dot dot dot)
#'
#' Provides information regarding the models entered in an ellipsis.
#' It detects whether all are models, regressions, nested regressions etc.,
#' assigning different classes to the list of objects.
#'
#' @param objects,... Arbitrary number of objects.
#'
#' @examples
#' m1 <- lm(Sepal.Length ~ Petal.Width + Species, data=iris)
#' m2 <- lm(Sepal.Length ~ Species, data=iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data=iris)
#' m4 <- lm(Sepal.Length ~ 1, data=iris)
#' m5 <- lm(Petal.Width ~ 1, data=iris)
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
ellipsis_info <- function(objects, ...){
  UseMethod("ellipsis_info")
}


#' @export
ellipsis_info.default <- function(..., only_models=TRUE){
  # Create list with names
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # Check whether all are models
  is_model <- sapply(objects, insight::is_model)
  class(objects) <- c("ListModels", class(objects))

  if(sum(is_model == FALSE) > 1){
    if(only_models){
      warning(paste(paste0(object_names[is_model == T], collapse = ", "),
                    "are not supported models and have been dropped."))
      objects <- objects[is_model]
      object_names <- object_names[is_model]
    } else{
      class(objects) <- c("ListObjects", class(objects))
    }
  }
  attr(objects, "is_model") <- is_model

  # Now objects is of class ListObjects or ListModels, so dispatching on the appropriate method
  ellipsis_info(objects)
}





# ListObjects and ListModels ----------------------------------------------


#' @rdname ellipsis_info
#' @export
ellipsis_info.ListObjects <- function(objects, ...){
  # Do nothing
  objects
}

#' @rdname ellipsis_info
#' @export
ellipsis_info.ListModels <- function(objects, ...){
  # Regressions
  # TODO: model_info(x)$is_regression is not yet implemented
  # is_regression <- sapply(objects, function(x) insight::model_info(x)$is_regression)
  is_regression <- rep(TRUE, length(objects))
  if (all(is_regression)) {
    all_regressions <- TRUE
  } else{
    all_regressions <- FALSE
  }

  if(all_regressions) {
    class(objects) <- c("ListRegressions", class(objects))
  }

  # SEM
  # TODO: Add model_info(x)$is_sem or something?

  if(all_regressions == FALSE){  # Later add '& all_sem == FALSE & all_whatnot == FALSE' etc.
    class(objects) <- c("ListVariousModels", class(objects))
  }

  # Dispatch on the next appropriate method
  ellipsis_info(objects)
}


# ListRegressions ---------------------------------------------------------

#' @rdname ellipsis_info
#' @export
ellipsis_info.ListVariousModels <- function(objects, ...){
  # Do nothing for now, but later there could be specific methods
  # For instance to check the nesting of SEM models or such
  objects
}

#' @rdname ellipsis_info
#' @export
ellipsis_info.ListRegressions <- function(objects, ...){
  object_names <- names(objects)

  # Check if same outcome
  same_response <- c()
  outcome <- insight::get_response(objects[[1]])
  for(i in 2:length(object_names)){
    rez <- FALSE
    new_outcome <- insight::get_response(objects[[i]])
    if(length(outcome) == length(new_outcome)){
      if(sum(outcome - new_outcome) == 0){
        rez <- TRUE
      }
    }
    same_response <- c(same_response, rez)
  }
  if(all(same_response)){
    attr(objects, "same_response") <- TRUE
  } else{
    attr(objects, "same_response") <- FALSE
  }

  # Check if nested
  is_nested <- c()
  for(i in 2:length(object_names)){
    is_nested <- c(is_nested, .nested_regressions(objects[[i-1]], objects[[i]]))
  }
  if(all(same_response) & all(is_nested)){
    class(objects) <- c("ListNestedRegressions", class(objects))
    attr(objects, "is_nested") <- TRUE
  } else{
    class(objects) <- c("ListNonNestedRegressions", class(objects))
    attr(objects, "is_nested") <- FALSE
  }
  objects
}



# Helpers -----------------------------------------------------------------

#' @keywords internal
.nested_regressions <- function(basemodel, model) {
  params_base <- insight::find_parameters(basemodel, effects="fixed", component="conditional", flatten=TRUE)
  params <- insight::find_parameters(model, effects="fixed", component="conditional", flatten=TRUE)
  if(all(params %in% params_base)){
    TRUE
  } else{
    FALSE
  }
}