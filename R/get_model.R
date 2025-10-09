#' @title Get a model objects that is saved as attribute
#' @name get_model
#'
#' @description This functions tries to get a model object from the object `x`,
#' where the model object is saved as an (arbitrarily named) attribute. This is
#' useful for example, when a model is fitted and saved as an attribute of a
#' data frame.
#'
#' @param x An object that contains a model object as an attribute. This could be
#' a data frame or any other object that has an attribute containing the model.
#' @param name The name of the attribute that contains the model object. Defaults
#' to `"model"`.
#' @param element String or character vector. If provided, this argument allows
#' you to specify which element(s) of the model object to return. This can be
#' useful if the model object is a list or has multiple components, and you only
#' want to extract a specific part.
#' @param ... Not used.
#'
#' @return The object that is stored as an attribute of `x` with the name `name`,
#' or the specific element of that object if `element` is provided. If the
#' attribute or element does not exist, an error is raised.
#'
#' @examples
#' # Example of using get_model
#' d <- data.frame(x = rnorm(100), y = rnorm(100))
#' # fit a model and save it as an attribute
#' model <- lm(y ~ x, data = d)
#' attr(d, "model") <- model
#' # get the model back
#' get_model(d)
#' # get the coefficients of the model
#' get_model(d, element = "coefficients")
#'
#' @export
get_model <- function(x, name = "model", element = NULL, ...) {
  # extract the model from the attributes of x
  model <- .safe(attr(x, name, exact = TRUE))

  # check if "name" exists in attributes of x
  if (is.null(model)) {
    format_error(paste0("No attribute named `", name, "` found in the object."))
  }

  # check if element should be extracted
  if (!is.null(element)) {
    if (is.list(model) && all(element %in% names(model))) {
      if (length(element) > 1) {
        return(model[element])
      } else {
        return(model[[element]])
      }
    } else {
      element <- element[!element %in% names(model)]
      format_error(paste0(
        "Element(s) ",
        toString(paste0("`", element, "`")),
        " not found in the model object."
      ))
    }
  }

  # return the entire model object
  model
}
