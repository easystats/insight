#' @title Get the data from predictor variables
#' @name get_predictors
#'
#' @description Returns the data from all predictor variables (fixed effects).
#'
#' @inheritParams find_predictors
#'
#' @return The data from all predictor variables, as data frame.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' head(get_predictors(m))
#' @export
get_predictors <- function(x) {
  vars <- if (inherits(x, "wbm"))
    unlist(.compact_list(find_variables(x, flatten = FALSE)[c("conditional", "instruments")]))
  else
    find_predictors(x, effects = "fixed", component = "all", flatten = TRUE)

  dat <- get_data(x)[, vars, drop = FALSE]

  if (is_empty_object(dat)) {
    print_color("Warning: Data frame is empty, probably you have an intercept-only model?\n", "red")
    return(NULL)
  }

  dat
}
