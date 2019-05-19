#' @title Get clean names of model paramters
#' @name clean_parameters
#'
#' @description This function "cleans" names of model parameters by removing
#' patterns like \code{"r_"} or \code{"b[]"} (mostly applicable to Stan models)
#' and adding columns with information to which group or component parameters
#' belong (i.e. fixed or random, count or zero-inflated...)
#'
#' @param x A fitted model.
#'
#' @return A data frame with "cleaned" parameter names and information on
#'   group or component where parameters belong to.
#'
#' @examples
#' model <- download_model("brms_zi_2")
#' clean_parameters(model)
#' @export
clean_parameters <- function(x, ...) {
  UseMethod("clean_parameters")
}


#' @export
clean_parameters.brmsfit <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

}


#' @export
clean_parameters.glmmTMB <- function(x, ...) {
  pars <- find_parameters(x, effects = "all", component = "all", flatten = FALSE)

  l <- lapply(names(pars), function(i) {
    eff <- if (grepl("random", i, fixed = TRUE))
      "random"
    else
      "fixed"

    com <- if (grepl("zero_inflated", i, fixed = TRUE))
      "zero_inflated"
    else
      "conditional"

    if (eff == "random") {
      rand_eff <- lapply(names(pars[[i]]), function(j) {
        data.frame(
          Parameter = pars[[i]][[j]],
          Effect = eff,
          Component = com,
          Group = j,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
      do.call(rbind, rand_eff)
    } else {
      data.frame(
        Parameter = pars[[i]],
        Effect = eff,
        Component = com,
        Group = "",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  })

  do.call(rbind, l)
}