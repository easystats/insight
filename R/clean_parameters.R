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
#'   effects, component and group where parameters belong to. To be consistent
#'   across different models, the returned data frame always has the four
#'   columns \code{parameter}, \code{effect}, \code{component} and \code{group},
#'   even for models where some of these columns are not applicable.
#'
#' @details The \code{effects} column indicate if a parameter is a \emph{fixed}
#' or \emph{random} effect. The \code{component} can either be \emph{conditional}
#' or \emph{zero_inflated}. For models with random effects, the \code{group}
#' column indicates the grouping factor of the random effects.
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
clean_parameters.default <- function(x, ...) {
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
          parameter = pars[[i]][[j]],
          effects = eff,
          component = com,
          group = j,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
      do.call(rbind, rand_eff)
    } else {
      data.frame(
        parameter = pars[[i]],
        effects = eff,
        component = com,
        group = "",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  })

  do.call(rbind, l)
}