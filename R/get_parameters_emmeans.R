#' @title Get model parameters from estimated marginal means objects
#' @name get_parameters.emmGrid
#'
#' @description Returns the coefficients from a model.
#'
#' @param merge_parameters Logical, if \code{TRUE} and \code{x} has multiple
#'   columns for parameter names (like \code{emmGrid} objects may have), these
#'   are merged into a single parameter column, with parameters names and values
#'   as values.
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#' @inheritParams get_parameters.BGGM
#'
#' @return A data frame with two columns: the parameter names and the related
#'   point estimates.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.emmGrid <- function(x, summary = FALSE, merge_parameters = FALSE, ...) {
  # check if we have a Bayesian model here
  if (!.is_baysian_emmeans(x) || isTRUE(summary)) {
    s <- summary(x)
    estimate_pos <- which(colnames(s) == x@misc$estName)
    params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    if (isTRUE(merge_parameters) && ncol(params) > 1) {
      r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
      out <- data.frame(
        Parameter = unname(sapply(as.data.frame(r), paste, collapse = ", ")),
        Estimate = s[[estimate_pos]],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    } else {
      out <- data.frame(
        params,
        Estimate = s[[estimate_pos]],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      if (isTRUE(merge_parameters)) {
        colnames(out)[1] <- "Parameter"
      }
    }
    .remove_backticks_from_parameter_names(out)
  } else {
    .clean_emmeans_draws(x)
  }
}


#' @rdname get_parameters.emmGrid
#' @export
get_parameters.emm_list <- function(x, summary = FALSE, ...) {
  if (!.is_baysian_emmeans(x) || isTRUE(summary)) {
    do.call(rbind, lapply(names(x), function(i) {
      out <- get_parameters(x[[i]], summary = summary)
      if (ncol(out) > 2) {
        est <- out$Estimate
        out$Estimate <- NULL
        r <- apply(out, 1, function(i) paste0(colnames(out), " [", i, "]"))
        out <- data.frame(
          Parameter = unname(sapply(as.data.frame(r), paste, collapse = ", ")),
          Estimate = unname(est),
          stringsAsFactors = FALSE
        )
      }
      out$Component <- i
      colnames(out)[1] <- "Parameter"
      out
    }))
  } else {
    do.call(cbind, lapply(names(x), function(i) {
      .clean_emmeans_draws(x[[i]])
    }))
  }
}




# helper --------------------


.clean_emmeans_draws <- function(x, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop(
      "Package 'emmeans' required for this function to work.\n",
      "Please install it by running `install.packages('emmeans')`."
    )
  }

  if (!is.null(attributes(x)$misc$predict.type) && attributes(x)$misc$predict.type != "none") {
    x <- emmeans::regrid(x, transform = attributes(x)$misc$predict.type, ...)
  }

  draws <- emmeans::as.mcmc.emmGrid(
    x,
    names = FALSE,
    sep.chains = FALSE,
    NE.include = TRUE,
    ...
  )
  data.frame(draws, check.names = FALSE)
}
