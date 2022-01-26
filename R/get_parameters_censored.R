# Survival and censored  models ---------------------------------------------


#' @export
get_parameters.flexsurvreg <- function(x, ...) {
  cf <- stats::coef(x)
  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.aareg <- function(x, ...) {
  sc <- summary(x)

  params <- data.frame(
    Parameter = rownames(sc$table),
    Estimate = unname(sc$table[, 2]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.crr <- function(x, ...) {
  sc <- x$coef

  params <- data.frame(
    Parameter = names(sc),
    Estimate = unname(sc),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.lmodel2 <- function(x, ...) {
  res <- x$regression.results
  out <- as.data.frame(cbind(Method = rep(res$Method, 2), utils::stack(res, select = 2:3)))
  colnames(out) <- c("Component", "Estimate", "Parameter")
  out[c("Parameter", "Estimate", "Component")]
}


#' @export
get_parameters.rqs <- function(x, ...) {
  sc <- suppressWarnings(summary(x))

  if (all(unlist(lapply(sc, is.list)))) {
    list_sc <- lapply(sc, function(i) {
      .x <- as.data.frame(stats::coef(i))
      .x$Parameter <- rownames(.x)
      .x$tau <- i$tau
      .x
    })
    out <- do.call(rbind, list_sc)
    params <- data.frame(
      Parameter = out$Parameter,
      Estimate = out$coefficients,
      Component = sprintf("tau (%g)", out$tau),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    get_parameters.default(x, ...)
  }
  text_remove_backticks(params)
}


#' @export
get_parameters.crq <- function(x, ...) {
  sc <- summary(x)

  if (all(unlist(lapply(sc, is.list)))) {
    list_sc <- lapply(sc, function(i) {
      .x <- as.data.frame(i)
      .x$Parameter <- rownames(.x)
      .x
    })
    out <- do.call(rbind, list_sc)
    params <- data.frame(
      Parameter = out$Parameter,
      Estimate = out$coefficients.Value,
      Component = sprintf("tau (%g)", out$tau),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    params <- data.frame(
      Parameter = names(sc$coefficients[, 1]),
      Estimate = unname(sc$coefficients[, 1]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  text_remove_backticks(params)
}

#' @export
get_parameters.crqs <- get_parameters.crq


#' @export
get_parameters.lqmm <- function(x, ...) {
  cs <- stats::coef(x)

  if (is.matrix(cs)) {
    params <- .gather(as.data.frame(cs), names_to = "Component", values_to = "Estimate")
    params$Component <- sprintf("tau (%s)", params$Component)
    params$Parameter <- rep(rownames(cs), length.out = nrow(params))
    params <- params[c("Parameter", "Estimate", "Component")]
    row.names(params) <- NULL
  } else {
    params <- data.frame(
      Parameter = names(cs),
      Estimate = unname(cs),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  text_remove_backticks(params)
}

#' @export
get_parameters.lqm <- get_parameters.lqmm
