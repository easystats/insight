#' @title Get model parameters from models with special components
#' @name get_parameters.betareg
#'
#' @description Returns the coefficients from a model.
#'
#' @param ... Currently not used.
#'
#' @inheritParams find_parameters
#' @inheritParams find_predictors
#'
#' @return A data frame with three columns: the parameter names, the related
#'   point estimates and the component.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
#' get_parameters(m)
#' @export
get_parameters.betareg <- function(x, component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  params <- data.frame(
    Parameter = gsub("^\\(phi\\)_", "", names(cf)),
    Estimate = unname(cf),
    Component = c(rep("conditional", length(x$coefficients$mean)), rep("precision", length(x$coefficients$precision))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @rdname get_parameters.betareg
#' @export
get_parameters.DirichletRegModel <- function(x, component = c("all", "conditional", "precision", "location", "distributional", "auxiliary"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x)

  if (x$parametrization == "common") {
    component <- "all"
    n_comp <- lapply(cf, length)
    pattern <- paste0("(", paste(x$varnames, collapse = "|"), ")\\.(.*)")
    p_names <- gsub(pattern, "\\2", names(unlist(cf)))

    params <- data.frame(
      Parameter = p_names,
      Estimate = unname(unlist(cf)),
      Response = rep(names(n_comp), sapply(n_comp, function(i) i)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    out1 <- .gather(data.frame(do.call(cbind, cf$beta)), names_to = "Response", values_to = "Estimate")
    out2 <- .gather(data.frame(do.call(cbind, cf$gamma)), names_to = "Component", values_to = "Estimate")
    out1$Component <- "conditional"
    out2$Component <- "precision"
    out2$Response <- NA
    params <- merge(out1, out2, all = TRUE, sort = FALSE)
    params$Parameter <- gsub("(.*)\\.(.*)\\.(.*)", "\\3", names(unlist(cf)))
    params <- params[c("Parameter", "Estimate", "Component", "Response")]
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}


#' @rdname get_parameters.betareg
#' @export
get_parameters.averaging <- function(x, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(x, full = component == "full")

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  .remove_backticks_from_parameter_names(params)
}


#' @rdname get_parameters.betareg
#' @export
get_parameters.glmx <- function(x, component = c("all", "conditional", "extra", "location", "distributional", "auxiliary"), ...) {
  component <- match.arg(component)
  cf <- stats::coef(summary(x))

  params <- rbind(
    data.frame(
      Parameter = names(cf$glm[, 1]),
      Estimate = unname(cf$glm[, 1]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = rownames(cf$extra),
      Estimate = cf$extra[, 1],
      Component = "extra",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}


#' @rdname get_parameters.betareg
#' @export
get_parameters.clm2 <- function(x, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  cf <- stats::coef(summary(x))
  n_intercepts <- length(x$xi)
  n_location <- length(x$beta)
  n_scale <- length(x$zeta)

  params <- data.frame(
    Parameter = rownames(cf),
    Estimate = unname(cf[, "Estimate"]),
    Component = c(rep("conditional", times = n_intercepts + n_location), rep("scale", times = n_scale)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}


#' @export
get_parameters.clmm2 <- get_parameters.clm2


#' @rdname get_parameters.betareg
#' @export
get_parameters.mvord <- function(x, component = c("all", "conditional", "thresholds", "correlation"), ...) {
  component <- match.arg(component)
  junk <- utils::capture.output(s <- summary(x))
  # intercepts thresholds
  thresholds <- as.data.frame(s$thresholds)
  thresholds$Parameter <- rownames(thresholds)
  thresholds$Response <- gsub("(.*)\\s(.*)", "\\1", thresholds$Parameter)
  # coefficients
  coefficients <- as.data.frame(s$coefficients)
  coefficients$Parameter <- rownames(coefficients)
  coefficients$Response <- gsub("(.*)\\s(.*)", "\\2", coefficients$Parameter)

  if (!all(coefficients$Response %in% thresholds$Response)) {
    resp <- unique(thresholds$Response)
    for (i in coefficients$Response) {
      coefficients$Response[coefficients$Response == i] <- resp[grepl(paste0(i, "$"), resp)]
    }
  }

  params <- data.frame(
    Parameter = c(thresholds$Parameter, coefficients$Parameter),
    Estimate = c(unname(thresholds[, "Estimate"]), unname(coefficients[, "Estimate"])),
    Component = c(rep("thresholds", nrow(thresholds)), rep("conditional", nrow(coefficients))),
    Response = c(thresholds$Response, coefficients$Response),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params_error <- data.frame(
    Parameter = rownames(s$error.structure),
    Estimate = unname(s$error.structure[, "Estimate"]),
    Component = "correlation",
    Response = NA,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params <- rbind(params, params_error)

  if (.n_unique(params$Response) == 1) {
    params$Response <- NULL
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}



#' @rdname get_parameters.betareg
#' @export
get_parameters.mjoint <- function(x, component = c("all", "conditional", "survival"), ...) {
  component <- match.arg(component)
  s <- summary(x)

  params <- rbind(
    data.frame(
      Parameter = rownames(s$coefs.long),
      Estimate = unname(s$coefs.long[, 1]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = rownames(s$coefs.surv),
      Estimate = unname(s$coefs.surv[, 1]),
      Component = "survival",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  .remove_backticks_from_parameter_names(params)
}
