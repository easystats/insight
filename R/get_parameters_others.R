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
#' @inheritSection find_predictors Model components
#'
#' @return A data frame with three columns: the parameter names, the related
#'   point estimates and the component.
#'
#' @examplesIf requireNamespace("betareg", quietly = TRUE)
#' data("GasolineYield", package = "betareg")
#' m <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
#' get_parameters(m)
#' get_parameters(m, component = "precision")
#' @export
get_parameters.betareg <- function(x, component = "all", ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "precision", "location", "distributional", "auxiliary")
  )
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

  text_remove_backticks(params)
}


#' @export
get_parameters.glmgee <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "conditional", "dispersion"))

  junk <- utils::capture.output({
    cs <- suppressWarnings(stats::coef(summary(x, corr = FALSE)))
  })
  est <- stats::na.omit(cs[, "Estimate"])

  out <- data.frame(
    Parameter = names(est),
    Estimate = as.vector(est),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # mark dispersion parameter
  out$Component[out$Parameter == "Dispersion"] <- "dispersion"

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  text_remove_backticks(out)
}


#' @export
get_parameters.nestedLogit <- function(x, component = "all", verbose = TRUE, ...) {
  cf <- as.data.frame(stats::coef(x))
  params <- .gather(cf, names_to = "Component", values_to = "Estimate")
  response_levels <- unlist(lapply(x$dichotomies, function(i) {
    paste0("{", toString(i[[1]]), "} vs. {", toString(i[[2]]), "}")
  }))
  params$Response <- rep(response_levels, each = nrow(cf))
  params$Parameter <- rep(row.names(cf), times = ncol(cf))
  row.names(params) <- NULL

  if (!is.null(component) && !identical(component, "all")) {
    comp <- intersect(names(x$models), component)
    if (!length(comp) && verbose) {
      format_alert(
        paste0(
          "No matching model found. Possible values for `component` are ",
          toString(paste0("\"", names(x$models), "\"")),
          "."
        )
      )
    } else {
      params <- params[params$Component %in% component, ]
    }
  }

  text_remove_backticks(params[c("Parameter", "Estimate", "Response", "Component")])
}


#' @export
get_parameters.DirichletRegModel <- function(x, component = "all", ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "precision", "location", "distributional", "auxiliary")
  )
  cf <- stats::coef(x)

  if (x$parametrization == "common") {
    component <- "all"
    n_comp <- lengths(cf)
    pattern <- paste0("(", paste(x$varnames, collapse = "|"), ")\\.(.*)")
    p_names <- gsub(pattern, "\\2", names(unlist(cf)))

    params <- data.frame(
      Parameter = p_names,
      Estimate = unlist(cf, use.names = FALSE),
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

  text_remove_backticks(params)
}


#' @export
get_parameters.averaging <- function(x, component = "conditional", ...) {
  component <- validate_argument(component, c("conditional", "full"))
  cf <- stats::coef(x, full = component == "full")

  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.glmx <- function(x, component = "all", ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "extra", "location", "distributional", "auxiliary")
  )
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

  text_remove_backticks(params)
}


#' @export
get_parameters.clm2 <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "conditional", "scale"))

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

  text_remove_backticks(params)
}


#' @export
get_parameters.clmm2 <- get_parameters.clm2


#' @export
get_parameters.mvord <- function(x, component = "all", ...) {
  component <- validate_argument(
    component,
    c("all", "conditional", "thresholds", "correlation")
  )
  junk <- utils::capture.output(s <- summary(x)) # nolint
  # intercepts thresholds
  thresholds <- as.data.frame(s$thresholds)
  thresholds$Parameter <- rownames(thresholds)
  thresholds$Response <- gsub("(.*)\\s(.*)", "\\1", thresholds$Parameter)
  # coefficients
  model_coef <- as.data.frame(s$coefficients)
  model_coef$Parameter <- rownames(model_coef)
  model_coef$Response <- gsub("(.*)\\s(.*)", "\\2", model_coef$Parameter)

  if (!all(model_coef$Response %in% thresholds$Response)) {
    resp <- unique(thresholds$Response)
    for (i in model_coef$Response) {
      model_coef$Response[model_coef$Response == i] <- resp[grepl(paste0(i, "$"), resp)]
    }
  }

  params <- data.frame(
    Parameter = c(thresholds$Parameter, model_coef$Parameter),
    Estimate = c(unname(thresholds[, "Estimate"]), unname(model_coef[, "Estimate"])),
    Component = c(rep("thresholds", nrow(thresholds)), rep("conditional", nrow(model_coef))),
    Response = c(thresholds$Response, model_coef$Response),
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

  if (has_single_value(params$Response, remove_na = TRUE)) {
    params$Response <- NULL
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  text_remove_backticks(params)
}


#' @export
get_parameters.mjoint <- function(x, component = "all", ...) {
  component <- validate_argument(component, c("all", "conditional", "survival"))
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

  text_remove_backticks(params)
}


#' @export
get_parameters.systemfit <- function(x, ...) {
  cf <- stats::coef(summary(x))
  f <- find_formula(x, verbose = FALSE)

  system_names <- names(f)
  parameter_names <- row.names(cf)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, parameter_names)
    data.frame(
      Parameter = gsub(pattern, "\\1", parameter_names[params]),
      Estimate = as.vector(cf[params, "Estimate"]),
      Component = i,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}


#' @export
get_parameters.marginaleffects <- function(x, summary = FALSE, merge_parameters = FALSE, ...) {
  # check if we have a Bayesian model here
  if (isTRUE(summary)) {
    s <- summary(x)
    estimate_pos <- which(colnames(s) == "estimate")
    params <- s[, seq_len(estimate_pos - 1), drop = FALSE]
    if (isTRUE(merge_parameters) && ncol(params) > 1L) {
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
    }
    if (isTRUE(merge_parameters)) {
      colnames(out)[1] <- "Parameter"
    }
  } else {
    excl <- c(
      "rowid", "type", "std.error", "contrast", "term", "dydx",
      "statistic", "p.value", "conf.low", "conf.high", "predicted_hi",
      "predicted_lo", "eps", "marginaleffects_eps", "predicted"
    )
    out <- as.data.frame(x[, !names(x) %in% excl, drop = FALSE])
    if ("dydx" %in% colnames(x)) {
      out$Estimate <- x$dydx
    } else {
      out$Estimate <- x$estimate
    }
  }
  text_remove_backticks(out)
}


#' @export
get_parameters.marginaleffects.summary <- function(x, ...) {
  get_parameters.marginaleffects(x, summary = FALSE, ...)
}


#' @export
get_parameters.deltaMethod <- function(x, ...) {
  params <- standardize_names(x)

  data.frame(
    Parameter = rownames(params),
    Estimate = params$Coefficient,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @export
get_parameters.ggcomparisons <- function(x, merge_parameters = FALSE, ...) {
  estimate_name <- intersect(colnames(x), c(attr(x, "coef_name"), "Difference", "Mean", "Ratio"))[1]
  estimate_pos <- which(colnames(x) == estimate_name)
  params <- x[, seq_len(estimate_pos - 1), drop = FALSE]

  if (isTRUE(merge_parameters) && ncol(params) > 1L) {
    r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
    out <- data.frame(
      Parameter = unname(vapply(as.data.frame(r), toString, character(1))),
      Estimate = x[[estimate_pos]],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    out <- data.frame(
      params,
      Estimate = x[[estimate_pos]],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    if (isTRUE(merge_parameters)) {
      colnames(out)[1] <- "Parameter"
    }
  }
  text_remove_backticks(out)
}


#' @export
get_parameters.coxph <- function(x, verbose = TRUE, ...) {
  cf <- stats::coef(summary(x))
  params <- rownames(cf)
  if (is.null(params)) {
    params <- paste(seq_along(cf))
  }

  params <- data.frame(
    Parameter = params,
    Estimate = unname(cf[, 1]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}


#' @export
get_parameters.asym <- function(x, verbose = TRUE, ...) {
  cf <- stats::coef(x)
  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params$Parameter <- gsub("^plus__", "+", params$Parameter)
  params$Parameter <- gsub("^minus__", "-", params$Parameter)

  text_remove_backticks(params)
}


#' @export
get_parameters.oohbchoice <- function(x, verbose = TRUE, ...) {
  cf <- stats::coef(x)
  params <- data.frame(
    Parameter = names(cf),
    Estimate = unname(cf),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  text_remove_backticks(params)
}
