simulate_predictions <- function(
  model,
  nsim,
  clean_terms,
  ci,
  type,
  interval = "confidence"
) {
  model_data <- get_data(model)
  model_info <- insight::model_info(model)

  if (
    model_info$is_binomial ||
      model_info$is_multinomial ||
      model_info$is_ordinal ||
      model_info$is_categorical
  ) {
    insight::format_error(
      "Can't simulate predictions from models with binary, categorical or ordinal outcome. Please use another option for argument `type`."
    )
  } # nolint

  if (type == "simulate") {
    sims <- suppressWarnings(tryCatch(
      stats::simulate(model, nsim = nsim, re.form = NULL),
      error = function(e) stats::simulate(model, nsim = nsim, re.form = NA)
    ))
  } else {
    sims <- stats::simulate(model, nsim = nsim, re.form = NA)
  }

  model_data$predicted <- rowMeans(sims)
  model_data$conf.low <- apply(sims, 1, stats::quantile, probs = 1 - ci)
  model_data$conf.high <- apply(sims, 1, stats::quantile, probs = ci)
  model_data$std.error <- apply(sims, 1, stats::sd)

  means_predicted <- .aggregate_simulations(model_data$predicted, clean_terms, model_data)
  means_conf_low <- .aggregate_simulations(model_data$conf.low, clean_terms, model_data)
  means_conf_high <- .aggregate_simulations(model_data$conf.high, clean_terms, model_data)
  means_se <- .aggregate_simulations(model_data$std.error, clean_terms, model_data)

  colnames(means_predicted) <- c(clean_terms, "predicted")
  colnames(means_conf_low) <- c(clean_terms, "conf.low")
  colnames(means_conf_high) <- c(clean_terms, "conf.high")
  colnames(means_se) <- c(clean_terms, "std.error")

  model_data <- cbind(
    means_predicted[clean_terms],
    predicted = means_predicted$predicted,
    conf.low = means_conf_low$conf.low,
    conf.high = means_conf_high$conf.high,
    std.error = means_se$std.error
  )
  rownames(model_data) <- NULL
  model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]

  if (length(clean_terms) == 1) {
    model_data <- model_data[order(model_data[[1]]), , drop = FALSE]
  } else if (length(clean_terms) == 2) {
    model_data <- model_data[order(model_data[[1]], model_data[[2]]), , drop = FALSE]
  } else if (length(clean_terms) == 3) {
    model_data <- model_data[
      order(model_data[[1]], model_data[[2]], model_data[[3]]),
      ,
      drop = FALSE
    ]
  } else if (length(clean_terms) == 4) {
    model_data <- model_data[
      order(model_data[[1]], model_data[[2]], model_data[[3]], model_data[[4]]),
      ,
      drop = FALSE
    ]
  }

  model_data
}


.aggregate_simulations <- function(sims, clean_terms, datagrid) {
  stats::aggregate(
    sims,
    lapply(clean_terms, function(i) datagrid[[i]]),
    mean,
    na.rm = TRUE,
    simplify = TRUE
  )
}
