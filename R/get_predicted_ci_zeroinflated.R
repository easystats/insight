.simulate_zi_predictions <- function(model,
                                     newdata,
                                     predictions,
                                     nsim = NULL,
                                     ci = .95) {


  # Since the zero inflation and the conditional model are working in "opposite
  # directions", confidence intervals can not be derived directly  from the
  # "predict()"-function. Thus, confidence intervals are based on quantiles
  # of simulated draws from a multivariate normal distribution
  # (see also _Brooks et al. 2017, pp.391-392_ for details).

  if (is.null(newdata)) {
    newdata <- get_data(model)
  }

  if (is.null(nsim)) {
    nsim <- 1000
  }

  if (inherits(model, "glmmTMB")) {
    out <- .simulate_predictions_glmmTMB(model, newdata, nsim)
  } else if (inherits(model, "MixMod")) {
    out <- .simulate_predictions_MixMod(model, newdata, nsim)
  } else {
    out <- .simulate_predictions_zeroinfl(model, newdata, nsim)
  }

  if (is.null(out)) {
    return(NULL)
  }

  sims <- link_inverse(model)(out$cond) * (1 - stats::plogis(out$zi))
  ci <- (1 + ci) / 2

  simulated <- data.frame(
    SE = apply(sims, 1, stats::sd),
    CI_low = apply(sims, 1, stats::quantile, probs = 1 - ci),
    CI_high = apply(sims, 1, stats::quantile, probs = ci),
    stringsAsFactors = FALSE
  )


  # We need to fix a bit here. We have the simulated standard errors and CI's -
  # but can use the "correct" predictions from "predict(type = "reponse")".
  # in order to make CI and predictions match, we take the simulated CI-range
  # and use the original predicted values as "center" for those CI-ranges.

  ci.range <- (simulated$CI_high - simulated$CI_low) / 2

  # fix lower bound ("center" lower bound related to predicted values)
  ci.low <- predictions - ci.range

  # fix negative CI
  neg.ci <- ci.low < 0
  if (any(neg.ci)) {
    ci.range[neg.ci] <- ci.range[neg.ci] - abs(ci.low[neg.ci]) - 1e-05
    simulated$SE[neg.ci] <- simulated$SE[neg.ci] - ((abs(ci.low[neg.ci]) + 1e-05) / stats::qnorm(ci))
  }

  simulated$CI_low <- predictions - ci.range
  simulated$CI_high <- predictions + ci.range

  simulated[c("SE", "CI_low", "CI_high")]
}





# glmmTMB -------------------


.simulate_predictions_glmmTMB <- function(model, newdata, nsim) {
  check_if_installed("lme4")
  check_if_installed("MASS")

  tryCatch(
    {
      condformula <- lme4::nobars(stats::formula(model)[-2])
      ziformula <- lme4::nobars(stats::formula(model$modelInfo$allForm$ziformula))

      matrix.conditional <- stats::model.matrix(condformula, newdata)
      beta.conditional <- lme4::fixef(model)$cond

      matrix.zero_inflated <- stats::model.matrix(ziformula, newdata)
      beta.zero_inflated <- lme4::fixef(model)$zi

      .get_simulation_from_zi(
        model,
        nsim,
        beta.conditional,
        beta.zero_inflated,
        matrix.conditional,
        matrix.zero_inflated
      )
    },
    error = function(x) {
      NULL
    }
  )
}




# GLMMAdaptive MixMod -------------------


.simulate_predictions_MixMod <- function(model, newdata, nsim) {
  check_if_installed("lme4")
  check_if_installed("MASS")

  tryCatch(
    {
      condformula <- stats::formula(model, type = "fixed")
      ziformula <- stats::formula(model, type = "zi_fixed")

      matrix.conditional <- stats::model.matrix(condformula, newdata)
      beta.conditional <- lme4::fixef(model, sub_model = "main")

      matrix.zero_inflated <- stats::model.matrix(ziformula, newdata)
      beta.zero_inflated <- lme4::fixef(model, sub_model = "zero_part")

      .get_simulation_from_zi(
        model,
        nsim,
        beta.conditional,
        beta.zero_inflated,
        matrix.conditional,
        matrix.zero_inflated
      )
    },
    error = function(x) {
      NULL
    }
  )
}



# pscl::zeroinfl ----------------------


.simulate_predictions_zeroinfl <- function(model, newdata, nsim = 1000) {

  # check for at least to factor levels, in order to build contrasts
  single_factor_levels <- sapply(newdata, function(i) is.factor(i) && nlevels(i) == 1)
  if (any(single_factor_levels)) {
    warning(format_message("Some factors in the data have only one level. Cannot compute model matrix for standard errors and confidence intervals."), call. = FALSE)
    return(NULL)
  }

  tryCatch(
    {
      condformula <- stats::as.formula(paste0("~", safe_deparse(stats::formula(model)[[3]][[2]])))
      ziformula <- stats::as.formula(paste0("~", safe_deparse(stats::formula(model)[[3]][[3]])))

      matrix.conditional <- stats::model.matrix(condformula, model = "count", data = newdata)
      beta.conditional <- stats::coef(model, model = "count")

      matrix.zero_inflated <- stats::model.matrix(ziformula, model = "zero", data = newdata)
      beta.zero_inflated <- stats::coef(model, model = "zero")

      .get_simulation_from_zi(
        model,
        nsim,
        beta.conditional,
        beta.zero_inflated,
        matrix.conditional,
        matrix.zero_inflated
      )
    },
    error = function(x) {
      NULL
    }
  )
}





# gam --------------------


.get_zeroinfl_gam_predictions <- function(model, newdata, nsim = 1000) {
  tryCatch(
    {
      mm <- stats::model.matrix(model, data = newdata)

      linpred <- attr(mm, "lpi", exact = TRUE)
      cond <- linpred[[1]]
      zi <- linpred[[2]]

      matrix.conditional <- mm[, cond]
      matrix.zero_inflated <- mm[, zi]

      beta.conditional <- stats::coef(model)[cond]
      beta.zero_inflated <- stats::coef(model)[zi]

      varcov.cond <- stats::vcov(model)[cond, cond]
      varcov.zi <- stats::vcov(model)[zi, zi]

      psim.cond <- MASS::mvrnorm(nsim, mu = beta.conditional, Sigma = varcov.cond)
      pred.cond <- matrix.conditional %*% t(psim.cond)

      psim.zi <- MASS::mvrnorm(nsim, mu = beta.zero_inflated, Sigma = varcov.zi)
      pred.zi <- matrix.zero_inflated %*% t(psim.zi)

      list(cond = pred.cond, zi = pred.zi)
    },
    error = function(x) {
      NULL
    }
  )
}



# helper -----------------


.get_simulation_from_zi <- function(model,
                                    nsim,
                                    beta.conditional,
                                    beta.zero_inflated,
                                    matrix.conditional,
                                    matrix.zero_inflated) {


  # if formula has a polynomial term, and this term is one that is held
  # constant, model.matrix() with "newdata" will throw an error - so we
  # re-build the newdata-argument by including all values for poly-terms, if
  # these are hold constant.

  # fixes <- .rows_to_keep(
  #   model,
  #   newdata,
  #   condformula,
  #   ziformula,
  #   terms,
  #   value_adjustment,
  #   condition
  # )
  #
  # if (!is.null(fixes)) {
  #   keep <- fixes$keep
  #   newdata <- fixes$newdata
  # } else {
  #   keep <- NULL
  # }

  cond.varcov <- get_varcov(model, component = "conditional")
  zi.varcov <- get_varcov(model, component = "zero_inflated")

  pred.condpar.psim <- MASS::mvrnorm(n = nsim, mu = beta.conditional, Sigma = cond.varcov)
  pred.cond.psim <- matrix.conditional %*% t(pred.condpar.psim)

  pred.zipar.psim <- MASS::mvrnorm(n = nsim, mu = beta.zero_inflated, Sigma = zi.varcov)
  pred.zi.psim <- matrix.zero_inflated %*% t(pred.zipar.psim)

  # if (!.is_empty(keep)) {
  #   pred.cond.psim <- pred.cond.psim[keep, , drop = FALSE]
  #   pred.zi.psim <- pred.zi.psim[keep, , drop = FALSE]
  # }

  list(cond = pred.cond.psim, zi = pred.zi.psim)
}
