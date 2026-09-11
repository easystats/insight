# glmmTMB ordinal family ------------------------------------------------
# =======================================================================

# glmmTMB's `ordinal()` family stores the K-1 thresholds as family-specific
# parameters ("psi", softmax-parameterized so that they stay ordered) rather
# than as fixed effects, and fixes the intercept to zero via an internal map.
# To match `ordinal::clm()`/`clmm()`, we report the thresholds (on the
# threshold scale) as conditional parameters ahead of the slopes, drop the
# non-estimated intercept, and transform the covariance matrix with the
# delta method. `vcov(x, full = TRUE)` is on the internal psi scale.

.is_glmmtmb_ordinal <- function(x) {
  inherits(x, "glmmTMB") &&
    identical(.safe(x$modelInfo$family$family), "ordinal")
}


# is the intercept fixed to zero by glmmTMB's internal map, i.e. not
# estimated? This is the default unless the user supplied a beta map
.glmmtmb_fixed_intercept <- function(x) {
  cf <- .safe(lme4::fixef(x)$cond)
  bmap <- .safe(x$obj$env$map$beta)
  icpt <- which(names(cf) == "(Intercept)")
  length(icpt) == 1L && !is.null(bmap) && is.na(bmap[icpt])
}


# named vector of thresholds followed by the estimated fixed effects
.glmmtmb_ordinal_conditional <- function(x) {
  cf <- lme4::fixef(x)$cond
  if (.glmmtmb_fixed_intercept(x)) {
    cf <- cf[names(cf) != "(Intercept)"]
  }
  c(glmmTMB::family_params(x), cf)
}


# covariance matrix of c(thresholds, fixed effects) on the threshold scale.
# theta_j = qlogis(cumsum(softmax(c(psi, 0)))_j) is a joint function of all
# psi elements, so the Jacobian is J[j, m] = s[m] * ((m <= j) - C_j) /
# (C_j * (1 - C_j)), with s = softmax(c(psi, 0)) and C_j = cumsum(s)[j]
.glmmtmb_ordinal_varcov <- function(x) {
  V <- .safe_vcov(x, full = TRUE)
  thresholds <- glmmTMB::family_params(x)
  cf <- lme4::fixef(x)$cond

  # internal parameter ids ("cond1", "psi1", ...) are carried as names of the
  # row labels; fall back to the labels if these are missing
  ids <- names(rownames(V))
  if (is.null(ids)) {
    psi_i <- match(names(thresholds), rownames(V))
    cond_i <- match(names(cf), rownames(V))
  } else {
    psi_i <- which(startsWith(ids, "psi"))
    cond_i <- which(startsWith(ids, "cond"))
  }
  # non-estimated parameters (the mapped intercept) have NA variance
  cond_i <- cond_i[!is.na(cond_i)]
  cond_i <- cond_i[!is.na(diag(V)[cond_i])]

  psi <- unname(x$fit$par[names(x$fit$par) == "psi"])
  k <- length(psi)
  w <- exp(c(psi, 0) - max(psi, 0))
  s <- w / sum(w)
  Cj <- cumsum(s)[seq_len(k)]
  J_psi <- outer(
    seq_len(k),
    seq_len(k),
    function(j, m) s[m] * ((m <= j) - Cj[j]) / (Cj[j] * (1 - Cj[j]))
  )

  J <- diag(k + length(cond_i))
  J[seq_len(k), seq_len(k)] <- J_psi
  idx <- c(psi_i, cond_i)
  out <- J %*% V[idx, idx, drop = FALSE] %*% t(J)
  nms <- c(names(thresholds), unname(rownames(V)[cond_i]))
  dimnames(out) <- list(nms, nms)
  out
}


# per-category probabilities (or the most likely category) from
# `predict(type = "probs")`, reshaped to long format like `get_predicted.clm()`
.get_predicted_glmmtmb_ordinal <- function(
  x,
  data = NULL,
  predict = "expectation",
  ci = NULL,
  include_random = "default",
  verbose = TRUE,
  dots = list()
) {
  # `type` takes precedence over `predict`, as in `.get_predicted_args()`
  if (is.null(dots$type)) {
    requested <- predict[1]
  } else {
    requested <- dots$type[1]
  }
  classification <- identical(requested, "classification")

  if (requested %in% c("prediction", "predicted") && verbose) {
    format_warning(
      "\"prediction\" is currently not supported by the `predict` argument for `glmmTMB` models.",
      "Changing to `predict=\"expectation\"`."
    )
  }
  if (classification && !is.null(ci)) {
    if (verbose) {
      format_warning("Confidence intervals are not available for classification.")
    }
    ci <- NULL
  }

  # sanitize input. This also handles the `predict`/`type` alert and the
  # aliases; the type passed to `predict()` is always "probs"
  my_args <- do.call(
    .get_predicted_args,
    c(
      list(
        x,
        data = data,
        predict = predict,
        ci = ci,
        include_random = include_random,
        verbose = verbose
      ),
      dots
    )
  )
  my_args$predict <- ifelse(classification, "classification", "expectation")

  # remaining dot-arguments are forwarded to `predict()`, as for other
  # families; those managed here take precedence
  predict_args <- list(
    x,
    newdata = my_args$data,
    type = "probs",
    re.form = my_args$re.form,
    allow.new.levels = my_args$allow_new_levels,
    se.fit = !classification
  )
  dots[c("type", "newdata", "re.form", "allow.new.levels", "se.fit")] <- NULL
  rez <- do.call(stats::predict, c(predict_args, dots))
  if (classification) {
    probs <- rez
  } else {
    probs <- rez$fit
  }
  resp_levels <- colnames(probs)

  # the response is not a focal predictor of the long-format output
  resp <- find_response(x)
  if (!is.null(my_args$data) && !is.null(resp)) {
    my_args$data <- my_args$data[, setdiff(colnames(my_args$data), resp), drop = FALSE]
  }

  if (classification) {
    out <- factor(
      resp_levels[max.col(probs, ties.method = "first")],
      levels = resp_levels
    )
    return(.get_predicted_out(out, my_args = my_args))
  }

  # predictions matrix to long format (column-major, i.e. by response level)
  out <- .get_predicted_out(probs, my_args = my_args)
  ci_data <- data.frame(
    Row = out$Row,
    Response = out$Response,
    SE = as.vector(rez$se.fit),
    stringsAsFactors = FALSE
  )

  # intervals on the logit scale via the delta method, as in
  # `ordinal:::predict.clm()`, so that they stay within [0, 1]
  if (!is.null(ci) && !is.na(ci)) {
    crit_val <- stats::qnorm((1 + ci) / 2)
    p <- out$Predicted
    se_logit <- ci_data$SE / (p * (1 - p))
    ci_data$CI_low <- stats::plogis(stats::qlogis(p) - crit_val * se_logit)
    ci_data$CI_high <- stats::plogis(stats::qlogis(p) + crit_val * se_logit)
  }

  attr(out, "ci_data") <- ci_data
  out
}
