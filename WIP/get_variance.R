    switch(familyName,
      gaussian = r2glmm(fam, varFE, varRE, varResid = sigma2(object)^2),
      binomial = ,
      quasibinomial = {
        vt <- .varRESum(.varcorr(null), mmRE)
        pmean <- fam$linkinv(fixefnull - 0.5 * vt * tanh(fixefnull *
          (1 + 2 * exp(-0.5 * vt)) / 6))
        r2glmm(fam, varFE, varRE, pmean = pmean, n = .binomial.sample.size(object))
      },
      nbinom2 = {
        vt <- .varRESum(.varcorr(null), mmRE)
        lambda <- unname(exp(fixefnull + 0.5 * vt))
        theta <- sigma2(object)
        r2glmm(familyName, varFE, varRE,
          lambda = lambda,
          omega = theta, link = fam$link
        )
      },
      Gamma = {
        nu <- sigma2(object)^-2
        omega <- 1
        r2glmm(fam, varFE, varRE, lambda = nu, omega = omega)
      },
      quasipoisson = ,
      nbinom1 = {
        vt <- .varRESum(.varcorr(null), mmRE)
        lambda <- unname(exp(fixefnull + 0.5 * vt))
        omega <- sigma2(object)
        r2glmm(fam, varFE, varRE, lambda = lambda, omega = omega)
      },
      poisson = {
        vt <- .varRESum(.varcorr(null), mmRE)
        lambda <- unname(exp(fixefnull + 0.5 * vt))
        omega <- 1
        rval <- r2glmm(fam, varFE, varRE,
          lambda = lambda,
          omega = omega
        )
        if (inherits(object, "merMod") && familyName == "poisson" &&
          pj2014) {
          xo <- .OLREFit(object)
          vc <- .varcorr(xo)
          fe <- .numfixef(xo)
          ok <- !is.na(fe)
          fitted <- (model.matrix(xo)[, ok, drop = FALSE] %*%
            fe[ok])[, 1L]
          n <- nrow(mmRE)
          vname <- names(xo@flist)[sapply(xo@flist, nlevels) ==
            n][1L]
          if (!vname %in% names(vc)) vname <- make.names(vname)
          stopifnot(vname %in% names(vc))
          varresid <- vc[[vname]][1L]
          rval <- rbind(pj2014 = r2glmm(fam, var(fitted),
            .varRESum(vc, mmRE) - varresid,
            varResid = log1p(1 / exp(mean(fitted))) +
              varresid
          )[1L, ], rval)
        }
        rval
      },
      {
        varResid <- insight::get_variance_residual(
          object,
          ...
        )[[1L]]
        if (!is.finite(varResid)) warning("residual variance cannot be calculated.")
        r2glmm(fam, varFE, varRE, varResid)
      }
    )

.varRESum <- function(vc, X) {
  if (is.null(vc)) {
    return(0)
  }
  n <- nrow(X)
  sum(sapply(vc, function(sig) {
    mm1 <- X[, rownames(sig), drop = FALSE]
    sum(matmultdiag(mm1 %*% sig, ty = mm1)) / n
  }))
}

r2glmm <- function(
    family, varFE, varRE, varResid, link, pmean, lambda,
    omega, n) {
  if (inherits(family, "family")) {
    link <- family$link
    family <- family$family
  }
  if (missing(varResid) || !is.numeric(varResid) || (is.na(varResid) &&
    !is.nan(varResid))) {
    varResid <- switch(paste(family, link, sep = "."),
      gaussian.identity = varResid,
      quasibinomial.logit = ,
      binomial.logit = c(
        theoretical = 3.28986813369645 / n,
        delta = 1 / (n * pmean * (1 - pmean))
      ),
      quasibinomial.probit = ,
      binomial.probit = c(theoretical = 1 / n, delta = 6.28318530717959 / n *
        pmean * (1 - pmean) * exp((qnorm(pmean) / 1.4142135623731)^2)^2),
      quasibinomial.cloglog = ,
      binomial.cloglog = c(
        theoretical = 1.64493406684823 / n,
        delta = pmean / n / log(1 - pmean)^2 / (1 - pmean)
      ),
      Gamma.log = ,
      poisson.log = ,
      quasipoisson.log = ,
      nbinom1.log = c(
        delta = omega / lambda, lognormal = log1p(omega / lambda),
        trigamma = trigamma(lambda / omega)
      ),
      quasipoisson.sqrt = ,
      nbinom1.sqrt = ,
      `poisson.mu^0.5` = ,
      poisson.sqrt = c(delta = 0.25 *
        omega),
      nbinom2.log = {
        vdelta <- (1 / lambda) + (1 / omega)
        c(
          delta = vdelta, lognormal = log1p(vdelta),
          trigamma = trigamma(1 / vdelta)
        )
      },
      NotImplementedFamily = stop(
        "not implemented yet for ",
        family, " and ", link
      ),
      {
        cry(
          sys.call(-1L), "do not know how to calculate variance for %s(%s)",
          family, dQuote(link)
        )
      }
    )
  }
  vtot <- sum(varFE, varRE)
  matrix(c(varFE, vtot) / (vtot + rep(varResid, each = 2L)),
    ncol = 2L, byrow = TRUE, dimnames = list(
      names(varResid),
      c("R2m", "R2c")
    )
  )
}
