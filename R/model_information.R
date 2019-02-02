#' @title Access information from model objects
#' @name model_info
#'
#' @description Retrieve information from model objects.
#'
#' @inheritParams find_predictors
#' @inheritParams link_inverse
#'
#' @return A list with information about the model, like family, link-function
#'   etc. (see 'Details').
#'
#' @details \code{model_info()} returns a list with information about the
#'   model for many different model objects. Following information
#'    is returned, where all values starting with \code{is_} are logicals.
#'    \itemize{
#'      \item \code{is_bin}: family is binomial (but not negative binomial)
#'      \item \code{is_pois}: family is poisson
#'      \item \code{is_negbin}: family is negative binomial
#'      \item \code{is_count}: model is a count model (i.e. family is either poisson or negative binomial)
#'      \item \code{is_beta}: family is beta
#'      \item \code{is_logit}: model has logit link
#'      \item \code{is_linear}: family is gaussian
#'      \item \code{is_ordinal}: family is ordinal or cumulative link
#'      \item \code{is_categorical}: family is categorical link
#'      \item \code{is_zeroinf}: model has zero-inflation component
#'      \item \code{is_multivariate}: model is a multivariate response model (currently only works for \emph{brmsfit} objects)
#'      \item \code{is_trial}: model response contains additional information about the trials
#'      \item \code{is_bayes}: model is a Bayesian model
#'      \item \code{link_fun}: the link-function
#'      \item \code{family}: the family-object
#'      \item \code{nobs}: number of observations
#'      \item \code{model_terms}: a list with all model terms, including terms such as random effects or from zero-inflated model parts.
#'    }
#'
#' @examples
#' library(glmmTMB)
#' data("Salamanders")
#' m <- glmmTMB(
#'   count ~ spp + cover + mined + (1 | site),
#'   ziformula = ~spp + mined,
#'   dispformula = ~DOY,
#'   data = Salamanders,
#'   family = nbinom2
#' )
#'
#' model_info(m)
#'
#' @importFrom stats formula terms
#' @export
model_info <- function(x, ...) {
  UseMethod("model_info")
}


#' @importFrom stats family
#' @export
model_info.default <- function(x, ...) {
  tryCatch(
    {
      if (inherits(x, c("Zelig-relogit")))
        faminfo <- stats::binomial(link = "logit")
      else
        faminfo <- stats::family(x)
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.glmmPQL <- function(x, ...) {
  tryCatch(
    {
      faminfo <- x$family
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.MixMod <- function(x, ...) {
  tryCatch(
    {
      faminfo <- x$family
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.lme <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.plm <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.gls <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.truncreg <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.lmRob <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.gam <- function(x, ...) {
  tryCatch(
    {
      if (!inherits(x, c("glm", "lm")))
        class(x) <- c(class(x), "glm", "lm")

      faminfo <- stats::family(x)
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.vgam <- function(x, ...) {
  tryCatch(
    {
      faminfo <- x@family
      link.fun <- faminfo@blurb[3]
      if (grepl("^\\Qlogit(\\E", link.fun, perl = TRUE)) link.fun <- "logit"
      make_family(
        x = x,
        fitfam = faminfo@vfamily[1],
        logit.link = string_contains(faminfo@blurb, "logit"),
        link.fun = link.fun,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.vglm <- function(x, ...) {
  tryCatch(
    {
      faminfo <- x@family
      link.fun <- faminfo@blurb[3]
      if (grepl("^\\Qlogit(\\E", link.fun, perl = TRUE)) link.fun <- "logit"
      make_family(
        x = x,
        fitfam = faminfo@vfamily[1],
        logit.link = string_contains(faminfo@blurb, "logit"),
        link.fun = link.fun,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.zeroinfl <- function(x, ...) {
  tryCatch(
    {
      if (is.list(x$dist))
        dist <- x$dist[[1]]
      else
        dist <- x$dist
      fitfam <- switch(
        dist,
        poisson = "poisson",
        negbin = "negative binomial",
        "poisson"
      )
      make_family(
        x = x,
        fitfam = fitfam,
        zero.inf = TRUE,
        link.fun = "log",
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.hurdle <- function(x, ...) {
  tryCatch(
    {
      if (is.list(x$dist))
        dist <- x$dist[[1]]
      else
        dist <- x$dist
      fitfam <- switch(
        dist,
        poisson = "poisson",
        negbin = "negative binomial",
        "poisson"
      )
      make_family(
        x = x,
        fitfam = fitfam,
        zero.inf = TRUE,
        link.fun = "log",
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.zerotrunc <- function(x, ...) {
  tryCatch(
    {
      if (is.list(x$dist))
        dist <- x$dist[[1]]
      else
        dist <- x$dist
      fitfam <- switch(
        dist,
        poisson = "poisson",
        negbin = "negative binomial",
        "poisson"
      )
      make_family(
        x = x,
        fitfam = fitfam,
        zero.inf = TRUE,
        link.fun = "log",
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.glmmTMB <- function(x, ...) {
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("To use this function, please install package 'lme4'.")

  tryCatch(
    {
      faminfo <- stats::family(x)
      make_family(
        x = x,
        fitfam = faminfo$family,
        zero.inf = !is_empty_object(lme4::fixef(x)$zi),
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.lm_robust <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.felm <- function(x, ...) {
  make_family(x, ...)
}


#' @export
model_info.betareg <- function(x, ...) {
  tryCatch(
    {
      make_family(
        x = x,
        fitfam = "beta",
        logit.link = x$link$mean$name == "logit",
        link.fun = x$link$mean$linkfun,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.coxph <- function(x, ...) {
  tryCatch(
    {
      make_family(
        x = x,
        fitfam = "survival",
        logit.link = TRUE,
        link.fun = NULL,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.coxme <- function(x, ...) {
  tryCatch(
    {
      make_family(
        x = x,
        fitfam = "survival",
        logit.link = TRUE,
        link.fun = NULL,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.MCMCglmm <- function(x, ...) {
  tryCatch(
    {
      make_family(
        x = x,
        fitfam = x$Residual$family,
        logit.link = FALSE,
        link.fun = "",
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.lrm <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.polr <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.multinom <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.clm2 <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.clm <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.clmm <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.mlogit <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.logistf <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.gmnl <- function(x, ...) {
  tryCatch(
    {
      faminfo <- stats::binomial(link = "logit")
      make_family(
        x = x,
        fitfam = faminfo$family,
        logit.link = faminfo$link == "logit",
        link.fun = faminfo$link,
        ...
      )
    },
    error = function(x) { NULL }
  )
}


#' @rdname model_info
#' @export
model_info.brmsfit <- function(x, mv_response = FALSE, ...) {
  tryCatch(
    {
      faminfo <- stats::family(x)
      if (!is.null(stats::formula(x)$response)) {
        multi.var <- TRUE
        if (!mv_response) faminfo <- faminfo[[1]]
      }

      if (mv_response && multi.var) {
        lapply(faminfo, function(.x) {
          make_family(
            x = x,
            fitfam = .x$family,
            zero.inf = FALSE,
            .x$link == "logit",
            multi.var = TRUE,
            link.fun = .x$link,
            ...
          )
        })
      } else {
        make_family(
          x = x,
          fitfam = faminfo$family,
          logit.link = faminfo$link == "logit",
          multi.var = multi.var,
          link.fun = faminfo$link,
          ...
        )
      }

    },
    error = function(x) { NULL }
  )
}


#' @export
model_info.stanmvreg <- function(x, mv_response = FALSE, ...) {
  tryCatch(
    {
      faminfo <- stats::family(x)
      multi.var <- TRUE
      if (!mv_response) faminfo <- faminfo[[1]]

      if (mv_response && multi.var) {
        lapply(faminfo, function(.x) {
          make_family(
            x = x,
            fitfam = .x$family,
            zero.inf = FALSE,
            .x$link == "logit",
            multi.var = TRUE,
            link.fun = .x$link,
            ...
          )
        })
      } else {
        make_family(
          x = x,
          fitfam = faminfo$family,
          logit.link = faminfo$link == "logit",
          multi.var = multi.var,
          link.fun = faminfo$link,
          ...
        )
      }

    },
    error = function(x) { NULL }
  )
}


make_family <- function(x, fitfam = "gaussian", zero.inf = FALSE, logit.link = FALSE, multi.var = FALSE, link.fun = "identity", ...) {
  # create logical for family
  binom_fam <-
    fitfam %in% c("bernoulli", "binomial", "quasibinomial", "binomialff") |
    grepl("\\Qbinomial\\E", fitfam, ignore.case = TRUE)

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson", "genpois", "ziplss") |
    grepl("\\Qpoisson\\E", fitfam, ignore.case = TRUE)

  neg_bin_fam <-
    grepl("\\Qnegative binomial\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qnbinom\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qnzbinom\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qgenpois\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qnegbinomial\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE)

  beta_fam <- inherits(x, "betareg") | fitfam %in% c("beta")

  linear_model <- !binom_fam & !poisson_fam & !neg_bin_fam & !logit.link

  zero.inf <- zero.inf | fitfam == "ziplss" |
    grepl("\\Qzero_inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qzero-inflated\\E", fitfam, ignore.case = TRUE) |
    grepl("\\Qneg_binomial\\E", fitfam, ignore.case = TRUE)
    grepl("\\Qhurdle\\E", fitfam, ignore.case = TRUE) |
    grepl("^(zt|zi|za|hu)", fitfam, perl = TRUE)

  is.ordinal <-
    inherits(x, c("polr", "clm", "clm2", "clmm", "gmnl", "mlogit", "multinom")) |
    fitfam %in% c("cumulative", "cratio", "sratio", "acat", "ordinal", "multinomial")

  is.categorical <- fitfam == "categorical"


  # check if we have binomial models with trials instead of binary outcome

  is.trial <- FALSE

  if (inherits(x, "brmsfit") && is.null(stats::formula(x)$responses)) {
    tryCatch(
      {
        rv <- deparse(stats::formula(x)$formula[[2L]], width.cutoff = 500L)
        is.trial <- trim(sub("(.*)\\|(.*)\\(([^,)]*).*", "\\2", rv)) %in% c("trials", "resp_trials")
      },
      error = function(x) { NULL }
    )
  }

  if (binom_fam && !inherits(x, "brmsfit")) {
    tryCatch(
      {
        rv <- deparse(stats::formula(x)[[2L]], width.cutoff = 500L)
        is.trial <- grepl("cbind\\((.*)\\)", rv)
      },
      error = function(x) { NULL }
    )
  }

  dots <- list(...)
  if (obj_has_name(dots, "no_terms") && isTRUE(dots$no_terms)) {
    model_terms <- NULL
  } else {
    model_terms <- tryCatch(
      {find_terms(x, effects = "all", component = "all", flatten = FALSE)},
      error = function(x) { NULL }
    )
  }


  list(
    is_bin = binom_fam & !neg_bin_fam,
    is_count = poisson_fam | neg_bin_fam,
    is_pois = poisson_fam,
    is_negbin = neg_bin_fam,
    is_beta = beta_fam,
    is_logit = logit.link,
    is_linear = linear_model,
    is_zeroinf = zero.inf,
    is_ordinal = is.ordinal,
    is_categorical = is.categorical,
    is_multivariate = multi.var,
    is_trial = is.trial,
    is_bayes = inherits(x, c("brmsfit", "stanfit", "stanreg", "stanmvreg")),
    link_fun = link.fun,
    family = fitfam,
    nobs = n_obs(x),
    model_terms = model_terms
  )
}
