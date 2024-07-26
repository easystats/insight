# Caution! this is somewhat experimental...
# It retrieves the variance-covariance matrix of random effects
# from nested lme-models.
.get_nested_lme_varcorr <- function(x) {
  check_if_installed("lme4")

  vcor <- lme4::VarCorr(x)
  class(vcor) <- "matrix"

  ## FIXME: doesn't work for nested RE from MASS::glmmPQL, see Nakagawa example
  re_index <- (which(rownames(vcor) == "(Intercept)") - 1)[-1]
  vc_list <- split(data.frame(vcor, stringsAsFactors = FALSE), findInterval(seq_len(nrow(vcor)), re_index))
  vc_rownames <- split(rownames(vcor), findInterval(seq_len(nrow(vcor)), re_index))
  re_pars <- unique(unlist(find_parameters(x)["random"]))
  re_names <- find_random(x, split_nested = TRUE, flatten = TRUE)

  names(vc_list) <- re_names

  mapply(
    function(x, y) {
      if ("Corr" %in% colnames(x)) {
        g_cor <- suppressWarnings(stats::na.omit(as.numeric(x[, "Corr"])))
      } else {
        g_cor <- NULL
      }
      row.names(x) <- as.vector(y)
      vl <- rownames(x) %in% re_pars
      x <- suppressWarnings(apply(x[vl, vl, drop = FALSE], MARGIN = c(1, 2), FUN = as.numeric))
      m1 <- matrix(, nrow = nrow(x), ncol = ncol(x))
      m1[seq_len(nrow(m1)), seq_len(ncol(m1))] <- as.vector(x[, 1])
      rownames(m1) <- rownames(x)
      colnames(m1) <- rownames(x)

      if (!is.null(g_cor)) {
        m1_cov <- sqrt(prod(diag(m1))) * g_cor
        for (j in seq_len(ncol(m1))) {
          m1[j, nrow(m1) - j + 1] <- m1_cov[1]
        }
      }

      attr(m1, "cor_slope_intercept") <- g_cor
      m1
    },
    vc_list,
    vc_rownames,
    SIMPLIFY = FALSE
  )
}


.is_nested_lme <- function(x) {
  if (inherits(x, "glmmPQL")) {
    length(find_random(x, flatten = TRUE)) > 1
  } else {
    sapply(find_random(x), function(i) any(grepl(":", i, fixed = TRUE)))
  }
}
