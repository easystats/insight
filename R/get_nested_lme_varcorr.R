# Caution! this is somewhat experimental...
# It retrieves the variance-covariance matrix of random effects
# from nested lme-models.
.get_nested_lme_varcorr <- function(x) {
  check_if_installed("lme4")

  vcor <- lme4::VarCorr(x)
  class(vcor) <- "matrix"

  ## FIXME: doesn't work for nested RE from MASS::glmmPQL, see Nakagawa example
  # each block starts with a header row like "Dog =", so a new block starts
  # at every header row but the first. Blocks need not have an intercept.
  re_index <- which(endsWith(rownames(vcor), "="))[-1]
  vc_list <- split(
    data.frame(vcor, stringsAsFactors = FALSE),
    findInterval(seq_len(nrow(vcor)), re_index)
  )
  vc_rownames <- split(rownames(vcor), findInterval(seq_len(nrow(vcor)), re_index))
  re_pars <- unique(unlist(find_parameters(x)["random"]))
  # blocks are ordered from outermost to innermost group, and each block
  # starts with a header row like "Dog ="
  re_names <- trimws(sub("=$", "", rownames(vcor)[endsWith(rownames(vcor), "=")]))

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
      variances <- suppressWarnings(as.numeric(x[vl, "Variance"]))
      # covariances are zero unless the block has correlations
      m1 <- diag(variances, nrow = sum(vl))
      rownames(m1) <- rownames(x)[vl]
      colnames(m1) <- rownames(x)[vl]

      if (length(g_cor) && nrow(m1) > 1) {
        # the correlations are a lower triangle: row i holds the correlations
        # with terms 1 to i - 1, starting in the "Corr" column
        corr_cols <- which(colnames(x) == "Corr"):ncol(x)
        r <- suppressWarnings(matrix(
          as.numeric(as.matrix(x[vl, corr_cols, drop = FALSE])),
          nrow = nrow(m1)
        ))
        for (i in 2:nrow(m1)) {
          for (k in seq_len(min(i - 1, ncol(r)))) {
            if (!is.na(r[i, k])) {
              m1[i, k] <- m1[k, i] <- r[i, k] * sqrt(m1[i, i] * m1[k, k])
            }
          }
        }
      }

      # a block without an intercept has no slope-intercept correlation
      if ("(Intercept)" %in% rownames(m1)) {
        attr(m1, "cor_slope_intercept") <- g_cor
      }
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
