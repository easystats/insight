#' p-values formatting
#'
#' Format p-values.
#'
#' @param p value or vector of p-values.
#' @param stars Add significance stars (e.g., p < .001***).
#' @param stars_only Return only significance stars.
#' @param name Name prefixing the text. Can be \code{NULL}.
#' @param digits Number of significant digits. May also be \code{"scientific"}
#'   to return exact p-values in scientific notation, or \code{"apa"} to use
#'   an APA-style for p-values (e.g., p < .01, p < .05, etc.). If
#'   \code{"scientific"}, control the number of digits by adding the value as
#'   suffix, e.g. \code{digits = "scientific4"} to have scientific notation
#'   with 4 decimal places.
#' @param ... Arguments from other methods.
#' @inheritParams format_value
#'
#' @return A formatted string.
#' @examples
#' format_p(c(.02, .065, 0, .23))
#' format_p(c(.02, .065, 0, .23), name = NULL)
#' format_p(c(.02, .065, 0, .23), stars_only = TRUE)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' p <- coef(summary(model))[, 4]
#' format_p(p, digits = "apa")
#' format_p(p, digits = "scientific")
#' format_p(p, digits = "scientific2")
#' @export
format_p <- function(p, stars = FALSE, stars_only = FALSE, name = "p", missing = "", digits = 3, ...) {

  # only convert p if it's a valid numeric, or at least coercible to
  # valid numeric values...
  if (!is.numeric(p)) {
    if (.is_numeric_character(p)) {
      p <- .factor_to_numeric(p)
    } else {
      return(p)
    }
  }


  if (identical(stars, "only")) {
    stars <- TRUE
    stars_only <- TRUE
  }

  if (digits == "apa") {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, "< .001***",
        ifelse(p < 0.01, "< .01**",
          ifelse(p < 0.05, "< .05*",
            ifelse(p > .999, "> .999",
              paste0("= ", format_value(p, 3))
            )
          )
        )
      )
    )
  } else if (is.character(digits) && grepl("^scientific", digits)) {
    digits <- tryCatch(
      {
        as.numeric(gsub("scientific", "", digits, fixed = TRUE))
      },
      error = function(e) {
        NA
      }
    )
    if (is.na(digits)) {
      digits <- 5
    }
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, sprintf("= %.*e***", digits, p),
        ifelse(p < 0.01, sprintf("= %.*e**", digits, p),
          ifelse(p < 0.05, sprintf("= %.*e*", digits, p),
            ifelse(p > 0.999, sprintf("= %.*e", digits, p),
              sprintf("= %.*e", digits, p)
            )
          )
        )
      )
    )
  } else if (digits <= 3) {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, "< .001***",
        ifelse(p < 0.01, paste0("= ", format_value(p, digits), "**"),
          ifelse(p < 0.05, paste0("= ", format_value(p, digits), "*"),
            ifelse(p > 0.999, "> .999",
              paste0("= ", format_value(p, digits))
            )
          )
        )
      )
    )
  } else {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, paste0("= ", format_value(p, digits), "***"),
        ifelse(p < 0.01, paste0("= ", format_value(p, digits), "**"),
          ifelse(p < 0.05, paste0("= ", format_value(p, digits), "*"),
            paste0("= ", format_value(p, digits))
          )
        )
      )
    )
  }

  .add_prefix_and_remove_stars(text, stars, stars_only, name, missing)
}


#' @keywords internal
.add_prefix_and_remove_stars <- function(text, stars, stars_only, name, missing = "") {
  missing_index <- is.na(text)

  if (is.null(name)) {
    text <- gsub("= ", "", text)
  } else {
    text <- paste(name, text)
  }

  if (stars_only == TRUE) {
    text <- gsub("[^\\*]", "", text)
  } else if (stars == FALSE) {
    text <- gsub("\\*", "", text)
  }

  text[missing_index] <- missing
  text
}
