# encodeB64 -----------------------------------------------------------------

#' Encode all data frame column names to Base64
#'
#' Renames every column to its `jmvcore::toB64()` equivalent and stores the
#' original name as the `"label"` attribute so gtsummary picks it up
#' automatically for table display.
#'
#' @param data A data frame (typically `self$data`)
#' @return The data frame with B64-encoded column names and label attributes
encodeB64 <- function(data) {
  origNames <- names(data)

  data[] <- lapply(origNames, function(col) {
    x <- data[[col]]
    attr(x, "label") <- col
    x
  })

  names(data) <- jmvcore::toB64(origNames)
  data
}


# buildB64Map ---------------------------------------------------------------

#' Build a B64-to-original-name mapping
#'
#' Returns a named character vector where names are B64-encoded strings and
#' values are the original human-readable names.
#'
#' @param origNames Character vector of original variable names
#' @return Named character vector: `c(b64name = "Original Name", ...)`
buildB64Map <- function(origNames) {
  stats::setNames(origNames, jmvcore::toB64(origNames))
}


# decodeB64 -----------------------------------------------------------------

#' Replace B64 tokens in text with original variable names
#'
#' Scans a character vector for B64-encoded tokens and substitutes the
#' original names. Replacements are applied **longest B64 key first** to
#' prevent partial-match corruption (e.g. B64 for "cat" is a prefix of
#' B64 for "cats").
#'
#' @param text Character vector of strings to decode
#' @param b64Map Named character vector from `buildB64Map()`
#' @return Character vector with B64 tokens replaced
decodeB64 <- function(text, b64Map) {
  if (length(text) == 0) {
    return(text)
  }

  # Sort by key length descending — longest first
  ord <- order(nchar(names(b64Map)), decreasing = TRUE)

  for (i in ord) {
    text <- gsub(names(b64Map)[i], b64Map[i], text, fixed = TRUE)
  }

  text
}


# decodeB64Error ------------------------------------------------------------

#' Decode B64 tokens in an error and re-signal it
#'
#' Intercepts an error condition, replaces any B64-encoded variable names
#' in its `$message` with the originals, and re-signals via `stop()`.
#'
#' @param err An error condition object
#' @param b64Map Named character vector from `buildB64Map()`
decodeB64Error <- function(err, b64Map) {
  err$message <- decodeB64(err$message, b64Map)
  stop(err)
}


# fixPolynomialLabels -------------------------------------------------------

#' Fix labels for polynomial terms in a gtsummary table
#'
#' Detects `I(b64name^N)` patterns in the `variable` column of
#' `table_body` and locates the translated name inside the existing label
#' to append a Unicode superscript digit (e.g. `"Patient Age"` becomes
#' `"Patient Age\u00B2"`). This safely preserves gtsummary's native
#' interaction formatting (e.g. `*`).
#'
#' @param tbl A gtsummary table object
#' @param b64Map Named character vector from `buildB64Map()`
#' @return The table with corrected polynomial labels
fixPolynomialLabels <- function(tbl, b64Map) {
  gtsummary::modify_table_body(tbl, function(body) {
    for (i in seq_len(nrow(body))) {
      var <- body$variable[i]

      # Extract ALL polynomial terms from the variable string natively
      # gregexpr finds all matches of I(something^digits)
      m <- regmatches(var, gregexpr("I\\([^\\^]+\\^\\d+\\)", var))[[1]]

      if (length(m) > 0) {
        newLabel <- body$label[i]
        newVarLabel <- body$var_label[i]

        for (term in m) {
          # term is exactly "I(b64Name^power)". Strip "I(" and ")"
          clean_term <- substr(term, 3, nchar(term) - 1)

          # Split at the "^" symbol
          parts <- strsplit(clean_term, "^", fixed = TRUE)[[1]]

          b64Name <- parts[1]
          power <- parts[2]

          origName <- b64Map[[b64Name]]
          superscript <- chartr(
            "0123456789",
            "\u2070\u00B9\u00B2\u00B3\u2074\u2075\u2076\u2077\u2078\u2079",
            power
          )

          # Safely inject the superscript into the existing labels independently
          replacement <- paste0(origName, superscript)
          newLabel <- gsub(origName, replacement, newLabel, fixed = TRUE)
          newVarLabel <- gsub(origName, replacement, newVarLabel, fixed = TRUE)
        }

        body$label[i] <- newLabel
        body$var_label[i] <- newVarLabel
      }
    }
    body
  })
}
