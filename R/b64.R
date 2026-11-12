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


# labelModelFrame -----------------------------------------------------------

#' Set display labels on saved model-frame columns
#'
#' Gives B64-encoded predictor columns their input variable names as labels
#' and polynomial columns labels with superscript powers. gtsummary uses
#' these labels when building the regression table, including interactions.
#'
#' @param model A fitted model with a saved `model$model` frame
#' @param b64Map Named character vector from `buildB64Map()`
#' @return The model with labels on its saved model-frame columns
labelModelFrame <- function(model, b64Map) {
  # encodeB64() gives input columns safe B64 names and stores their original
  # names as labels. Multivariable tables reapply those labels here after
  # fitting, but univariable regression and survival tables still rely on
  # the labels supplied by encodeB64(). Keep its encoding and labeling
  # together: the small overlap here does not justify splitting that helper.
  #
  # model$model is the saved data frame used to fit the model. Restore its
  # ordinary labels because newer survival versions can drop them.
  # It also contains transformed columns such as I(age^2), including those
  # used only in interactions. Give these columns labels such as "Age²"
  # and let gtsummary build the interaction and factor-level labels.
  #
  # Previously, fixPolynomialLabels() replaced names in the finished table.
  # Replacing "Age" with "Age²" also changed "Age Group" to "Age² Group".
  # Labeling model-frame columns avoids changing unrelated names or levels.
  superscriptDigits <- c(
    "\u2070",
    "\u00B9",
    "\u00B2",
    "\u00B3",
    "\u2074",
    "\u2075",
    "\u2076",
    "\u2077",
    "\u2078",
    "\u2079"
  )

  for (columnName in names(model$model)) {
    if (columnName %in% names(b64Map)) {
      label <- b64Map[[columnName]]
    } else if (grepl("^I\\([^\\^]+\\^\\d+\\)$", columnName)) {
      # Extract b64Name^power from I(b64Name^power), then split name and power.
      innerExpression <- substr(columnName, 3, nchar(columnName) - 1)
      parts <- strsplit(innerExpression, "^", fixed = TRUE)[[1]]
      digits <- as.integer(strsplit(parts[2], "", fixed = TRUE)[[1]])
      superscript <- paste0(superscriptDigits[digits + 1L], collapse = "")
      label <- paste0(b64Map[[parts[1]]], superscript)
    } else {
      next
    }
    attr(model$model[[columnName]], "label") <- label
  }

  model
}
