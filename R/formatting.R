#' Apply text formatting to a gtsummary table
#'
#' Shared helper for all table types. Applies bold/italic labels and levels,
#' bold p-values (including q-values and difference p-values), and
#' separate-p-footnotes — all controlled by the jamovi options object.
#'
#' @param table A gtsummary table object
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options A jamovi options object (self$options)
#' @return The formatted table
applyTextFormatting <- function(table, hasGroupVar, options) {
  # Labels and levels -------------------------------------------------------
  if (options$boldLabels) {
    table <- gtsummary::bold_labels(table)
  }
  if (options$boldLevels) {
    table <- gtsummary::bold_levels(table)
  }
  if (options$italicizeLabels) {
    table <- gtsummary::italicize_labels(table)
  }
  if (options$italicizeLevels) {
    table <- gtsummary::italicize_levels(table)
  }

  # Bold p-values -----------------------------------------------------------
  if (options$boldPvalue && options$addPvalue && hasGroupVar) {
    table <- gtsummary::bold_p(table, t = options$boldPvalueThreshold)
  }

  if (
    options$boldQ &&
      options$addQ &&
      (options$addPvalue || options$addDifference) &&
      hasGroupVar
  ) {
    table <- gtsummary::bold_p(
      table,
      t = options$boldQThreshold,
      q = TRUE
    )
  }

  # Separate p-value footnotes ----------------------------------------------
  if (options$separatePvalueFootnotes && options$addPvalue && hasGroupVar) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  # Difference formatting ---------------------------------------------------
  if (
    options$boldDiffPvalue &&
      options$addDifference &&
      hasGroupVar
  ) {
    table <- gtsummary::bold_p(table, t = options$boldDiffPvalueThreshold)
  }
  if (
    options$separateDiffPFootnotes &&
      options$addDifference &&
      hasGroupVar
  ) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  table
}
