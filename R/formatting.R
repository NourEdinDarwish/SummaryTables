#' Apply text formatting to a gtsummary table
#'
#' Shared helper for all table types. Applies bold/italic labels and levels,
#' bold p-values (including q-values and difference p-values), and
#' separate-p-footnotes — all controlled by the jamovi options object.
#'
#' Uses optTrue() (from utils.R) for boolean options that may not exist
#' in every analysis, preventing jmvcore's "does not exist" errors.
#'
#' @param table A gtsummary table object
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options A jamovi options object (self$options)
#' @return The formatted table
applyTextFormatting <- function(table, hasGroupVar, options) {
  # Labels and levels -------------------------------------------------------
  if (optTrue(options$boldLabels)) {
    table <- gtsummary::bold_labels(table)
  }
  if (optTrue(options$boldLevels)) {
    table <- gtsummary::bold_levels(table)
  }
  if (optTrue(options$italicizeLabels)) {
    table <- gtsummary::italicize_labels(table)
  }
  if (optTrue(options$italicizeLevels)) {
    table <- gtsummary::italicize_levels(table)
  }

  # Bold p-values -----------------------------------------------------------
  if (optTrue(options$boldPvalue) && optTrue(options$addPvalue) && hasGroupVar) {
    table <- gtsummary::bold_p(table, t = options$boldPvalueThreshold)
  }

  if (
    optTrue(options$boldQ) &&
      optTrue(options$addQ) &&
      (optTrue(options$addPvalue) || optTrue(options$addDifference)) &&
      hasGroupVar
  ) {
    table <- gtsummary::bold_p(
      table,
      t = options$boldQThreshold,
      q = TRUE
    )
  }

  # Separate p-value footnotes ----------------------------------------------
  if (optTrue(options$separatePvalueFootnotes) && optTrue(options$addPvalue) && hasGroupVar) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  # Difference formatting ---------------------------------------------------
  if (
    optTrue(options$boldDiffPvalue) &&
      optTrue(options$addDifference) &&
      hasGroupVar
  ) {
    table <- gtsummary::bold_p(table, t = options$boldDiffPvalueThreshold)
  }
  if (
    optTrue(options$separateDiffPFootnotes) &&
      optTrue(options$addDifference) &&
      hasGroupVar
  ) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  table
}
