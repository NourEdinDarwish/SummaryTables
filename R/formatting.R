#' Apply text formatting to a gtsummary table
#'
#' Shared helper for all table types. Applies bold/italic labels and levels,
#' bold p-values (including q-values and difference p-values), and
#' separate-p-footnotes — all controlled by the jamovi options object.
#'
#' Uses optTrue() (from utils.R) for boolean options that may not exist
#' in every analysis, preventing jmvcore's "does not exist" errors.
#'
#' @section Guard strategy:
#' Every p-value formatting guard uses two layers:
#'
#' - `hasPvalue` — runtime truth: TRUE when the table actually has
#'   a p.value column (from add_p, add_difference, or tbl_regression).
#'   Computed by each caller based on its own semantics. Protects against
#'   stale options (jamovi greys out controls but never resets them).
#' - `optTrue(options$<source>)` — source check: distinguishes
#'   which p-value source the formatting targets (e.g.
#'   `addPvalue` for add_p, `addDifference` for add_difference).
#'   Returns FALSE for options that do not exist in this analysis type,
#'   so regression tables (which lack addPvalue/addDifference) safely
#'   skip descriptive-only blocks.
#'
#' @param table A gtsummary table object
#' @param hasPvalue Logical: TRUE when the table has a p.value column
#'   (from add_p(), add_difference(), or tbl_regression())
#' @param options A jamovi options object (self$options)
#' @return The formatted table
applyTextFormatting <- function(table, hasPvalue, options) {
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

  # Bold p-values (from add_p) ----------------------------------------------
  if (optTrue(options$boldPvalue) && optTrue(options$addPvalue) && hasPvalue) {
    table <- gtsummary::bold_p(table, t = options$boldPvalueThreshold)
  }

  # Bold p-values (regression — inherent p-values, no source toggle) --------
  if (optTrue(options$boldPvalue) && hasPvalue &&
        !optTrue(options$addPvalue) && !optTrue(options$addDifference)) {
    table <- gtsummary::bold_p(table, t = options$boldPvalueThreshold)
  }

  # Bold q-values (adjusts any p-value source) ------------------------------
  if (optTrue(options$boldQ) && optTrue(options$addQ) && hasPvalue) {
    table <- gtsummary::bold_p(
      table,
      t = options$boldQThreshold,
      q = TRUE
    )
  }

  # Separate p-value footnotes (from add_p) ---------------------------------
  if (optTrue(options$separatePvalueFootnotes) &&
        optTrue(options$addPvalue) && hasPvalue) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  # Bold p-values (from add_difference) -------------------------------------
  if (
    optTrue(options$boldDiffPvalue) &&
      optTrue(options$addDifference) &&
      hasPvalue
  ) {
    table <- gtsummary::bold_p(table, t = options$boldDiffPvalueThreshold)
  }

  # Separate p-value footnotes (from add_difference) ------------------------
  if (
    optTrue(options$separateDiffPFootnotes) &&
      optTrue(options$addDifference) &&
      hasPvalue
  ) {
    table <- gtsummary::separate_p_footnotes(table)
  }

  table
}
