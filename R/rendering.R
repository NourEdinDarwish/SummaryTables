# rendering.R - Standalone HTML rendering utilities for gtsummary tables
# Extracted from tblsummary.b.R

#' Render gtsummary table to HTML for jamovi results
#'
#' Converts a gtsummary table to raw HTML using gt and sets it on a jamovi
#' Html results element. Rendering is a single deterministic operation (not
#' an iterator), so no runSafe() wrapping is needed.
#'
#' @param table A gtsummary object (e.g., from tbl_summary(), tbl_cross(), etc.)
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
#'
#' @return Invisibly returns the HTML content string
#' @export
renderHtml <- function(table, resultsHtml) {
  htmlContent <- table |>
    gtsummary::as_gt() |>
    gt::as_raw_html()

  resultsHtml$setContent(htmlContent)
  invisible(htmlContent)
}
