# rendering.R - Standalone HTML rendering utilities for gtsummary tables
# Extracted from tblsummary.b.R

#' Render gtsummary table to HTML for jamovi results
#'
#' Converts a gtsummary table to raw HTML using gt and sets it on a jamovi
#' Html results element. Warning and message capture is handled via the
#' collector environment.
#'
#' @param table A gtsummary object (e.g., from tbl_summary(), tbl_cross(), etc.)
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
#' @param collector An environment from `newCollector()` for warning/message capture
#'
#' @return Invisibly returns the HTML content string
#' @export
renderHtml <- function(table, resultsHtml, collector) {
  htmlContent <- runSafe(
    {
      table |>
        gtsummary::as_gt() |>
        gt::as_raw_html()
    },
    collector
  )

  resultsHtml$setContent(htmlContent)
  invisible(htmlContent)
}
