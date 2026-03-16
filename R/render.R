#' Render gtsummary table to HTML for jamovi results
#'
#' Converts a gtsummary table to raw HTML using gt and sets it on a jamovi Html
#' results element.
#'
#' @param table A gtsummary object (e.g., from tbl_summary(), tbl_cross(), etc.)
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
#' @return Invisibly returns the HTML content string
renderHtml <- function(table, resultsHtml) {
  htmlContent <- table |>
    gtsummary::as_gt() |>
    gt::tab_options(table.background.color = "transparent") |>
    gt::as_raw_html()

  # Jamovi hardcodes .jmv-results-html { width: 500px }.
  # Override to max-content: this sizes to the table's intrinsic width and,
  # critically, does NOT change when the iframe viewport is resized by the
  # parent. This breaks the feedback loop where:
  #   content renders → ERDM fires → parent resizes iframe → viewport
  #   changes → content re-layouts → ERDM fires again (= stretching).
  # max-content is viewport-independent, so only ONE resize event fires.
  resultsHtml$setContent(
    paste0(
      "<style>
        .jmv-results-html:has(.summaryTable) { width: max-content !important; }
      </style>
      <div class='summaryTable'>",
      htmlContent,
      "</div>"
    )
  )
}
