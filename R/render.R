# setHtmlResult -------------------------------------------------------------

#' Set HTML on a jamovi Html result element with the summaryTable wrapper
#'
#' Wraps raw HTML in the summaryTable CSS class and overrides jamovi's
#' fixed 500px width so that tables render at their natural intrinsic width.
#'
#' @param htmlContent Character: raw HTML string (from gt::as_raw_html())
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
setHtmlResult <- function(htmlContent, resultsHtml) {
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

# renderHtml ----------------------------------------------------------------

#' Render gtsummary table to HTML for jamovi results
#'
#' Converts a gtsummary table to raw HTML using gt and sets it on a jamovi Html
#' results element.
#'
#' @param table A gtsummary object (e.g., from tbl_summary(), tbl_cross(), etc.)
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
renderHtml <- function(table, resultsHtml) {
  htmlContent <- table |>
    gtsummary::as_gt() |>
    gt::tab_options(table.background.color = "transparent") |>
    gt::tab_style(
      style = gt::css(width = "1px"),
      locations = gt::cells_source_notes()
    ) |>
    gt::as_raw_html()

  setHtmlResult(htmlContent, resultsHtml)
}

# renderPlaceholder ---------------------------------------------------------

#' Render an initialization placeholder before variables are supplied
#'
#' Displays a message inside a minimal gt table so all styling (font, borders,
#' colour) comes directly from gt and stays in sync with real table output.
#'
#' @param message Character: instruction text (e.g. what variables to add)
#' @param resultsHtml A jmvcore::Html object with `$setContent()` method
renderPlaceholder <- function(message, resultsHtml) {
  htmlContent <- data.frame(message = message) |>
    gt::gt() |>
    gt::tab_options(
      table.background.color = "transparent",
      column_labels.hidden = TRUE
    ) |>
    gt::as_raw_html()

  setHtmlResult(htmlContent, resultsHtml)
}
