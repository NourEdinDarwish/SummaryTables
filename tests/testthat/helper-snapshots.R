expectSnapshotHtml <- function(result, name) {
  path <- withr::local_tempfile(lines = result$tbl$content, fileext = ".html")

  testthat::expect_snapshot_file(
    path,
    name = name,
    compare = testthat::compare_file_text,
    transform = normalizeSnapshotHtml
  )
}

normalizeSnapshotHtml <- function(lines) {
  # Replace gt's random ten-letter table ID.
  lines <- sub('<div id="[a-z]{10}"', '<div id="table-id"', lines)

  # gtsummary 2.5.0 wraps the significance-star footnote in a span.
  starsFootnote <- "*p&lt;0.05; **p&lt;0.01; ***p&lt;0.001"
  sub(
    paste0('<span class="gt_from_md">', starsFootnote, "</span>"),
    starsFootnote,
    lines,
    fixed = TRUE
  )
}
