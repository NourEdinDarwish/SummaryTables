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

  # gt 1.3.0 leaves spaces in spanning-header IDs; gt 1.4.0 uses hyphens.
  lines <- sub(
    'id="level 1; stat_1"',
    'id="level-1;-stat_1"',
    lines,
    fixed = TRUE
  )

  # gtsummary 2.5.0 wraps the significance-star footnote in a span.
  starsFootnote <- "*p&lt;0.05; **p&lt;0.01; ***p&lt;0.001"
  sub(
    paste0('<span class="gt_from_md">', starsFootnote, "</span>"),
    starsFootnote,
    lines,
    fixed = TRUE
  )
}
