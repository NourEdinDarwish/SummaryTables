describe("fixSpanningHeader()", {
  it("leaves single-row headers unchanged", {
    table <- gtsummary::tbl_summary(
      gtsummary::trial,
      include = c(age, grade)
    ) |>
      gtsummary::as_flex_table()

    result <- fixSpanningHeader(table)

    expect_identical(result, table)
  })

  it("merges blank header cells vertically", {
    table <- gtsummary::tbl_cross(
      gtsummary::trial,
      row = grade,
      col = trt
    ) |>
      gtsummary::add_p() |>
      gtsummary::as_flex_table()

    result <- fixSpanningHeader(table)

    # Header columns: 1 = label, 2-3 = Drug A / Drug B (under treatment
    # spanner), 4 = Total, 5 = p-value.
    mergedCols <- c(1, 4, 5)
    spannedCols <- c(2, 3)

    # Unspanned columns (1, 4, 5) merge vertically (2 = height, 0 = covered).
    expect_equal(
      result$header$spans$columns,
      rbind(c(2, 1, 1, 2, 2), c(0, 1, 1, 0, 0))
    )

    # Adjacent blank cells (4, 5) are split horizontally (1, 1) while the
    # treatment spanner (2, 3) stays merged (2, 0).
    expect_equal(
      result$header$spans$rows,
      rbind(c(1, 2, 0, 1, 1), rep(1, 5))
    )

    # Row-2 headers are copied into row 1 so merged cells show the label text.
    expect_identical(
      result$header$dataset[1, mergedCols],
      table$header$dataset[2, mergedCols],
      ignore_attr = "row.names"
    )
    expect_identical(
      result$header$content$data[1, mergedCols],
      table$header$content$data[2, mergedCols]
    )

    # Columns under the spanning header stay unchanged.
    expect_identical(
      result$header$dataset[, spannedCols],
      table$header$dataset[, spannedCols]
    )
    expect_identical(
      result$header$content$data[, spannedCols],
      table$header$content$data[, spannedCols]
    )

    # Table body and footer stay unchanged.
    expect_identical(result$body, table$body)
    expect_identical(result$footer, table$footer)
  })
})

describe("exportDocx()", {
  it("saves a Word document and passes its filename and path to the action", {
    table <- gtsummary::tbl_cross(
      gtsummary::trial,
      row = grade,
      col = trt
    )
    exportDir <- withr::local_tempdir()
    exportPath <- file.path(exportDir, "table")

    # Minimal fake for jamovi's options object so OptionAction$perform()
    # can run without a jamovi host session.
    actionResult <- NULL
    fakeOptions <- list(
      openDocx = TRUE,
      option = function(name) {
        list(
          perform = function(fun) {
            actionResult <<- fun(list(params = list(fullPath = exportPath)))
            invisible(NULL)
          }
        )
      }
    )

    exportDocx(table, fakeOptions, "Cross Table.docx")

    expect_equal(
      actionResult,
      list(
        filename = "Cross Table.docx",
        path = file.path(exportDir, "table.docx")
      )
    )
    expect_true(file.exists(actionResult$path))
  })
})
