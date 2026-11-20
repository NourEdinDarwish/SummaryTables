describe("Cross Table", {
  it("tabulates counts and column percentages with both margins", {
    result <- tblCross(
      data = gtsummary::trial,
      row = "grade",
      col = "trt"
    )

    expectSnapshotHtml(result, "counts-column-percentages.html")
  })

  it("tabulates counts without margins and adds an automatic p-value", {
    result <- tblCross(
      data = gtsummary::trial,
      row = "response",
      col = "trt",
      statistic = "n",
      margin = "none",
      missingText = "Missing",
      addPvalue = TRUE,
      digitsPvalue = "3"
    )

    expectSnapshotHtml(result, "counts-pvalue.html")
  })

  it("shows row percentages and reports the selected test in a source note", {
    result <- tblCross(
      data = gtsummary::trial,
      row = "response",
      col = "trt",
      statistic = "percent",
      percent = "row",
      margin = "row",
      marginText = "All patients",
      digits = "1",
      addPvalue = TRUE,
      testDefault = "chisq.test.no.correct",
      sourceNote = TRUE
    )

    expectSnapshotHtml(result, "row-percentages-source-note.html")
  })
})
