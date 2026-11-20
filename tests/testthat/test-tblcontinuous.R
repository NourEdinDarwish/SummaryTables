describe("Continuous Table", {
  it("summarizes a continuous variable across factors", {
    result <- tblContinuous(
      data = gtsummary::trial,
      contVar = "age",
      varsCat = c("grade", "trt")
    )

    expectSnapshotHtml(result, "ungrouped.html")
  })

  it("compares groups with overall statistics and two-way ANOVA", {
    result <- tblContinuous(
      data = gtsummary::trial,
      contVar = "age",
      varsCat = "grade",
      groupVar = "trt",
      addOverall = TRUE,
      addPvalue = TRUE,
      addQ = TRUE,
      digitsCont = "1"
    )

    expectSnapshotHtml(result, "grouped.html")
  })

  it("applies per-factor statistics, sorting, and parametric p-values", {
    result <- tblContinuous(
      data = gtsummary::trial,
      contVar = "marker",
      varsCat = c("grade", "trt"),
      statDefault = "medianIqr",
      statSpecific = list(list(var = "trt", stat = "meanSd")),
      sortCatDefault = "frequency",
      sortCatSpecific = list(list(var = "trt", sort = "default")),
      digitsCont = "2",
      addPvalue = TRUE,
      journal = "nejm"
    )

    expectSnapshotHtml(result, "custom-statistics.html")
  })

  it("isolates Welch and rank-test overrides from an equal-variance default", {
    data <- data.frame(
      value = c(1, 4, 5, 6, 2, 4, 6, 8, 10, 12, 14, 16),
      student = rep(c("A", "B"), c(4, 8)),
      welch = rep(c("A", "B"), c(4, 8)),
      rank = rep(c("A", "B", "C"), each = 4)
    )
    result <- tblContinuous(
      data = data,
      contVar = "value",
      varsCat = c("student", "welch", "rank"),
      addPvalue = TRUE,
      testDefault = "t.test.equalVar",
      testSpecific = list(
        list(var = "welch", test = "t.test"),
        list(var = "rank", test = "kruskal.test")
      ),
      digitsPvalue = "3"
    )

    expectSnapshotHtml(result, "pvalue-overrides.html")
  })
})
