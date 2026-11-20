describe("Summary Table", {
  it("shows a placeholder when no variables are selected", {
    placeholder <- tblSummary(data = data.frame())
    expectSnapshotHtml(placeholder, "placeholder.html")
  })

  it("leaves the table empty in manual mode before Run is pressed", {
    deferred <- tblSummary(
      data = gtsummary::trial,
      varsCont = "age",
      manualRun = TRUE
    )
    expect_false(deferred$tbl$isFilled())
  })

  it("summarizes ungrouped variables in the selected order", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = c("age", "marker"),
      varsCat = c("grade", "response"),
      varOrder = c("grade", "age", "response", "marker"),
      boldLabels = TRUE,
      italicizeLevels = TRUE
    )

    expectSnapshotHtml(result, "ungrouped.html")
  })

  it("compares groups with an overall column, counts, p-values, and q-values", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = "age",
      varsCat = c("grade", "response"),
      groupVar = "trt",
      addOverall = TRUE,
      addN = TRUE,
      addNLast = TRUE,
      addPvalue = TRUE,
      boldPvalue = TRUE,
      addQ = TRUE,
      boldQ = TRUE,
      boldLevels = TRUE,
      italicizeLabels = TRUE
    )

    expectSnapshotHtml(result, "grouped.html")
  })

  it("applies per-variable statistics, frequency sorting, and missingness", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = c("age", "marker"),
      varsCat = c("grade", "response"),
      statContDefault = "medianIqr",
      statContSpecific = list(list(var = "marker", stat = "eda")),
      statCatSpecific = list(list(var = "response", stat = "percent")),
      sortCatDefault = "frequency",
      sortCatSpecific = list(list(var = "response", sort = "default")),
      digitsCont = "2",
      digitsCatPct = "1",
      missingText = "Missing",
      missingStat = "percent"
    )

    expectSnapshotHtml(result, "custom-statistics.html")
  })

  it("isolates a Welch override from an equal-variance default", {
    data <- data.frame(
      group = rep(c("A", "B"), c(4, 8)),
      first = c(1, 4, 5, 6, 2, 4, 6, 8, 10, 12, 14, 16),
      second = c(2, 3, 5, 7, 1, 3, 6, 9, 12, 15, 18, 21)
    )
    result <- tblSummary(
      data = data,
      varsCont = c("first", "second"),
      groupVar = "group",
      addPvalue = TRUE,
      testContDefault = "t.test.equalVar",
      testContSpecific = list(list(var = "second", test = "t.test")),
      digitsPvalue = "3",
      separatePvalueFootnotes = TRUE
    )

    expectSnapshotHtml(result, "pvalue-welch-override.html")
  })

  it("isolates a rank-test override from an equal-variance default", {
    data <- gtsummary::trial
    data$`Marker (ng * mL^-1)` <- as.numeric(data$marker)
    result <- tblSummary(
      data = data,
      varsCont = c("age", "Marker (ng * mL^-1)"),
      groupVar = "grade",
      addPvalue = TRUE,
      testContDefault = "oneway.test.equalVar",
      testContSpecific = list(
        list(var = "Marker (ng * mL^-1)", test = "kruskal.test")
      ),
      separatePvalueFootnotes = TRUE
    )

    expectSnapshotHtml(result, "pvalue-rank-override.html")
  })

  it("computes continuous and multi-level categorical differences", {
    data <- data.frame(
      group = rep(c("A", "B"), c(4, 8)),
      first = c(1, 4, 5, 6, 2, 4, 6, 8, 10, 12, 14, 16),
      second = c(2, 3, 5, 7, 1, 3, 6, 9, 12, 15, 18, 21),
      stage = factor(rep(c("I", "II", "III"), 4))
    )
    result <- tblSummary(
      data = data,
      varsCont = c("first", "second"),
      varsCat = "stage",
      groupVar = "group",
      addDifference = TRUE,
      diffContDefault = "t.test.equalVar",
      diffContSpecific = list(list(var = "second", method = "t.test")),
      diffDigitsCont = "2",
      diffDigitsCat = "2",
      diffDigitsPvalue = "3",
      diffCiMerge = TRUE,
      boldDiffPvalue = TRUE,
      separateDiffPFootnotes = TRUE
    )

    expectSnapshotHtml(result, "difference-methods.html")
  })

  it("computes dichotomous differences with per-variable methods", {
    data <- gtsummary::trial
    data$responseCorrected <- data$response
    data$responseSmd <- data$response
    result <- tblSummary(
      data = data,
      varsCat = c("response", "responseCorrected", "responseSmd"),
      groupVar = "trt",
      addDifference = TRUE,
      diffDichotDefault = "prop.test.no.correct",
      diffDichotSpecific = list(
        list(var = "responseCorrected", method = "prop.test"),
        list(var = "responseSmd", method = "smd")
      ),
      diffDigitsDichot = "2",
      diffDigitsPvalue = "3",
      diffCiMerge = TRUE
    )

    expectSnapshotHtml(result, "difference-dichotomous.html")
  })

  it("computes confidence intervals with per-variable methods", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = c("age", "marker"),
      varsCat = c("grade", "response"),
      statContSpecific = list(list(var = "marker", stat = "medianIqr")),
      statCatSpecific = list(list(var = "response", stat = "percent")),
      digitsCont = "2",
      addCi = TRUE,
      ciContSpecific = list(
        list(var = "age", method = "useDefault"),
        list(var = "marker", method = "wilcox.test")
      ),
      ciCatSpecific = list(
        list(var = "grade", method = "useDefault"),
        list(var = "response", method = "exact")
      ),
      ciDigitsCont = "2",
      ciDigitsCat = "1"
    )

    expectSnapshotHtml(result, "ci-methods.html")
  })

  it("uses round CI brackets when statistics have no parentheses", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = "age",
      varsCat = "response",
      statContSpecific = list(list(var = "age", stat = "mean")),
      statCatSpecific = list(list(var = "response", stat = "percent")),
      digitsCatPct = "1",
      addCi = TRUE,
      ciMerge = TRUE,
      ciDigitsCont = "1",
      ciDigitsCat = "1"
    )

    expectSnapshotHtml(result, "ci-round-brackets.html")
  })

  it("includes overall CIs with a compact French theme", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = "age",
      varsCat = "response",
      groupVar = "trt",
      addOverall = TRUE,
      addOverallLast = TRUE,
      addCi = TRUE,
      ciMerge = TRUE,
      ciDigitsCont = "1",
      ciDigitsCat = "1",
      language = "fr",
      compact = TRUE
    )

    expectSnapshotHtml(result, "ci-overall.html")
  })

  it("shows the overall column with JAMA's Median (IQR) label", {
    result <- tblSummary(
      data = gtsummary::trial,
      varsCont = "age",
      varsCat = "grade",
      groupVar = "trt",
      statContDefault = "eda",
      missingStat = "nPercent",
      journal = "jama",
      addOverall = TRUE
    )

    expectSnapshotHtml(result, "theme-jama-eda.html")
  })
})
