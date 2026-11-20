describe("Univariable Logistic Regression", {
  it("displays odds ratios from separate models in the selected order", {
    result <- tblUniRegLogistic(
      data = gtsummary::trial,
      dep = "response",
      covs = "age",
      factors = "grade",
      varOrder = c("grade", "age")
    )

    expectSnapshotHtml(result, "odds-ratios.html")
  })

  it("displays log-odds coefficients with global p-values and event counts", {
    result <- tblUniRegLogistic(
      data = gtsummary::trial,
      dep = "response",
      covs = "age",
      factors = "grade",
      exponentiate = FALSE,
      globalP = TRUE,
      addQ = TRUE,
      addNEvent = TRUE,
      addNEventLocation = "both",
      ciMerge = TRUE,
      digitsCoef = "3"
    )

    expectSnapshotHtml(result, "log-odds-options.html")
  })

  it("rejects non-binary outcomes", {
    expect_snapshot(
      error = TRUE,
      tblUniRegLogistic(
        data = gtsummary::trial,
        dep = "grade",
        covs = "age"
      )
    )
  })
})
