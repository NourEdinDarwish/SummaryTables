describe("Univariable Linear Regression", {
  it("displays coefficients from separate models in the selected order", {
    result <- tblUniRegLinear(
      data = gtsummary::trial,
      dep = "marker",
      covs = "age",
      factors = "grade",
      varOrder = c("grade", "age")
    )

    expectSnapshotHtml(result, "coefficients.html")
  })

  it("standardizes each model on its own complete cases", {
    data <- gtsummary::trial
    data$grade[1:5] <- NA
    result <- tblUniRegLinear(
      data = data,
      dep = "marker",
      covs = "age",
      factors = "grade",
      standardize = TRUE,
      addNLocation = "both",
      ciMerge = TRUE,
      digitsCoef = "3",
      digitsPvalue = "3",
      journal = "nejm"
    )

    expectSnapshotHtml(result, "standardized.html")
  })
})
