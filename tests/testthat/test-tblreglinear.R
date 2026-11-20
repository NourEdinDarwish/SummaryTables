describe("Multivariable Linear Regression", {
  it("displays coefficients in the selected term order", {
    result <- tblRegLinear(
      data = gtsummary::trial,
      dep = "marker",
      covs = "age",
      factors = "grade",
      modelTerms = list("grade", "age")
    )

    expectSnapshotHtml(result, "coefficients.html")
  })

  it("standardizes using complete cases for variables in the model", {
    data <- gtsummary::trial
    data$grade[1:5] <- NA
    data$unrelated <- NA_real_
    result <- tblRegLinear(
      data = data,
      dep = "marker",
      covs = "age",
      factors = c("grade", "unrelated"),
      modelTerms = list("age", "grade"),
      standardize = TRUE,
      addN = TRUE,
      addVif = TRUE,
      modelFit = "footnote",
      ciMerge = TRUE,
      digitsCoef = "3",
      digitsPvalue = "3",
      boldPvalue = TRUE
    )

    expectSnapshotHtml(result, "standardized.html")
  })

  it("preserves overlapping labels with polynomial terms", {
    data <- gtsummary::trial
    data$Age <- data$age
    data$`Age Group` <- factor(
      data$grade,
      labels = c("Age I", "Age II", "Age III")
    )
    result <- tblRegLinear(
      data = data,
      dep = "marker",
      covs = "Age",
      factors = "Age Group",
      modelTerms = list(
        "Age Group",
        c("Age", "Age Group"),
        c("Age", "Age"),
        "Age"
      ),
      digitsCoef = "3"
    )

    expectSnapshotHtml(result, "polynomial-labels.html")
  })

  it("rejects polynomials on factors and preserves their names in the error", {
    data <- gtsummary::trial
    # Braces should survive error-message interpolation.
    data$`Grade {group}` <- data$grade

    expect_snapshot(
      error = TRUE,
      tblRegLinear(
        data = data,
        dep = "marker",
        factors = "Grade {group}",
        modelTerms = list(c("Grade {group}", "Grade {group}"))
      )
    )
  })
})
