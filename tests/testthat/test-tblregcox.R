describe("Multivariable Cox Regression", {
  it("displays hazard ratios in the selected term order", {
    result <- tblRegCox(
      data = gtsummary::trial,
      elapsed = "ttdeath",
      event = "death",
      eventLevel = NULL,
      covs = "age",
      factors = "grade",
      modelTerms = list("grade", "age")
    )

    expectSnapshotHtml(result, "hazard-ratios.html")
  })

  it("preserves overlapping labels with polynomial terms", {
    data <- gtsummary::trial
    data$`Follow-up (months)` <- data$ttdeath
    data$`Event status` <- factor(ifelse(data$death == 1, "Died", "Alive"))
    data$Age <- data$age
    data$`Age Group` <- factor(
      data$grade,
      labels = c("Age I", "Age II", "Age III")
    )
    result <- tblRegCox(
      data = data,
      elapsed = "Follow-up (months)",
      event = "Event status",
      eventLevel = "Died",
      covs = "Age",
      factors = "Age Group",
      modelTerms = list("Age Group", c("Age", "Age"), "Age"),
      addN = TRUE,
      addNEvent = TRUE,
      digitsCoef = "3"
    )

    expectSnapshotHtml(result, "polynomial-labels.html")
  })

  it("warns about standard errors displayed alongside hazard ratios", {
    result <- tblRegCox(
      data = gtsummary::trial,
      elapsed = "ttdeath",
      event = "death",
      eventLevel = NULL,
      covs = "age",
      modelTerms = list("age"),
      addStars = TRUE,
      starsShowSe = TRUE
    )
    expect_snapshot(cat(result[["runWarn"]]$content))
  })
})
