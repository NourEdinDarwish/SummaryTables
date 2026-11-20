describe("Univariable Cox Regression", {
  it("displays hazard ratios from separate models in the selected order", {
    result <- tblUniRegCox(
      data = gtsummary::trial,
      elapsed = "ttdeath",
      event = "death",
      eventLevel = NULL,
      covs = "age",
      factors = "grade",
      varOrder = c("grade", "age")
    )

    expectSnapshotHtml(result, "hazard-ratios.html")
  })

  it("displays log hazard ratios with factor-coded events and event counts", {
    data <- gtsummary::trial
    data$`Follow-up (months)` <- data$ttdeath
    data$`Event status` <- factor(ifelse(data$death == 1, "Died", "Alive"))
    data$`Patient age` <- data$age
    result <- tblUniRegCox(
      data = data,
      elapsed = "Follow-up (months)",
      event = "Event status",
      eventLevel = "Died",
      covs = "Patient age",
      factors = "grade",
      exponentiate = FALSE,
      addNEvent = TRUE,
      addNEventLocation = "both",
      ciMerge = TRUE,
      digitsCoef = "3"
    )

    expectSnapshotHtml(result, "factor-event.html")
  })
})
