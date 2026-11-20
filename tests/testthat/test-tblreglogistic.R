describe("Multivariable Logistic Regression", {
  it("displays odds ratios in the selected term order", {
    result <- tblRegLogistic(
      data = gtsummary::trial,
      dep = "response",
      covs = "age",
      factors = "grade",
      modelTerms = list("grade", "age")
    )

    expectSnapshotHtml(result, "odds-ratios.html")
  })

  it("shows global p-values, q-values, counts, and model fit", {
    data <- gtsummary::trial
    data$`Marker (ng * mL^-1)` <- data$marker
    result <- tblRegLogistic(
      data = data,
      dep = "response",
      covs = "Marker (ng * mL^-1)",
      factors = "grade",
      modelTerms = list("grade", "Marker (ng * mL^-1)"),
      globalP = TRUE,
      globalPKeep = TRUE,
      addQ = TRUE,
      addN = TRUE,
      addNLocation = "both",
      addNEvent = TRUE,
      addNEventLocation = "both",
      addRefRowEstimate = TRUE,
      modelFit = "table",
      ciMerge = TRUE
    )

    expectSnapshotHtml(result, "global-p-counts-fit.html")
  })

  it("shows merged CIs and significance stars for log-odds coefficients", {
    result <- tblRegLogistic(
      data = gtsummary::trial,
      dep = "response",
      covs = "age",
      factors = "grade",
      modelTerms = list("age", "grade"),
      exponentiate = FALSE,
      intercept = TRUE,
      ciMerge = TRUE,
      addStars = TRUE,
      starsShowCi = TRUE,
      starsShowP = TRUE,
      starsShowSe = FALSE,
      digitsCoef = "3"
    )

    expectSnapshotHtml(result, "significance-stars.html")
  })

  it("warns about standard errors displayed alongside odds ratios", {
    qjeResult <- tblRegLogistic(
      data = gtsummary::trial,
      dep = "response",
      covs = "age",
      modelTerms = list("age"),
      journal = "qjecon"
    )
    expect_snapshot(cat(qjeResult[["runWarn"]]$content))
  })

  it("rejects non-binary outcomes", {
    data <- gtsummary::trial
    # Braces should survive error-message interpolation.
    data$`Grade {group}` <- data$grade

    expect_snapshot(
      error = TRUE,
      tblRegLogistic(
        data = data,
        dep = "Grade {group}",
        covs = "age",
        modelTerms = list("age")
      )
    )
  })
})
