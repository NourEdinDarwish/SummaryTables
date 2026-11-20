describe("Survival Table", {
  it("estimates overall survival at specified time points", {
    result <- tblSurvfit(
      data = gtsummary::trial,
      elapsed = "ttdeath",
      event = "death",
      eventLevel = NULL,
      times = "6, 12, 18"
    )

    expectSnapshotHtml(result, "time-points.html")
  })

  it("estimates stratified cumulative incidence across multiple time points", {
    result <- tblSurvfit(
      data = gtsummary::trial,
      elapsed = "ttdeath",
      event = "death",
      eventLevel = NULL,
      strata = "trt",
      times = "12, 24",
      type = "risk",
      digitsEstimate = "1"
    )

    expectSnapshotHtml(result, "cumulative-incidence.html")
  })

  it("estimates single-time-point cumulative hazard with factor-coded events", {
    data <- gtsummary::trial
    data$`Follow-up (months)` <- data$ttdeath
    data$`Event status` <- factor(
      ifelse(data$death == 1, "Died", "Alive"),
      levels = c("Alive", "Died")
    )
    result <- tblSurvfit(
      data = data,
      elapsed = "Follow-up (months)",
      event = "Event status",
      eventLevel = "Died",
      strata = "trt",
      times = "12",
      timeSuffix = " months",
      type = "cumhaz"
    )

    expectSnapshotHtml(result, "cumulative-hazard.html")
  })

  it("estimates stratified median survival with counts and test overrides", {
    data <- gtsummary::trial
    # 1 = censored, 2 = event coding
    data$death12 <- data$death + 1L
    result <- tblSurvfit(
      data = data,
      elapsed = "ttdeath",
      event = "death12",
      eventLevel = NULL,
      strata = c("trt", "grade"),
      statistic = "median",
      addN = TRUE,
      addNEvent = TRUE,
      addPvalue = TRUE,
      testSpecific = list(
        list(var = "grade", test = "petopeto_gehanwilcoxon")
      ),
      digitsPvalue = "3",
      addQ = TRUE,
      digitsEstimate = "1"
    )

    expectSnapshotHtml(result, "median-by-strata.html")
  })

  it("decodes variable names in info notices", {
    tiedData <- data.frame(
      `Follow-up` = c(1, 1, 2, 2),
      Died = c(1, 1, 0, 0),
      `Study Arm` = factor(c("A", "A", "B", "B")),
      check.names = FALSE
    )
    noticeResult <- tblSurvfit(
      data = tiedData,
      elapsed = "Follow-up",
      event = "Died",
      eventLevel = NULL,
      strata = "Study Arm",
      times = "1",
      addPvalue = TRUE,
      testDefault = "coxph_wald"
    )
    # Upstream cards/survival notice wording can change across package
    # versions; assert only what SummaryTables owns.
    expect_match(noticeResult[["runMsg"]]$content, "occurred:", fixed = TRUE)
    expect_match(noticeResult[["runMsg"]]$content, "Study Arm", fixed = TRUE)
    expect_no_match(
      noticeResult[["runMsg"]]$content,
      jmvcore::toB64("Study Arm"),
      fixed = TRUE
    )
  })

  it("rejects invalid inputs", {
    badEventData <- gtsummary::trial
    badEventData$badEvent <- c(0, 2, rep(1, nrow(badEventData) - 2))

    expect_snapshot(
      error = TRUE,
      tblSurvfit(
        data = badEventData,
        elapsed = "ttdeath",
        event = "badEvent",
        eventLevel = NULL
      )
    )
    expect_snapshot(
      error = TRUE,
      tblSurvfit(
        data = gtsummary::trial,
        elapsed = "ttdeath",
        event = "death",
        eventLevel = NULL,
        times = "invalid"
      )
    )
    expect_snapshot(
      error = TRUE,
      tblSurvfit(
        data = gtsummary::trial,
        elapsed = "ttdeath",
        event = "death",
        eventLevel = NULL,
        times = "-6, 12"
      )
    )
  })
})
