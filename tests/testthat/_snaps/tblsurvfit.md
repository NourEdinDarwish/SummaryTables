# Survival Table / rejects invalid inputs

    Code
      tblSurvfit(data = badEventData, elapsed = "ttdeath", event = "badEvent",
        eventLevel = NULL)
    Condition
      Error:
      ! Invalid values in the Event variable. It must be coded using 0/1 (0 = censored, 1 = event) or 1/2 (1 = censored, 2 = event). Found values: 0, 1, 2

---

    Code
      tblSurvfit(data = gtsummary::trial, elapsed = "ttdeath", event = "death",
      eventLevel = NULL, times = "invalid")
    Condition
      Error:
      ! Please enter one or more comma-separated numeric time points (e.g. 12 or 12, 24, 60)

---

    Code
      tblSurvfit(data = gtsummary::trial, elapsed = "ttdeath", event = "death",
      eventLevel = NULL, times = "-6, 12")
    Condition
      Error:
      ! Time points must be non-negative numbers

