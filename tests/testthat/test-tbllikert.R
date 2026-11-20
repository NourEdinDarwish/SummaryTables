describe("Likert Table", {
  it("unifies item levels in the first variable's order", {
    data <- data.frame(
      first = factor(
        c("Agree", "Neutral", "Disagree", "Agree", NA, "Neutral"),
        levels = c("Disagree", "Neutral", "Agree")
      ),
      second = factor(
        c("Agree", "Agree", "Disagree", "Agree", "Disagree", NA),
        levels = c("Disagree", "Agree")
      )
    )
    result <- tblLikert(
      data = data,
      vars = c("first", "second")
    )

    expectSnapshotHtml(result, "unified-levels.html")
  })

  it("reverses levels with per-item statistics and observation counts", {
    data <- data.frame(
      first = factor(
        c("Agree", "Neutral", "Disagree", "Agree", NA, "Neutral"),
        levels = c("Disagree", "Neutral", "Agree")
      ),
      second = factor(
        c("Agree", "Agree", "Disagree", "Agree", "Disagree", NA),
        levels = c("Disagree", "Agree")
      ),
      third = factor(
        c("Neutral", "Agree", "Disagree", "Neutral", "Agree", "Disagree"),
        levels = c("Disagree", "Neutral", "Agree")
      )
    )
    result <- tblLikert(
      data = data,
      vars = c("first", "second", "third"),
      levelOrder = "reversed",
      statistic = "n",
      statSpecific = list(
        list(var = "second", stat = "percent"),
        list(var = "third", stat = "nPercent")
      ),
      digits = "1",
      addN = TRUE,
      addNLast = TRUE,
      boldLabels = TRUE
    )

    expectSnapshotHtml(result, "reversed-custom-statistics.html")
  })
})
