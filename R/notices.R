# notices.R - Notice Display Utility for SummaryTables
# Converts collected warnings and messages into jmvcore::Notice objects

#' Display Notices
#'
#' Converts collected warnings and messages into jmvcore::Notice objects
#' and inserts them into results.
#'
#' Messages are displayed as INFO notices — they come from cards/gtsummary
#' re-emitting statistical test warnings (e.g., "cannot compute exact
#' p-value with ties") via cli_inform().
#'
#' @param collector Environment with `$warnings` and `$messages` vectors
#' @param options jamovi options for Notice creation
#' @param results jamovi results group to insert Notices into
#' @export
displayNotices <- function(collector, options, results) {
  # Messages (INFO) — stat test feedback from cards
  if (length(collector$messages) > 0) {
    msgContent <- paste(collector$messages, collapse = "\n")

    msgNotice <- jmvcore::Notice$new(
      options = options,
      name = "runMsg",
      type = jmvcore::NoticeType$INFO
    )
    msgNotice$setContent(msgContent)
    results$insert(1, msgNotice)
  }

  # Warnings (WARNING) — direct R warnings
  if (length(collector$warnings) > 0) {
    warnContent <- paste(collector$warnings, collapse = "\n")

    warnNotice <- jmvcore::Notice$new(
      options = options,
      name = "runWarn",
      type = jmvcore::NoticeType$WARNING
    )
    warnNotice$setContent(warnContent)
    results$insert(1, warnNotice)
  }
}
