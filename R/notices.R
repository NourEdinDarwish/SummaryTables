# Notice Display Utility for SummaryTables
#
# Converts collected messages/warnings into jmvcore::Notice objects
# and inserts them into the results.

#' Display Notices
#'
#' Converts collected messages/warnings into jmvcore::Notice objects
#' and inserts them into results.
#'
#' @param collector Environment with `$messages` and `$warnings` character vectors
#' @param options jamovi options for Notice creation
#' @param results jamovi results group to insert Notices into
#' @export
displayNotices <- function(collector, options, results) {
  # 1. Messages (INFO)
  if (length(collector$messages) > 0) {
    # Combine messages
    finalContent <- paste(collector$messages, collapse = "\n")

    # Create Notice with INFO type
    messageNotice <- jmvcore::Notice$new(
      options = options,
      name = 'runMessage',
      type = jmvcore::NoticeType$INFO
    )
    messageNotice$setContent(finalContent)
    results$insert(1, messageNotice)
  }

  # 2. Warnings (WARNING)
  if (length(collector$warnings) > 0) {
    # Combine warnings
    finalContent <- paste(collector$warnings, collapse = "\n")

    # Create Notice with WARNING type
    warningNotice <- jmvcore::Notice$new(
      options = options,
      name = 'runWarn',
      type = jmvcore::NoticeType$WARNING
    )
    warningNotice$setContent(finalContent)
    results$insert(1, warningNotice)
  }
}
