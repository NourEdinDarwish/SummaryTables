#' Create a new collector environment
#'
#' @return An environment with `warnings` and `messages` character vectors
newCollector <- function() {
  collector <- new.env(parent = emptyenv())
  collector$warnings <- character()
  collector$messages <- character()
  collector
}

#' Execute an expression, capturing warnings and messages
#'
#' Warnings are collected for display as WARNING notices. Messages are collected
#' for display as INFO notices (cards re-emits stat test warnings as messages
#' via cli_inform).
#'
#' Known noise patterns (e.g., Rtools path warnings, theme confirmations) are
#' filtered out.
#'
#' @param expr The expression to evaluate
#' @param collector An environment created by newCollector()
#' @return The evaluated result of `expr` (e.g., the generated `gtsummary` table
#'   or statistical object). The `collector` environment is modified in-place
#'   with intercepted strings.
runSafe <- function(expr, collector) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- w$message
      msg <- gsub("were returned during :", "occurred:", msg, fixed = TRUE)
      if (!grepl("C:/Rtools/home/builder", msg, fixed = TRUE)) {
        collector$warnings <- c(collector$warnings, trimws(msg))
      }
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- m$message
      msg <- gsub("were returned during :", "occurred:", msg, fixed = TRUE)
      collector$messages <- c(collector$messages, trimws(msg))
      invokeRestart("muffleMessage")
    }
  )
}

#' Display Notices
#'
#' Converts collected warnings and messages into jmvcore::Notice objects and
#' inserts them into results.
#'
#' Messages are displayed as INFO notices — they come from cards/gtsummary
#' re-emitting statistical test warnings (e.g., "cannot compute exact p-value
#' with ties") via cli_inform().
#'
#' @param collector Environment with `$warnings` and `$messages` vectors
#' @param options jamovi options for Notice creation
#' @param results jamovi results group to insert Notices into
displayNotices <- function(collector, options, results) {
  # Messages (INFO) — stat test feedback from cards
  if (length(collector$messages) > 0) {
    msgContent <- paste(collector$messages, collapse = "\n\n")

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
    warnContent <- paste(collector$warnings, collapse = "\n\n")

    warnNotice <- jmvcore::Notice$new(
      options = options,
      name = "runWarn",
      type = jmvcore::NoticeType$WARNING
    )
    warnNotice$setContent(warnContent)
    results$insert(1, warnNotice)
  }
}
