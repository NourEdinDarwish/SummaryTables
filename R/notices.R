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
#' When `b64Map` is supplied, any B64-encoded variable names in the text
#' are decoded back to their original human-readable names before display.
#'
#' @param collector Environment with `$warnings` and `$messages` vectors
#' @param options jamovi options for Notice creation
#' @param results jamovi results group to insert Notices into
#' @param b64Map Optional named character vector from `buildB64Map()`.
#'   When provided, B64 tokens in notice text are replaced with originals.
displayNotices <- function(collector, options, results, b64Map = NULL) {
  # Messages (INFO) — stat test feedback from cards
  if (length(collector$messages) > 0) {
    msgs <- collector$messages
    if (length(b64Map) > 0) {
      msgs <- decodeB64(msgs, b64Map)
    }
    msgContent <- paste(msgs, collapse = "\n\n")

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
    warns <- collector$warnings
    if (length(b64Map) > 0) {
      warns <- decodeB64(warns, b64Map)
    }
    warnContent <- paste(warns, collapse = "\n\n")

    warnNotice <- jmvcore::Notice$new(
      options = options,
      name = "runWarn",
      type = jmvcore::NoticeType$WARNING
    )
    warnNotice$setContent(warnContent)
    results$insert(1, warnNotice)
  }
}
