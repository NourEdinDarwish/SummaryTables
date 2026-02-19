# safeExecution.R - Safe execution utilities for SummaryTables
# Captures warnings and messages from gtsummary/cards calls.
# - Warnings: direct R warnings (e.g., from stat functions)
# - Messages: cards converts stat warnings to messages via cli_inform()
#   (Source: cards/R/eval_capture_conditions.R L83-89, L104-118)

#' Create a new collector environment
#'
#' @return An environment with `warnings` and `messages` character vectors
#' @export
newCollector <- function() {
  collector <- new.env(parent = emptyenv())
  collector$warnings <- character()
  collector$messages <- character()
  collector
}

#' Execute an expression, capturing warnings and messages
#'
#' Warnings are collected for display as WARNING notices.
#' Messages are collected for display as INFO notices
#' (cards re-emits stat test warnings as messages via cli_inform).
#'
#' Known noise patterns (e.g., Rtools path warnings, theme confirmations)
#' are filtered out.
#'
#' @param expr The expression to evaluate
#' @param collector An environment created by newCollector()
#' @return The result of evaluating expr
#' @export
runSafe <- function(expr, collector) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- w$message
      if (!grepl("C:/Rtools/home/builder", msg, fixed = TRUE)) {
        collector$warnings <- c(collector$warnings, msg)
      }
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- m$message
      collector$messages <- c(collector$messages, trimws(msg))
      invokeRestart("muffleMessage")
    }
  )
}
