# safeExecution.R - Standalone safe execution utilities for tblsummary
# Extracted from tblsummary.b.R

#' Create a new message/warning collector environment
#'
#' @return An environment with `warnings` and `messages` character vectors
#' @export
newCollector <- function() {
  collector <- new.env(parent = emptyenv())
  collector$warnings <- character()
  collector$messages <- character()
  collector
}

#' Add a warning message to a warning list
#'
#' Filters out known noise patterns (e.g., Rtools warnings) before adding.
#'
#' @param msg The warning message to add
#' @param warningList The character vector to append to (modified in place if environment)
#' @param filters Character vector of patterns to filter out (default: Rtools path)
#' @return The updated warning list (invisibly)
#' @export
addWarning <- function(
  msg,
  warningList,
  filters = c("C:/Rtools/home/builder")
) {
  shouldAdd <- TRUE
  for (filter in filters) {
    if (grepl(filter, msg, fixed = TRUE)) {
      shouldAdd <- FALSE
      break
    }
  }
  if (shouldAdd) {
    if (is.environment(warningList)) {
      warningList$warnings <- c(warningList$warnings, msg)
    } else {
      warningList <- c(warningList, msg)
    }
  }
  invisible(warningList)
}

#' Add a message to a message list
#'
#' @param msg The message to add
#' @param messageList The character vector to append to (modified in place if environment)
#' @return The updated message list (invisibly)
#' @export
addMessage <- function(msg, messageList) {
  if (is.environment(messageList)) {
    messageList$messages <- c(messageList$messages, msg)
  } else {
    messageList <- c(messageList, msg)
  }
  invisible(messageList)
}

#' Execute an expression with safe warning and message capture
#'
#' Wraps the expression with calling handlers that capture warnings and messages
#' into the provided collector environment.
#'
#' @param expr The expression to evaluate
#' @param collector An environment created by newCollector() to store warnings/messages
#' @return The result of evaluating expr
#' @export
runSafe <- function(expr, collector) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      addWarning(w$message, collector)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      addMessage(m$message, collector)
      invokeRestart("muffleMessage")
    }
  )
}
