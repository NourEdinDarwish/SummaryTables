# theming.R - Standalone gtsummary theme management utilities
# Provides functions to reset and apply gtsummary themes with safe execution

#' Reset all active gtsummary themes
#'
#' Clears any previously applied gtsummary themes to prevent state leakage
#' between analyses. Should be called at the start of each table generation
#' and on cleanup via on.exit().
#'
#' @return NULL (invisibly)
#' @export
resetTheme <- function() {
  gtsummary::reset_gtsummary_theme()
  invisible(NULL)
}

#' Apply gtsummary theme(s) based on user options
#'
#' Applies one or more gtsummary themes in the correct order. ALWAYS resets
#' first to prevent state leakage from previous runs. Themes stack on top of
#' each other (e.g., JAMA + compact + Spanish).
#'
#' @param themeOption Character: General theme to apply. One of:
#'   "none", "mean_sd", "eda", "continuous2"
#' @param journalOption Character: Journal theme to apply. One of:
#'   "none", "jama", "lancet", "nejm", "qjecon"
#' @param languageOption Character: Language for labels. One of:
#'   "en", "de", "es", "fr", "gu", "hi", "is", "ja", "kr", "mr", "nl", "no", "pt", "se", "zh-cn", "zh-tw"
#' @param compactOption Logical: Apply compact theme if TRUE
#' @param collector Environment: Collector from newCollector() for warning/message capture
#' @return NULL (invisibly)
#' @export
applyTheme <- function(
  themeOption = "none",
  journalOption = "none",
  languageOption = "en",
  compactOption = FALSE,
  collector
) {
  # ALWAYS reset first to prevent state leakage
  resetTheme()

  # Apply journal theme if specified (not "none")
  if (journalOption != "none") {
    runSafe(
      gtsummary::theme_gtsummary_journal(journal = journalOption),
      collector
    )
  }

  # Apply compact theme if requested
  if (compactOption) {
    runSafe(
      gtsummary::theme_gtsummary_compact(set_theme = TRUE),
      collector
    )
  }

  # Apply general theme if specified (not "none")
  if (themeOption != "none") {
    themeExpr <- switch(
      themeOption,
      "mean_sd" = gtsummary::theme_gtsummary_mean_sd(),
      "eda" = gtsummary::theme_gtsummary_eda(),
      "continuous2" = gtsummary::theme_gtsummary_continuous2(),
      NULL
    )
    if (!is.null(themeExpr)) {
      runSafe(themeExpr, collector)
    }
  }

  # Apply language theme if not English
  if (languageOption != "en") {
    runSafe(
      gtsummary::theme_gtsummary_language(language = languageOption),
      collector
    )
  }

  invisible(NULL)
}
