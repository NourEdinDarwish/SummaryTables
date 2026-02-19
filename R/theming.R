# theming.R - Standalone gtsummary theme management utilities
# Provides functions to reset and apply gtsummary themes

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
#' Theme functions are deterministic config setters — they never produce
#' data-dependent warnings or errors, so they are called directly without
#' runSafe(). Any confirmation messages they emit are harmless in jamovi
#' (they go nowhere since there is no visible console).
#'
#' @param journalOption Character: Journal theme to apply. One of:
#'   "none", "jama", "lancet", "nejm", "qjecon"
#' @param languageOption Character: Language for labels. One of:
#'   "en", "de", "es", "fr", "gu", "hi", "is", "ja", "kr", "mr", "nl", "no", "pt", "se", "zh-cn", "zh-tw"
#' @param compactOption Logical: Apply compact theme if TRUE
#' @return NULL (invisibly)
#' @export
applyTheme <- function(
  journalOption = "none",
  languageOption = "en",
  compactOption = FALSE
) {
  # ALWAYS reset first to prevent state leakage
  resetTheme()

  # Apply journal theme if specified (not "none")
  if (journalOption != "none") {
    gtsummary::theme_gtsummary_journal(journal = journalOption)
  }

  # Apply compact theme if requested
  if (compactOption) {
    gtsummary::theme_gtsummary_compact(set_theme = TRUE)
  }

  # Apply language theme if not English
  if (languageOption != "en") {
    gtsummary::theme_gtsummary_language(language = languageOption)
  }

  invisible(NULL)
}
