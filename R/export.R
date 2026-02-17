# export.R - Standalone DOCX export utilities for tblsummary
# Extracted from tblsummary.b.R lines 210-229

#' Resolve Export Path
#'
#' Handles smart default paths for exporting files.
#' - If `path` is a filename only, defaults to User's Documents folder.
#' - Expands `~` to the correct home directory on all platforms.
#' - Normalizes paths to use forward slashes for consistency.
#'
#' @param path String. The user-provided path or filename.
#' @return String. The absolute, normalized path.
resolveExportPath <- function(path) {
  # Expand ~ manually if possible on Windows to avoid Rtools/builder path issues
  if (grepl("~", path, fixed = TRUE)) {
    home <- Sys.getenv("USERPROFILE")
    if (home == "") {
      home <- Sys.getenv("HOME")
    }
    if (home == "") {
      home <- path.expand("~")
    }

    path <- gsub("~", home, path, fixed = TRUE)
  }

  # Standard expansion as fallback/cleanup
  path <- path.expand(path)

  # Normalize to absolute path with consistent slashes
  return(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Export a gtsummary table to DOCX format
#'
#' Converts a gtsummary table to a flextable and saves it as a DOCX file.
#' Also inserts a success Notice into the results.
#'
#' @param table A gtsummary object to export
#' @param path A resolved file path string where the DOCX will be saved
#' @param options A jamovi options object for Notice creation
#' @param results A jamovi results group to insert the success Notice into
#' @param collector An environment for warning capture (from safeExecution.R)
#'
#' @return Invisibly returns NULL. Called for side effects (file creation and notice insertion).
#'
#' @export
exportDocx <- function(table, path, options, results, collector) {
  runSafe(
    {
      flexTableObject <- gtsummary::as_flex_table(table)
      flextable::save_as_docx(flexTableObject, path = path)

      # Create a specific Notice for the save message so it stands out
      saveNotice <- jmvcore::Notice$new(
        options = options,
        name = "exportSuccess",
        type = jmvcore::NoticeType$INFO
      )
      saveNotice$setContent(paste0("Exported to: ", path))
      results$insert(1, saveNotice)
    },
    collector
  )

  invisible(NULL)
}
