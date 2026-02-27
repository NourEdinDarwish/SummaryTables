#' Resolve Export Path
#'
#' Handles smart default paths for exporting files.
#' - If blank, defaults to `~/Desktop/Summary Table.docx`.
#' - Bare filenames (no directory) default to Desktop.
#' - Auto-appends `.docx` if missing.
#' - Expands `~` using `USERPROFILE` on Windows (avoids R's `HOME` = Documents
#'   issue).
#'
#' @param path String. The user-provided path or filename.
#' @return String. The absolute, normalized path.
resolveExportPath <- function(path) {
  path <- trimws(path)

  # Strip surrounding quotes (from Windows "Copy as path")
  path <- gsub("^[\"']|[\"']$", "", path)

  # Blank or directory-only input → safe default
  if (nchar(path) == 0 || path %in% c("~", "~/")) {
    path <- "~/Desktop/Summary Table.docx"
  }

  # Helper: resolve user home directory.
  # USERPROFILE is always set on Windows (true profile path).
  # HOME is always set on Mac/Linux.
  # We avoid path.expand("~") because inside Jamovi's R engine on Windows
  # it resolves to Documents, not the user profile.
  getHome <- function() {
    home <- Sys.getenv("USERPROFILE") # Windows
    if (home == "") {
      home <- Sys.getenv("HOME")
    } # Mac/Linux
    home
  }

  # Expand leading ~.  Cannot use sub() because USERPROFILE
  # contains backslashes that sub() interprets as backreferences.
  if (grepl("^~", path)) {
    path <- paste0(getHome(), substring(path, 2))
  }

  # Bare filename (no directory separator) → put on Desktop
  if (!grepl("[/\\\\]", path)) {
    path <- file.path(getHome(), "Desktop", path)
  }

  # Ensure .docx extension (AFTER path assembly, so ~ and dirs are intact)
  if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".docx")
  }

  normalizePath(path, mustWork = FALSE)
}

#' Export a gtsummary table to DOCX format
#'
#' Converts a gtsummary table to a flextable and saves it as a DOCX file.
#' Inserts a Notice into the results to report the saved path.
#'
#' @param table A gtsummary object to export
#' @param path A resolved file path string where the DOCX will be saved
#' @param options A jamovi options object for Notice creation
#' @param results A jamovi results group to insert the Notice into
exportDocx <- function(table, path, options, results) {
  flexTableObject <- gtsummary::as_flex_table(table)
  flextable::save_as_docx(flexTableObject, path = path)

  notice <- jmvcore::Notice$new(
    options = options,
    name = "exportSuccess",
    type = jmvcore::NoticeType$INFO
  )
  notice$setContent(paste0("Saved to: ", path))
  results$insert(1, notice)
}
