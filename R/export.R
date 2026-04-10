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

#' Fix blank spanning header cells in flextable
#'
#' gtsummary pads unspanned header columns with " ". This function vertically
#' merges those blank cells with the label row below, eliminating the floating
#' border artifact in Word export.
#'
#' Assumes " " is gtsummary padding (not user content), and that blanks extend
#' continuously down to the label row.
#'
#' @param x A `flextable` object, typically produced by
#'   [gtsummary::as_flex_table()].
#' @return The same `flextable` object with blank header cells vertically
#'   merged.
fix_spanning_header <- function(x) {
  nrows <- nrow(x$header$dataset)
  if (nrows <= 1) {
    return(x)
  }

  ncols <- ncol(x$header$dataset)
  handled <- logical(ncols)
  merge_ops <- list()

  for (i in seq_len(nrows - 1)) {
    for (j in seq_len(ncols)) {
      if (x$header$dataset[i, j] == " ") {
        x$header$spans$rows[i, j] <- 1
        if (!handled[j]) {
          merge_ops <- c(merge_ops, list(list(rows = seq(i, nrows), col = j)))
          handled[j] <- TRUE
        }
      }
    }
  }

  for (op in merge_ops) {
    anchor_r <- op$rows[1]
    base_r <- op$rows[length(op$rows)]
    col <- op$col
    x$header$dataset[anchor_r, col] <- x$header$dataset[base_r, col]
    x$header$content$data[anchor_r, col] <- x$header$content$data[base_r, col]
    x <- flextable::merge_at(x, i = op$rows, j = col, part = "header")
  }

  x
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
  flexTableObject <- gtsummary::as_flex_table(table) |>
    fix_spanning_header()
  flextable::save_as_docx(flexTableObject, path = path)

  notice <- jmvcore::Notice$new(
    options = options,
    name = "exportSuccess",
    type = jmvcore::NoticeType$INFO
  )
  notice$setContent(paste0("Saved to: ", path))
  results$insert(1, notice)
}
