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