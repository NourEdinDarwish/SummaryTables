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

#' Save or open a gtsummary table as a Word document
#'
#' Handles the Save as Word File and Open in Word actions using the same
#' flextable conversion. jamovi handles the save dialog, opening, or download.
#'
#' @param table A gtsummary object to save or open
#' @param options The analysis options containing the Word actions
#' @param filename The suggested Word filename, including the .docx extension
exportDocx <- function(table, options, filename) {
  for (name in c("saveDocx", "openDocx")) {
    if (!options[[name]]) {
      next
    }

    option <- options$option(name)
    option$perform(function(action) {
      # officer requires the output path to end in .docx.
      path <- paste0(action$params$fullPath, ".docx")
      flexTableObject <- gtsummary::as_flex_table(table) |>
        fix_spanning_header()
      flextable::save_as_docx(flexTableObject, path = path)

      list(filename = filename, path = path)
    })
  }
}
