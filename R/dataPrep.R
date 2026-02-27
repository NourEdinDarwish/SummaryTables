# sortByFreq ----------------------------------------------------------------

#' Sort categorical factor levels by frequency
#'
#' Reorders the factor levels of categorical variables by descending frequency
#' (most common level first). Mirrors the internal sort logic used by
#' gtsummary's tbl_summary(sort = "frequency"), but applied at the data level
#' before passing to any gtsummary table function.
#'
#' Variables with sort set to "default" (i.e. as defined) keep their
#' current factor order. Labels are preserved through the reordering.
#'
#' @param data The data frame to sort
#' @param varsCat Character vector of categorical variable names to consider
#' @param options Jamovi options object with `sortCatDefault` and
#'   `sortCatSpecific` entries
#' @return The data frame with factor levels reordered where requested
sortByFreq <- function(data, varsCat, options) {
  if (length(varsCat) == 0) {
    return(data)
  }

  # Build per-variable sort spec from options
  sortSpec <- stats::setNames(
    rep(options$sortCatDefault, length(varsCat)),
    varsCat
  )

  for (item in options$sortCatSpecific) {
    if (item$sort != "useDefault" && item$var %in% varsCat) {
      sortSpec[[item$var]] <- item$sort
    }
  }

  # Reorder factor levels by frequency where requested
  for (v in varsCat) {
    if (sortSpec[[v]] == "frequency" && v %in% names(data)) {
      lbl <- attr(data[[v]], "label")
      data[[v]] <- factor(
        data[[v]],
        levels = names(sort(table(data[[v]]), decreasing = TRUE))
      )
      attr(data[[v]], "label") <- lbl
    }
  }

  data
}
