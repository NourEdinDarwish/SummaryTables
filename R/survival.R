# parseCommaNumeric ---------------------------------------------------------

#' Parse a comma-separated string of numbers
#'
#' Splits a string by commas, trims whitespace, and converts to numeric.
#' Non-numeric tokens (typos) are silently dropped.
#'
#' @param text Character string, e.g. "12, 24, 60"
#' @return Numeric vector (may be empty if all tokens are invalid)
parseCommaNumeric <- function(text) {
  tokens <- trimws(strsplit(text, ",")[[1]])
  values <- suppressWarnings(as.numeric(tokens))
  values[!is.na(values)]
}


# buildSurvfitList -----------------------------------------------------------

#' Build a list of survfit objects for tbl_survfit
#'
#' Creates an overall (`~1`) fit followed by one fit per stratifying variable.
#' Uses rlang::inject() to embed the formula and data values directly into each
#' survfit call, which is required for add_n() and add_nevent() to re-extract
#' them later (see ?tbl_survfit_errors).
#'
#' @param data Data frame
#' @param elapsed Character: name of the time variable
#' @param event Character: name of the event variable
#' @param strataClean Character vector of cleaned stratifying variable names
#' @param confInt Numeric: confidence level as a proportion (e.g. 0.95)
#' @return A list of survfit objects (overall first, then one per stratum)
buildSurvfitList <- function(data, elapsed, event, strataClean, confInt) {
  survLHS <- sprintf(
    "survival::Surv(%s, %s)",
    jmvcore::composeTerm(elapsed),
    jmvcore::composeTerm(event)
  )

  # Overall
  overallF <- reformulate("1", response = survLHS)
  fits <- list(
    rlang::inject(survival::survfit(
      !!overallF,
      data = !!data,
      conf.int = confInt
    ))
  )

  # One fit per stratifying variable (each analyzed independently)
  for (s in strataClean) {
    stratumF <- reformulate(s, response = survLHS)
    fits <- c(
      fits,
      list(
        rlang::inject(survival::survfit(
          !!stratumF,
          data = !!data,
          conf.int = confInt
        ))
      )
    )
  }

  fits
}


# buildSurvfitHeader ---------------------------------------------------------

#' Build descriptive label and spanning headers for tbl_survfit
#'
#' Returns a list with `label_header` (per-column glue template) and optionally
#' `spanning_header` for multi-time-point tables. The CI portion uses the same
#' dynamic formatting as the regression analysis.
#'
#' @param statistic `"times"` or `"median"`
#' @param type `"survival"`, `"risk"`, or `"cumhaz"`
#' @param confLevel Numeric confidence level as percentage (e.g. 95)
#' @param timeUnit Character label for the time unit (e.g. "Months", "-Year")
#' @param nTimes Number of time points (used only when statistic == "times")
#' @return A list with `label_header` and `spanning_header` (NULL if not needed)
buildSurvfitHeader <- function(
  statistic,
  type,
  confLevel,
  timeUnit,
  nTimes = 1
) {
  ciPart <- paste0(
    " (",
    gtsummary::style_number(confLevel),
    "% CI)"
  )

  typePart <- switch(
    type,
    "survival" = "Survival",
    "risk" = "Cumulative Incidence",
    "cumhaz" = "Cumulative Hazard"
  )

  # No space before unit if it starts with a hyphen (e.g. "-Year" → "5-Year")
  sep <- if (startsWith(timeUnit, "-")) "" else " "
  timeGlue <- paste0("{time}", sep, timeUnit)

  if (statistic == "median") {
    return(list(
      label_header = paste0("**Median Survival", ciPart, "**"),
      spanning_header = NULL
    ))
  }

  # statistic == "times"
  if (nTimes >= 2) {
    list(
      label_header = paste0("**", timeGlue, "**"),
      spanning_header = paste0("**", typePart, ciPart, "**")
    )
  } else {
    list(
      label_header = paste0("**", typePart, " at ", timeGlue, ciPart, "**"),
      spanning_header = NULL
    )
  }
}
