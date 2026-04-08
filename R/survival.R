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
