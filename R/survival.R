# parseCommaNumeric ---------------------------------------------------------

#' Parse a comma-separated string of numbers
#'
#' Splits a string by commas, trims whitespace, and converts to numeric.
#' Non-numeric tokens (typos) are silently dropped.
#'
#' @param text Character string, e.g. "12, 24, 60"
#' @return Numeric vector of valid, non-negative time points
parseCommaNumeric <- function(text) {
  tokens <- trimws(strsplit(text, ",")[[1]])
  values <- suppressWarnings(as.numeric(tokens))
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    jmvcore::reject(
      "Please enter one or more comma-separated numeric time points (e.g. 12 or 12, 24, 60)" # nolint
    )
  }

  if (any(values < 0)) {
    jmvcore::reject(
      "Time points must be non-negative numbers"
    )
  }

  values
}


# buildSurvfitList ----------------------------------------------------------

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


# buildSurvfitHeader --------------------------------------------------------

#' Build descriptive label and spanning headers for tbl_survfit
#'
#' Returns a list with `label_header` (per-column glue template) and optionally
#' `spanning_header` for multi-time-point tables. The CI portion uses the same
#' dynamic formatting as the regression analysis.
#'
#' @param statistic `"times"` or `"median"`
#' @param type `"survival"`, `"risk"`, or `"cumhaz"`
#' @param confLevel Numeric confidence level as percentage (e.g. 95)
#' @param timeSuffix Character label appended to time values (e.g. "-Month",
#'   "-Year")
#' @param nTimes Number of time points (used only when statistic == "times")
#' @return A list with `label_header` and `spanning_header` (NULL if not needed)
buildSurvfitHeader <- function(
  statistic,
  type,
  confLevel,
  timeSuffix,
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

  timeGlue <- paste0("{time}", timeSuffix)

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
      label_header = paste0("**", timeGlue, " ", typePart, ciPart, "**"),
      spanning_header = NULL
    )
  }
}


# pipeAddPSurvfit -----------------------------------------------------------

#' Add p-values to a tbl_survfit table
#'
#' Pipeline step. Calls gtsummary::add_p() on a tbl_survfit using the
#' test names from add_p.tbl_survfit: "logrank", "tarone", "survdiff",
#' "petopeto_gehanwilcoxon", "coxph_lrt", "coxph_wald", "coxph_score".
#'
#' Option names match gtsummary test names directly — no alias mapping needed.
#'
#' @param table A gtsummary tbl_survfit table
#' @param strata Character vector of original strata variable names
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with p-values added (or unchanged)
pipeAddPSurvfit <- function(table, strata, options, collector) {
  if (!options$addPvalue || length(strata) == 0) {
    return(table)
  }

  addPArgs <- list(x = table)

  # Assign default test to every stratum, then override specific ones.
  # Unlike tbl_summary/tbl_continuous, assign_tests.tbl_survfit has no
  # internal defaults — a named list must cover ALL strata or it crashes
  # (fill_formula_selectors breaks with partial coverage).
  testArguments <- stats::setNames(
    rep(list(options$testDefault), length(strata)),
    strata
  )

  # Per-variable overrides from UI; make.names() converts original labels
  # (e.g. "Tumor Response") to R-safe keys matching strataClean.
  for (item in options$testSpecific) {
    if (item$test != "useDefault") {
      testArguments[[make.names(item$var, unique = TRUE)]] <- item$test
    }
  }

  addPArgs$test <- testArguments

  # P-value digits
  pvDigits <- options$digitsPvalue
  if (pvDigits != "auto") {
    addPArgs$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(pvDigits)
    )
  }

  runSafe(do.call(gtsummary::add_p, addPArgs), collector)
}
