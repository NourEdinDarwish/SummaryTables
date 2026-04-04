# pipeAddPCont --------------------------------------------------------------

#' Add p-values to a tbl_continuous table
#'
#' tbl_continuous-specific pipeline step. Handles the two modes:
#' - With grouping variable (`by`): always Two-way ANOVA (no user choice)
#' - Without grouping variable: delegates to tbl_summary-style continuous tests
#'   with per-variable overrides via testSpecific
#'
#' @param table A gtsummary tbl_continuous table
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with p-values added (or unchanged)
pipeAddPCont <- function(table, hasGroupVar, options, collector) {
  if (!options$addPvalue) {
    return(table)
  }

  addPArgs <- list(x = table)
  testArguments <- list()
  testArgsArguments <- list()

  if (!hasGroupVar) {
    # Without by: build test formulas from default + per-variable overrides
    # (With by: gtsummary defaults to anova_2way, no test argument needed)

    if (options$testDefault == "parametric") {
      gtsummary::set_gtsummary_theme(list(
        "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
        "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
      ))
    } else if (options$testDefault != "nonparametric") {
      testArguments <- c(
        testArguments,
        list(~ resolveTestAlias(options$testDefault))
      )
      if (
        options$testDefault %in% c("t.test.equalVar", "oneway.test.equalVar")
      ) {
        testArgsArguments <- c(
          testArgsArguments,
          list(~ list(var.equal = TRUE))
        )
      }
    }

    # Per-variable test overrides
    for (item in options$testSpecific) {
      if (item$test != "useDefault") {
        testArguments[[item$var]] <- resolveTestAlias(item$test)
        if (item$test %in% c("t.test.equalVar", "oneway.test.equalVar")) {
          testArgsArguments[[item$var]] <- list(var.equal = TRUE)
        }
      }
    }
  }

  if (length(testArguments) > 0) {
    addPArgs$test <- testArguments
  }
  if (length(testArgsArguments) > 0) {
    addPArgs$test.args <- testArgsArguments
  }

  # P-value digits
  pvDigits <- options$digitsPvalue
  if (pvDigits != "auto") {
    addPArgs$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(pvDigits)
    )
  }

  runSafe(do.call(gtsummary::add_p, addPArgs), collector)
}


# buildStatArgsCont ---------------------------------------------------------

#' Build statistic arguments for tbl_continuous
#'
#' Constructs the `statistic` argument list for gtsummary::tbl_continuous().
#' Maps jamovi option names to glue format strings, applying per-variable
#' overrides from the include variables.
#'
#' Note: tbl_continuous requires a single statistic string per variable
#' (no continuous2 / multi-line support), so eda is not available here.
#'
#' @param varsCat Character vector of categorical variable names
#' @param options Jamovi options object
#' @param themeStrings Theme strings from getThemeStrings()
#' @return A formula-list suitable for the `statistic` argument
buildStatArgsCont <- function(varsCat, options, themeStrings) {
  statStrings <- list(
    meanSd = "{mean} ({sd})",
    medianIqr = paste0("{median} ({p25}", themeStrings$iqrSep, "{p75})"),
    medianRange = paste0("{median} ({min}", themeStrings$rangeSep, "{max})"),
    mean = "{mean}",
    median = "{median}"
  )

  defaultStat <- statStrings[[options$statDefault]]

  statisticArguments <- list(~defaultStat)

  # Per-variable overrides
  for (item in options$statSpecific) {
    if (item$stat != "useDefault") {
      statisticArguments[[item$var]] <- statStrings[[item$stat]]
    }
  }

  statisticArguments
}


# buildDigitsArgsCont -------------------------------------------------------

#' Build digits arguments for tbl_continuous
#'
#' Simpler than tbl_summary: only continuous digits are relevant since
#' all include variables are categorical (their levels are row labels,
#' not statistics).
#'
#' @param options Jamovi options object
#' @return A list suitable for the `digits` argument (or NULL)
buildDigitsArgsCont <- function(options) {
  if (options$digitsCont == "auto") {
    return(NULL)
  }
  as.numeric(options$digitsCont)
}
